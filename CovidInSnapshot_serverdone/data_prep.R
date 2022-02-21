# CovidInSnapshot - data cleanup/prep
# Joy-El Talbot
# ALY6070 - WinterB 2021
#
# GOAL: Create the Global and Country level datasets for the CovidInSnapshot dashboard

# Requirements
# 1) Country-level Global.health data
#    - rolled up to day and country 
#    - limited to 2020 (figure out exact date range based on data quality)
#    - FIELDS: day, country, new_cases, new_cases_pm3days, new_case_rate,
#      ave_cases_per_week_p3to31days, in_top_3_rates, mortality_rate, mortalities  
#      - may want cases with outcome listed; or flag countries without any outcome data
# 2) Global-level Global.health data (similar to country without country filter)
# 3) (perhaps) Latitude/Longitude values for each country

# Checks
# 1) Do all countries have some outcome data reported? No - sticking to Germany, Colombia, China
# 2) What date range has the best by country coverage? 2020-02-01 to 2021-02-01
#    Actually pretty poor coverage (fewer than 5 countries after 2020-11-11), but
#    opting to get more interesting dates to examine WITH note of how many countries
#    included

library(plyr)
library(dplyr)

setwd("~/Roux/2021Wb_ALY6070/CovidInSnapshot")

raw_df <- read.csv("../data/reduced_globaldothealth_02-28-2021.csv")

# ******************************************************************************
# Data cleanup and quality checks
# ******************************************************************************

# reduce fields to needed set: 
#  events.confirmed.date -> default new_case_date
#  events.onsetSymptoms.date -> preferred new_case_date if available
#  events.outcome.value -> ultimately mortalities (some post-processing)
#  events.outcome.date -> may want to map mortalities back to THOSE dates, not confirmed.date
#  location.country -> country

raw_minimal <- raw_df[,c("events.confirmed.date",
                         "events.onsetSymptoms.date",
                         "events.outcome.date",
                         "events.outcome.value",
                         "location.country")]
rm(raw_df) # to free up RAM

#
## cleanup field assignments

# convert date strings to dates
# expect errors if the format doesn't match "%Y-%m-%d" or "%Y/%m/%d"
raw_minimal$cleaned.events.confirmed.date <- as.Date(raw_minimal$events.confirmed.date)
# convert blanks to NAs for other two date fields first
raw_minimal$events.onsetSymptoms.date[raw_minimal$events.onsetSymptoms.date == ""] <- NA
raw_minimal$cleaned.events.onsetSymptoms.date <- as.Date(raw_minimal$events.onsetSymptoms.date)
raw_minimal$events.outcome.date[raw_minimal$events.outcome.date == ""] <- NA
raw_minimal$cleaned.events.outcome.date <- as.Date(raw_minimal$events.outcome.date)

# remove factoring on country & outcomes
raw_minimal$location.country <- as.character(raw_minimal$location.country)
raw_minimal$events.outcome.value <- as.character(raw_minimal$events.outcome.value)

# step-wise clean up mapping for events.outcome.value
unique(raw_minimal$events.outcome.value)
raw_minimal$cleaned.events.outcome.value <- NA
mortalities <- c("death", "Death", "died", "dead")
other_outcomes <- c("discharge", "recovered", "Recovered", "released from quarantine", "discharged")
raw_minimal$cleaned.events.outcome.value[raw_minimal$events.outcome.value %in% mortalities] <- "Mortality"
raw_minimal$cleaned.events.outcome.value[raw_minimal$events.outcome.value %in% other_outcomes] <- "Other Outcome"

raw_minimal[150000:150010,] # inspect a random bit of the data

#
## New Case Date Assignment

# if onsetSymptoms.date < confirmed.date then use onsetSymptoms.date
raw_minimal$newcase.date <- dplyr::if_else(raw_minimal$cleaned.events.onsetSymptoms.date < 
                                             raw_minimal$cleaned.events.confirmed.date,
                                           true = raw_minimal$cleaned.events.onsetSymptoms.date,
                                           false = raw_minimal$cleaned.events.confirmed.date,
                                           missing = raw_minimal$cleaned.events.confirmed.date)

# Are there cases where outcome.date < newcase.date?
View(raw_minimal[(!is.na(raw_minimal$cleaned.events.outcome.date)) & 
              raw_minimal$cleaned.events.outcome.date < raw_minimal$newcase.date,
            c("location.country", "newcase.date", "cleaned.events.outcome.date", "cleaned.events.outcome.value")])

# YES, 35 rows where this happened 
raw_minimal$outcome.before.newcase <- (!is.na(raw_minimal$cleaned.events.outcome.date)) & 
                   raw_minimal$cleaned.events.outcome.date < raw_minimal$newcase.date
sum(raw_minimal$outcome.before.newcase) # because True counts as 1, false as 0

# update these dates
raw_minimal$newcase.date[raw_minimal$outcome.before.newcase] <- raw_minimal$cleaned.events.outcome.date[raw_minimal$outcome.before.newcase]

# test that we see no new cases of outcome.date < newcase.date
View(raw_minimal[(!is.na(raw_minimal$cleaned.events.outcome.date)) & 
                   raw_minimal$cleaned.events.outcome.date < raw_minimal$newcase.date,
                 c("location.country", "newcase.date", "cleaned.events.outcome.date", "cleaned.events.outcome.value")])
# it is an empty data.frame - success!

#
## Mortality Date Assignment

# use outcome.date if available, otherwise confirmed.date NOT onsetSymptoms.date
raw_minimal$mortality.date <- if_else(is.na(raw_minimal$cleaned.events.outcome.date),
                                     true = raw_minimal$cleaned.events.confirmed.date,
                                     false = raw_minimal$cleaned.events.outcome.date)
raw_minimal$mortality.date[is.na(raw_minimal$cleaned.events.outcome.value) |
                             raw_minimal$cleaned.events.outcome.value != "Mortality"] <- NA

#
## Reduce columns to those cleaned and still needed
raw_cleaned <- raw_minimal[,c("newcase.date",
                              "location.country",
                              "mortality.date",
                              "cleaned.events.outcome.value")]
colnames(raw_cleaned) <- c("newcase.date", "country", "mortality.date", "outcome")

rm(raw_minimal, mortalities, other_outcomes)

# glimpse at the data
dplyr::glimpse(raw_cleaned[!is.na(raw_cleaned$outcome) & raw_cleaned$outcome == "Mortality",])
dplyr::glimpse(raw_cleaned[!is.na(raw_cleaned$outcome) & raw_cleaned$outcome != "Mortality",])


#
## Review outcomes
all_countries <- unique(raw_cleaned$country)
length(all_countries) # 130

countries_with_outcomes <- unique(raw_cleaned$country[!is.na(raw_cleaned$outcome)])
length(countries_with_outcomes) # 17
# DECISION POINT: will keep the mortality data but note that it will be limited

# Can we use all time points available for those 17 countries? 
# Yes IF > 85% of the days include at least one noted outcome for that country

raw_cleaned$has_outcome <- if_else(is.na(raw_cleaned$outcome), 0, 1)

outcomes <- raw_cleaned %>%
  dplyr::filter(country %in% countries_with_outcomes) %>%
  dplyr::group_by(newcase.date, country) %>%
  dplyr::summarise(has_outcome = sum(has_outcome))

outcome.frequency <- outcomes %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(days_with_outcome = sum(has_outcome > 0),
                   total_days = sum(has_outcome >= 0))
outcome.frequency$frequency <- outcome.frequency$days_with_outcome / outcome.frequency$total_days
View(outcome.frequency)
rm(outcomes, outcome.frequency, countries_with_outcomes)
# DECISION POINT: will keep the mortality data but note that it will be limited to only:
#  Colombia ( 100% outcome reporting over 360 days), 
#  China    (  97% outcome reporting over 242 days),
#  Germany  (  90% outcome reporting over 417 days)

#
## Remove outcomes and mortality dates for other countries
raw_cleaned$mortality.date[!(raw_cleaned$country %in% c("Colombia", "China", "Germany"))] <- NA
raw_cleaned <- raw_cleaned[,c("newcase.date", "country", "mortality.date")]


#
## Determine date range for the analysis

# looking for range with at least 20 countries having data
date.coverage <- raw_cleaned %>%
  dplyr::group_by(newcase.date, country) %>%
  dplyr::summarise(has_data = n()) %>%
  dplyr::group_by(newcase.date) %>%
  dplyr::summarise(number.countries = n())

View(date.coverage)

plot(date.coverage$newcase.date, date.coverage$number.countries)
min(date.coverage$newcase.date[date.coverage$number.countries >= 20]) # 2020-02-26
max(date.coverage$newcase.date[date.coverage$number.countries >= 20]) # 2020-05-27

min(date.coverage$newcase.date[date.coverage$number.countries >= 10]) # 2020-01-26
max(date.coverage$newcase.date[date.coverage$number.countries >= 10]) # 2020-06-15

min(date.coverage$newcase.date[date.coverage$number.countries >= 5]) # 2020-01-20
max(date.coverage$newcase.date[date.coverage$number.countries >= 5]) # 2020-11-11

# DECISION: Data consistency is really quite terrible, I want to view winter holidays
# Therefore, I'm going to run dates from 2020-02-01 to 2021-02-01 
# AND highlight how many countries are in the snapshot!

cleaned <- raw_cleaned[raw_cleaned$newcase.date >= as.Date("2020-02-01") &
                         raw_cleaned$newcase.date <= as.Date("2021-02-01"),]
rm(raw_cleaned, date.coverage, all_countries)


# ******************************************************************************
# OUTPUT 1: Country-level summary
# ******************************************************************************
# 1) Country-level Global.health data
#    - rolled up to day and country 
#    - limited to 2020 (figure out exact date range based on data quality)
#    - FIELDS: day, country, new_cases, new_cases_pm3days, new_case_rate,
#      ave_cases_per_week_p3to31days, in_top_3_rates, mortality_rate, mortalities  
#      - may want cases with outcome listed; or flag countries without any outcome data

country.data <- cleaned %>%
  dplyr::group_by(newcase.date, country) %>%
  dplyr::summarise(new.cases = n())

mortalities <- cleaned %>%
  dplyr::filter(!is.na(mortality.date)) %>%
  dplyr::group_by(mortality.date, country) %>%
  dplyr::summarise(mortalities = n())

country.data <- merge(country.data,
                      mortalities,
                      by.x = c("newcase.date", "country"),
                      by.y = c("mortality.date", "country"),
                      all.x = TRUE,
                      all.y = TRUE)
# convert NAs from merge to 0s
country.data$new.cases[is.na(country.data$new.cases)] <- 0
country.data$mortalities[is.na(country.data$mortalities)] <- 0

rm(mortalities)

calculate.windows <- function(data, current.country = "All") {
  if(current.country != "All") {
    data <- data %>% 
      dplyr::filter(country == current.country)
  }
  
  calculate.values <- function(date, data){
    newcases.pm3days <- sum(data$new.cases[data$newcase.date >= date - 3 &
                                             data$newcase.date <= date + 3])

    ave.cases.per.week.m1tom4 <- sum(data$new.cases[data$newcase.date >= date - (7*4 + 3) &
                                                      data$newcase.date >= date - (7*0 + 3)]) / 4
    newcase.rate <- newcases.pm3days / ave.cases.per.week.m1tom4
    
    mortalities.pm3days <- sum(data$mortalities[data$newcase.date >= date - 3 &
                                                  data$newcase.date <= date + 3]) 
    mortality.rate <- mortalities.pm3days / newcases.pm3days
    
    return(c(date, newcase.rate, mortality.rate))
  }
  
  results <- mapply(calculate.values, 
                    date = unique(data$newcase.date), 
                    MoreArgs = list(data = data))
 
  # shifting around data to get into data.frame
  results <- data.frame(t(results))
  colnames(results) <- c("newcase.date", "newcase.rate", "mortality.rate")
  results$newcase.date <- as.Date.numeric(results$newcase.date, origin="1970-01-01")
  results$country <- current.country

  return(results)
}

country.windows <- plyr::ldply(mapply(calculate.windows, 
                        current.country = unique(country.data$country), 
                        MoreArgs = list(data = country.data),
                        SIMPLIFY = FALSE),
                       data.frame)[,c("newcase.date",
                                      "country",
                                      "newcase.rate",
                                      "mortality.rate")]
country.data <- merge(country.data,
                      country.windows,
                      by = c("newcase.date", "country"),
                      all.x = TRUE,
                      all.y = TRUE)

rm(country.windows)
country.data <- country.data[country.data$newcase.date >= as.Date("2020-02-01") &
                               country.data$newcase.date <= as.Date("2021-02-01"),]
View(country.data)

#
## Add ranking for top 3 newcase.rate each day
calculate.rank.top_n <- function(data, date, n=3){
  df <- data[data$newcase.date == date, ] %>%
    dplyr::arrange(newcase.rate)
  top3 <- as.character(tail(df$country, n = 3))
  result <- data.frame(date = date, top3 = paste(top3, collapse=","))
  return(result)
}

top3 <- mapply(calculate.rank.top_n, date = unique(country.data$newcase.date),
                MoreArgs = list(data = country.data),
       SIMPLIFY = FALSE)
top3 <- ldply(top3, data.frame)

country.data <- merge(country.data,
                      top3,
                      by.x = "newcase.date",
                      by.y = "date",
                      all.x = TRUE)
View(country.data)

write.csv(country.data, 
          file="country_data.csv",
          row.names = FALSE)
rm(country.data, top3)

# ******************************************************************************
# OUTPUT 2: Global-level summary
# ******************************************************************************
# 2) Global-level Global.health data
#    - rolled up to day 
#    - limited to 2020 (figure out exact date range based on data quality)
#    - FIELDS: day, country, new_cases, new_cases_pm3days, new_case_rate,
#      ave_cases_per_week_p3to31days, in_top_3_rates, mortality_rate, mortalities  
#      - may want cases with outcome listed; or flag countries without any outcome data

global.data <- cleaned %>%
  dplyr::group_by(newcase.date) %>%
  dplyr::summarise(new.cases = n())

mortalities <- cleaned %>%
  dplyr::filter(!is.na(mortality.date)) %>%
  dplyr::group_by(mortality.date) %>%
  dplyr::summarise(mortalities = n())

global.data <- merge(global.data,
                      mortalities,
                      by.x = c("newcase.date"),
                      by.y = c("mortality.date"),
                      all.x = TRUE,
                      all.y = TRUE)
# convert NAs from merge to 0s
global.data$new.cases[is.na(global.data$new.cases)] <- 0
global.data$mortalities[is.na(global.data$mortalities)] <- 0

rm(mortalities)

global.windows <- plyr::ldply(mapply(calculate.windows, 
                                      current.country = "All", 
                                      MoreArgs = list(data = global.data),
                                      SIMPLIFY = FALSE),
                               data.frame)[,c("newcase.date",
                                              "newcase.rate",
                                              "mortality.rate")]
global.data <- merge(global.data,
                      global.windows,
                      by = c("newcase.date"),
                      all.x = TRUE,
                      all.y = TRUE)

rm(global.windows)
global.data <- global.data[global.data$newcase.date >= as.Date("2020-02-01") &
                             global.data$newcase.date <= as.Date("2021-02-01"),]
View(global.data)

write.csv(global.data,
          file="global_data.csv",
          row.names = FALSE)

rm(global.data, cleaned)

# ******************************************************************************
# OUTPUT 3: Country Latitude/Longitude Mappings
# ******************************************************************************

raw_df <- read.csv("../data/reduced_globaldothealth_02-28-2021.csv")

# country mapping table
#  add on to tie country-level location.geoResolution lat/long to country name
country_map <- raw_df[raw_df$location.geoResolution == "Country", 
                      c("location.country", 
                        "location.geometry.latitude",
                        "location.geometry.longitude")]
country_map <- unique(country_map) # 89 countries with lat/long
# How many countries in complete dataset?
all_countries <- unique(raw_df$location.country) 
length(all_countries) # 130

# Add countries with missing Country-level lat/long to the country_map table
# We will add their Lat/Long manually in Excel
missing_countries <- all_countries[!(all_countries %in% country_map$location.country)]
length(missing_countries) # 41 which CHECKS wiht 130 total - 89 in country map = 41 missing

# convert missing_countries to dataframe matching country_map
missing_countries <- data.frame(location.country = missing_countries)
missing_countries$location.geometry.latitude <- NA
missing_countries$location.geometry.longitude <- NA

country_map <- rbind(country_map, missing_countries)
View(country_map)                     

write.csv(country_map, 
          file = "country_lat_long.csv",
          row.names = FALSE)

rm(country_map, missing_countries, all_countries,raw_df)


