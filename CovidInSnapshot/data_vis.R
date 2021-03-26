# CovidInSnapshot - initial data visualizations
# Joy-El Talbot
# ALY6070 - WinterB 2021
#
# GOAL: identify key visuals for RShiny dashboard

# Inputs
# 1) Country-level Global.health data
#    - rolled up to day and country 
#    - limited to 2020 (figure out exact date range based on data quality)
#    - FIELDS: day, country, new_cases, new_cases_pm3days, new_case_rate,
#      ave_cases_per_week_p3to31days, in_top_3_rates, mortality_rate, mortalities  
#      - may want cases with outcome listed; or flag countries without any outcome data
# 2) Global-level Global.health data (similar to country without country filter)
# 3) SnapshotDate <- Time point chosen by user

# Visuals to add
# 1) Map of NewCaseRate by country at SnapshotDate
#    - # new cases +/- 3 days of SnapshotDate (7 days total or 1 week)
#    - vs average # new cases per week across month before SnapshotDate
# 1b) Global weekly rate of new cases (as simple text visual with notes)
#    - "based on X new cases from Y to Z" 
#      - X = raw new cases globally (WILL HAVE MISSING DATA)
#      - Y = SnapshotDate - 3 days
#      - Z = SnapshotDAte + 3 days
#    - "reported from X countries"
#      - X = number of countries contributing to snapshop data
# 2) Line graph of raw cases +/- 28 days of SnapshotDate
#    - LIMIT to top three countries based on NewCaseRate (see 1)
# 2b) Line graph of global raw cases in month prior
# 3) Scatter plot newcaserate vs mortality rate?? +/- 28 days
#      DECISION: will keep the mortality data but note that it will be limited to only:
#        Colombia ( 100% outcome reporting over 360 days), 
#        China    (  97% outcome reporting over 242 days),
#        Germany  (  90% outcome reporting over 417 days)

# DECISION: Data consistency is really quite terrible - very few months have data
# from more than 5 countries. Because I want to view winter holidays in my dashboard,
# I'm going to run dates from 2020-02-01 to 2021-02-01 
# AND highlight how many countries are in the snapshot!

library(ggplot2)
library(dplyr)
library(ggrepel) # repel text labels to avoid overlap

setwd("~/Roux/2021Wb_ALY6070/CovidInSnapshot")

global.data <- read.csv("global_data.csv", stringsAsFactors = FALSE)
global.data$newcase.date <- as.Date(global.data$newcase.date)
country.data <- read.csv("country_data.csv", stringsAsFactors = FALSE)
country.data$newcase.date <- as.Date(country.data$newcase.date)
location.data <- read.csv("country_lat_long.csv", stringsAsFactors = FALSE)
snapshot.date <- as.Date("2020-04-01") # default for now, will be input variable in RShiny

# filter data
global.filtered <- global.data %>%
  filter(newcase.date >= snapshot.date - 28,
         newcase.date <= snapshot.date + 28)

country.filtered <- country.data %>%
  filter(newcase.date >= snapshot.date - 28,
         newcase.date <= snapshot.date + 28)



# ******************************************************************************
# Visual 1: NewCaseRate by Country
#  MAP view - using location.data for latitudes/longitudes
#  COLOR (shades of red) to show rate of new cases - darker color for higher number
#  COLOR light grey for no data
# ******************************************************************************
# adapting from tutorials found here: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
world_map <- map_data("world")

# note in this data, world_map$region is the country BUT
# it uses USA and UK for those countries
world_map$region[world_map$region == "USA"] <- "United States" # to match covid data
world_map$region[world_map$region == "UK"] <- "United Kingdom" # to match covid data

caserate <- country.filtered %>%
  filter(newcase.date == snapshot.date) %>%
  select(country, newcase.rate, new.cases) 

caserate <- full_join(caserate, world_map, by=c("country" = "region"))

(ggplot(caserate)
  + aes(x = long, y = lat, group = group)
  + geom_polygon(aes(fill=newcase.rate), color="darkgrey")
  + scale_fill_gradient("Increase in\nNew Cases", 
                        low="white", 
                        high="red", 
                        na.value = "grey90")
  + theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.15,0.4))
)

# ******************************************************************************
# Visual 1b: Global NewCaseRate 
#  SIMPLE TEXT view following template
#  Rate of new cases [inc/dec] by [global newcase.rate] vs last 4 week average
#  [total raw cases] from [snapshot.date - 3] to [snapshot.date + 3]
#  reported from [num countries] countries
# ******************************************************************************


# ******************************************************************************
# Visual 2: Raw cases +/- 28 days of snapshot for top three countries of concern
#  LINE GRAPH
#  3 countries with highest newcase.rate on snapshot.date
#  try to include country label inline vs legend
# ******************************************************************************

# Get our top3 countries into a list for snapshot date
list.top3 <- country.filtered$top3[country.filtered$newcase.date == snapshot.date] %>%
  unique() %>%
  as.character() %>%
  strsplit(split=",") 
list.top3 <- as.character(list.top3[[1]])

df.filtered1 <- country.filtered[country.filtered$country %in% list.top3, ]

# get x & y positions for text labels
top3 <- data.frame(country = list.top3,
                   #color = brewer.pal(3, "Dark2"),
                   x = rep(max(df.filtered1$newcase.date - snapshot.date) + 1, 3)
                  )
top3$y <- NA
for (c in top3$country) {
  top3$y[top3$country == c] <- tail(df.filtered1$new.cases[df.filtered1$country == c],1) 
}

(ggplot(df.filtered1)
  + aes(x = newcase.date - snapshot.date, 
        y = new.cases, 
        color=country)
  + geom_line(size=1)
  + scale_y_continuous("Reported Cases")
  + scale_x_continuous(paste0("Days Since ", snapshot.date))
  + scale_color_brewer(palette = "Dark2")
  + geom_text_repel(data = top3, 
                    aes(x, y, label=country),
                    direction="y",
                    nudge_x=1)
  + ggtitle("Reported Cases - 3 Fastest Growing Case Loads")
  + coord_cartesian(xlim=c(min(df.filtered1$newcase.date - snapshot.date)-1,
                           max(df.filtered1$newcase.date - snapshot.date)+3))
  + theme_light()
  + theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "darkgrey"))
  
 )

# ******************************************************************************
# Visual 2b: Raw cases +/- 28 days of snapshot globally
#  LINE GRAPH
# ******************************************************************************
plot(snapshot.date - global.filtered$newcase.date,
     global.filtered$new.cases,
     xlab=paste0("Days since ", snapshot.date),
     ylab="Reported Cases",
     main="Global Reported Cases",
     type="l",
     sub=paste0("Includes data from ", length(unique(country.filtered$country)), " countries."))

# ******************************************************************************
# Visual 3: newcase.rate vs mortality.rate
#  SCATTER PLOT
#  limit to Colombia, China, Germany (only reporters with > 90% of days having outcomes)
#  +/- 28 days of snapshot.date
# ******************************************************************************

df.filtered2 <- country.filtered %>%
  filter(country %in% c("China", "Colombia", "Germany"))

plot(df.filtered2$newcase.rate,
     df.filtered2$mortality.rate,
     ylab = "Mortality Rate",
     xlab = "New Case Rate",
     main = paste0("Daily Rates +/- 1 month of ", snapshot.date),
     sub = "Data for China, Colombia, and Germany only")
