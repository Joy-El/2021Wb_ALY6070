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
library(sp, rgdal) # for reading in spatial data files for maps
library(RColorBrewer) # for color palettes

setwd("~/Roux/2021Wb_ALY6070/CovidInSnapshot")

global.data <- read.csv("global_data.csv", stringsAsFactors = FALSE)
global.data$newcase.date <- as.Date(global.data$newcase.date)
country.data <- read.csv("country_data.csv", stringsAsFactors = FALSE)
country.data$newcase.date <- as.Date(country.data$newcase.date)
germany.data <- read.csv("germany_data.csv", stringsAsFactors = FALSE)
germany.data$newcase.date <- as.Date(germany.data$newcase.date)
colombia.data <- read.csv("colombia_data.csv", stringsAsFactors = FALSE)
colombia.data$newcase.date <- as.Date(colombia.data$newcase.date)
#location.data <- read.csv("country_lat_long.csv", stringsAsFactors = FALSE)
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
# Visual 1 (alt): NewCaseRate by State for Germany or Colombia
#  MAP view - using location.data for latitudes/longitudes
#  COLOR (shades of red) to show rate of new cases - darker color for higher number
#  COLOR light grey for no data
# ******************************************************************************
# adapting from this blog post: https://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/

germany_map <- rgdal::readOGR("gadm36_DEU_gpkg/gadm36_DEU.gpkg", "gadm36_DEU_1")
colombia_map <- rgdal::readOGR("gadm36_COL_gpkg/gadm36_COL.gpkg", "gadm36_COL_1")

# filter and summarize germany data to single value per region
germany.cases <- germany.data %>%
  dplyr::filter(newcase.date >= snapshot.date - 28, 
                newcase.date <= snapshot.date + 28) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(cases.per10k.2monthsOfsnapshot = sum(cases.per10k))
View(germany.cases)

colombia.cases <- colombia.data %>%
  dplyr::filter(newcase.date >= snapshot.date - 28, 
                newcase.date <= snapshot.date + 28) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(cases.per10k.2monthsOfsnapshot = sum(cases.per10k))
View(colombia.cases)

# because we are going to be plotting in base plot, 
# we need to build our color gradation ourselves

# break the data field into 6 sections with the cut command
breaks <- cut(germany.cases$cases.per10k.2monthsOfsnapshot, 6)
germany.cases <- cbind(germany.cases, breaks)

# tie break levels to colors in our color palette
colorPalette <- brewer.pal(6, "Purples") # get 6 gradations of the Purples set 
  # scroll down at this link to see the options: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html
colorPalette <- cbind(colorPalette, levels(breaks))
colnames(colorPalette) <- c("color", "breaks")
colorPalette

# add color onto the germany data
germany.cases <- merge(germany.cases,
                       colorPalette,
                       by = "breaks",
                       all.x = TRUE,
                       all.y = FALSE)
# remove the factoring
germany.cases$color <- as.character(germany.cases$color)

# tie cases data to the plotting data
View(germany_map@data) # let's us see what the data in the shape file looks like

# add a new column to the data via merge
germany_map@data <- merge(germany_map@data,
                          germany.cases,
                          by.x = "NAME_1",
                          by.y = "region",
                          all.x = TRUE, # keep the map data if we don't have covid data
                          all.y = TRUE) # exlcude covid data if we don't have map regions for it
View(germany_map@data)

# add the NA color
colorNA <- c("#BBBBBB") # a grey color for NAs
germany_map@data$color[is.na(germany_map@data$cases.per10k.2monthsOfsnapshot)] <- colorNA

plot(germany_map, col=germany_map$color)
legend("topright", 
       legend = levels(factor(colorPalette[,2])), # change this to clean up your legend values
       fill = colorPalette[,1],
       title = "New cases per 10K")

###
colombia_map <- rgdal::readOGR("gadm36_COL_gpkg/gadm36_COL.gpkg", "gadm36_COL_1")

# filter and summarize germany data to single value per region
colombia.cases <- colombia.data %>%
  dplyr::filter(newcase.date >= snapshot.date - 28, 
                newcase.date <= snapshot.date + 28) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(cases.per10k.2monthsOfsnapshot = sum(cases.per10k))
View(colombia.cases)

# because we are going to be plotting in base plot, 
# we need to build our color gradation ourselves

# break the data field into 6 sections with the cut command
breaks <- cut(colombia.cases$cases.per10k.2monthsOfsnapshot, 6)
colombia.cases <- cbind(colombia.cases, breaks)

# tie break levels to colors in our color palette
colorPalette <- brewer.pal(6, "Purples") # get 6 gradations of the Purples set 
# scroll down at this link to see the options: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html
colorPalette <- cbind(colorPalette, levels(breaks))
colnames(colorPalette) <- c("color", "breaks")
colorPalette

# add color onto the colombia data
colombia.cases <- merge(colombia.cases,
                       colorPalette,
                       by = "breaks",
                       all.x = TRUE,
                       all.y = FALSE)
# remove the factoring
colombia.cases$color <- as.character(colombia.cases$color)

# tie cases data to the plotting data
View(colombia_map@data) # let's us see what the data in the shape file looks like

# add a helper column for the data merge because the MAP has names in proper case 
# and the covid data has names in upper case
colombia_map@data$region_helper <- toupper(colombia_map@data$NAME_1)
# a few manual updates
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Atlántico"] <- "ATLÁNTICO"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Bolívar"] <- "BOLÍVAR"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Boyacá"] <- "BOYACÁ"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Córdoba"] <- "C�"RDOBA"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Caquetá"] <- "CAQUETÁ"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Chocó"] <- "CHOC�""
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Nariño"] <- "NARI�'O"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "Quindío"] <- "QUINDÍO"
colombia_map@data$region_helper[colombia_map@data$NAME_1 == "San Andrés y Providencia"] <- "ARCHIPI�???LAGO DE SAN ANDR�???S, PROVIDENCIA Y SANTA CATALINA"


# add a new column to the data via merge
colombia_map@data <- merge(colombia_map@data,
                          colombia.cases,
                          by.x = "region_helper",
                          by.y = "region",
                          all.x = TRUE, # keep the map data if we don't have covid data
                          all.y = TRUE) # exlcude covid data if we don't have map regions for it
View(colombia_map@data)

# add the NA color
colorNA <- c("#BBBBBB") # a grey color for NAs
colombia_map@data$color[is.na(colombia_map@data$cases.per10k.2monthsOfsnapshot)] <- colorNA

plot(colombia_map, col=colombia_map$color)
legend("topleft", 
       legend = levels(factor(colorPalette[,2])), # change this to clean up your legend values
       fill = colorPalette[,1],
       title = "New cases per 10K")


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
