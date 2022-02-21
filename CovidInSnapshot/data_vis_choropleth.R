# CovidInSnapshot - choropleth map visualization
# Joy-El Talbot
# ALY6070 - WinterB 2021
#
# GOAL: Create choropleth maps for the "states" of Germany and Colombia

# inputs:
#   Geopackages (*.gpkg) for each country
#   Covid data by "state" aka region

library(dplyr)
library(sp, rgdal) # for reading in spatial data files for maps
library(RColorBrewer) # for color palettes

setwd("~/Roux/2021Wb_ALY6070/CovidInSnapshot")

germany.data <- read.csv("germany_data.csv", stringsAsFactors = FALSE)
germany.data$newcase.date <- as.Date(germany.data$newcase.date)
colombia.data <- read.csv("colombia_data.csv", stringsAsFactors = FALSE)
colombia.data$newcase.date <- as.Date(colombia.data$newcase.date)

snapshot.date <- as.Date("2020-04-01") # default for now, will be input variable in RShiny


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
