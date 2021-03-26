# CovidInSnapshot - RShiny server
# Joy-El Talbot
# ALY6070 - WinterB 2021
#
# GOAL: build server-side of RShiny App

library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel) # repel text labels to avoid overlap

# ONE TIME ONLY data source loads
global.data <- read.csv("global_data.csv", stringsAsFactors = FALSE)
global.data$newcase.date <- as.Date(global.data$newcase.date)
country.data <- read.csv("country_data.csv", stringsAsFactors = FALSE)
country.data$newcase.date <- as.Date(country.data$newcase.date)

world_map <- map_data("world")
# note in this data, world_map$region is the country BUT
# it uses USA and UK for those countries
world_map$region[world_map$region == "USA"] <- "United States" # to match covid data
world_map$region[world_map$region == "UK"] <- "United Kingdom" # to match covid data


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$date <- renderText({
        paste("Showing data from ", 
              as.character(input$snapshot.date - 28), 
              " to ", 
              as.character(input$snapshot.date + 28))
    })

    # ******************************************************************************
    # Visual 1b: Global NewCaseRate 
    #  SIMPLE TEXT view following template
    #  Rate of new cases [inc/dec] by [global newcase.rate] vs last 4 week average
    #  [total raw cases] from [snapshot.date - 3] to [snapshot.date + 3]
    #  reported from [num countries] countries
    # ******************************************************************************
    output$globalRate <- renderText({
        paste(round(global.data$newcase.rate[global.data$newcase.date == input$snapshot.date],5))
    })
    output$globalRateHelper <- renderText({
        paste("More new cases compared to last month's weekly average.")
    })
    output$globalCount <- renderText({
        paste(global.data$new.cases[global.data$newcase.date == input$snapshot.date])
    })
    output$globalCountHelper <- renderText({
        countries <- country.data %>%
            filter(newcase.date >= input$snapshot.date - 28,
                   newcase.date <= input$snapshot.date + 28) %>%
            group_by(country) %>%
            dplyr::summarize(count = n()) %>%
            group_by() %>%
            dplyr::summarise(count = n())
        paste("From", input$snapshot.date -3, "to", input$snapshot.date +3, 
              "reported across", countries$count, "countries")
    })
     
    # ******************************************************************************
    # Visual 1: NewCaseRate by Country
    #  MAP view - using location.data for latitudes/longitudes
    #  COLOR (shades of red) to show rate of new cases - darker color for higher number
    #  COLOR light grey for no data
    # ******************************************************************************
    output$countryNewCaseRatePlot <- renderPlot({
        caserate <- country.data %>%
            filter(newcase.date == input$snapshot.date) %>%
            select(country, newcase.rate, new.cases) %>%
            full_join(world_map, by=c("country" = "region"))
        
        
        # creating the plot
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
    }) # end of output$countryNewCaseRatePlot

    # ******************************************************************************
    # Visual 2b: Raw cases +/- 28 days of snapshot globally
    #  LINE GRAPH
    # ******************************************************************************
    output$globalReportedCases <- renderPlot({
        global.filtered <- global.data %>%
            filter(newcase.date >= input$snapshot.date - 28,
                   newcase.date <= input$snapshot.date + 28)
        countries <- country.data %>%
            filter(newcase.date >= input$snapshot.date - 28,
                   newcase.date <= input$snapshot.date + 28) %>%
            group_by(country) %>%
            dplyr::summarize(count = n()) %>%
            group_by() %>%
            dplyr::summarise(count = n())
        
        plot(global.filtered$newcase.date - input$snapshot.date,
         global.filtered$new.cases,
         xlab= paste0("Days since ", input$snapshot.date),
         ylab="Reported Cases",
         main="Global Reported Cases",
         type="l",
         sub=paste0("Includes data from ", countries$count, " countries."))
    }) # end of output$globalReportedCases
    
    # ******************************************************************************
    # Visual 2: Raw cases +/- 28 days of snapshot for top three countries of concern
    #  LINE GRAPH
    #  3 countries with highest newcase.rate on snapshot.date
    #  try to include country label inline vs legend
    # ******************************************************************************
    output$countryTop3ReportedCasesPlot <- renderPlot({
        # plot specific data prep
        country.filtered <- country.data %>%
            filter(newcase.date >= input$snapshot.date - 28,
                   newcase.date <= input$snapshot.date + 28)
        
        # Get our top3 countries into a list for snapshot date
        list.top3 <- country.data$top3[country.data$newcase.date == input$snapshot.date] %>%
            unique() %>%
            as.character() %>%
            strsplit(split=",")
        list.top3 <- as.character(list.top3[[1]])

        df.filtered1 <- country.filtered[country.filtered$country %in% list.top3, ]

        # get x & y positions for text labels
        top3 <- data.frame(country = list.top3,
                           #color = brewer.pal(3, "Dark2"),
                           x = rep(max(df.filtered1$newcase.date - input$snapshot.date) + 1, 3)
        )
        top3$y <- NA
        for (c in top3$country) {
            top3$y[top3$country == c] <- tail(df.filtered1$new.cases[df.filtered1$country == c],1)
        }

        # creating the plot
        (ggplot(df.filtered1)
            + aes(x = newcase.date - input$snapshot.date,
                  y = new.cases,
                  color=country)
            + geom_line(size=1)
            + scale_y_continuous("Reported Cases")
            + scale_x_continuous(paste0("Days Since ", input$snapshot.date))
            + scale_color_brewer(palette = "Dark2")
            + geom_text_repel(data = top3,
                              aes(x, y, label=country),
                              direction="y",
                              nudge_x=1)
            + ggtitle("Reported Cases - 3 Fastest Growing Case Loads")
            + coord_cartesian(xlim=c(min(df.filtered1$newcase.date - input$snapshot.date)-1,
                                     max(df.filtered1$newcase.date - input$snapshot.date)+3))
            + theme_light()
            + theme(legend.position="none",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line(color = "darkgrey"))
        )
    }) # end of output$countryTop3ReportedCasesPlot
    
    # ******************************************************************************
    # Visual 3: newcase.rate vs mortality.rate
    #  SCATTER PLOT
    #  limit to Colombia, China, Germany (only reporters with > 90% of days having outcomes)
    #  +/- 28 days of snapshot.date
    # ******************************************************************************
    output$mortalityPlot <- renderPlot({
        # plot specific data prep
        country.filtered <- country.data %>%
            filter(newcase.date >= input$snapshot.date - 28,
                   newcase.date <= input$snapshot.date + 28)
        df.filtered2 <- country.filtered %>%
            filter(country %in% c("China", "Colombia", "Germany"))
    
    plot(df.filtered2$newcase.rate,
         df.filtered2$mortality.rate,
         ylab = "Mortality Rate",
         xlab = "New Case Rate",
         main = paste0("Daily Rates +/- 1 month of ", input$snapshot.date),
         sub = "Data for China, Colombia, and Germany only")
    }) # end of output$mortalityPlot

})
