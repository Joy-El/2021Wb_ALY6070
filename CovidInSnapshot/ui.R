# CovidInSnapshot - RShiny ui
# Joy-El Talbot
# ALY6070 - WinterB 2021
#
# GOAL: build UI-side of RShiny App

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Covid-19 Rates in Snapshot"),
    

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateInput(inputId = "snapshot.date",
                      label = h3("Focus Date"),
                      min = "2020-02-01",
                      max = "2021-02-01",
                      value = "2020-04-01")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("date"),
            h3(textOutput("globalRate")),
            textOutput("globalRateHelper"),
            h3(textOutput("globalCount")),
            textOutput("globalCountHelper"),
            plotOutput("countryNewCaseRatePlot"),
            plotOutput("globalReportedCases"),
            plotOutput("countryTop3ReportedCasesPlot"),
            plotOutput("mortalityPlot")
        )
    )
))
