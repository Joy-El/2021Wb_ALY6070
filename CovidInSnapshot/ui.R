# CovidInSnapshot - RShiny ui
# Joy-El Talbot
# ALY6070 - WinterB 2021
#
# GOAL: build UI-side of RShiny App

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(div(HTML("<h1>Covid-19 Rates in Snapshot</h1>
                        <p><h4><em style=\"color:blue;\">Explore</em> how influx of new cases changes over time.</h4></p>"))),
    

    # Sidebar with a slider input for number of bins
    fluidRow(
        column(width=4,
               fluidRow(
                    column(width = 12, 
                           dateInput(inputId = "snapshot.date",
                              label = h3("Pick Your Focus Date"),
                              min = "2020-02-01",
                              max = "2021-02-01",
                              value = "2020-04-01")
                    ),
                    column(width = 6, 
                           align="center", # center align text in this grouping
                           h3(textOutput("globalRate"), 
                              style="padding:20px;"), # add padding above text
                           textOutput("globalRateHelper"),
                    ),
                    column(width = 6,
                           h3(textOutput("globalCount"),
                              style="padding:20px;"), # add padding above text
                           textOutput("globalCountHelper")
                    )
                )
        ),
        column(width = 8, h3("Countries with fastest growing case load this week"),
               plotOutput("countryNewCaseRatePlot")
        )),
    fluidRow(
        
        plotOutput("globalReportedCases"),
        plotOutput("countryTop3ReportedCasesPlot"),
        column(width = 6,
               h1("", style="padding:20px;"), # add some empty space to shift down next element
               wellPanel(h4("For the three countries with regular outcome reporting,
               we can look to see how mortality rate behaves relative to the rate of new cases."))
        ),
        column(width = 6,
               plotOutput("mortalityPlot")
        )

    )
))
