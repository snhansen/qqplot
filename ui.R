library(shiny)
library(shinythemes)
library(shinyFeedback)

ui <- fluidPage(
  title = "Quantile-quantile plots for normally distributed data", theme = shinytheme("lumen"),
  titlePanel(
    h1("Quantile-quantile plots for normally distributed data", align = "center")
  ),
  div(style = "margin-top: 30px"),
  fluidRow(
    plotOutput("scatterPlot", height = "800px"),
    shinyFeedback::useShinyFeedback()
  ),
  
  fluidRow(
    column(2,
           h2("Parameters"),
           numericInput("n",
                        label = "Number of observations:",
                        value = 100,
                        step = 10,
                        min = 1,
                        max = 100000,
                        width = "80%"),
           div(style="padding: 26px 0px 0px 0px;", 
              actionButton("generate",
                           label = "Generate plot(s)",
                           width = "80%")
           ),
           offset = 4
    ),
    column(2,
           h2("Preferences"),
           numericInput("n_rows",
                        label = "Number of rows in grid:",
                        value = 2,
                        step = 1,
                        min = 1,
                        width = "80%"),
           numericInput("n_cols",
                        label = "Number of columns in grid:",
                        value = 5,
                        step = 1,
                        min = 1,
                        width = "80%"),
    ),
  )
)