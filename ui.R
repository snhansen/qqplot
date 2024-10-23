library(shiny)
library(shinythemes)
library(shinyFeedback)

ui <- fluidPage(
  title = "Quantile-quantile plots for normally distributed data",
  theme = shinytheme("lumen"),
  titlePanel(h1("Quantile-quantile plots for normally distributed data",
                align = "center")),
  div(style = "margin-top: 30px"),
  fluidRow(
    column(8,
      plotOutput("main_plot", height = "600px"),
      shinyFeedback::useShinyFeedback(),
      offset = 2)
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
                           width = "80%")),
           offset = 4),
    column(2,
           h2("Settings"),
           numericInput("n_rows",
                        label = "Number of rows in grid:",
                        value = 2,
                        step = 1,
                        min = 1,
                        width = "80%"),
           numericInput("n_cols",
                        label = "Number of columns in grid:",
                        value = 4,
                        step = 1,
                        min = 1,
                        width = "80%")
    ),
    column(2,
           h2("Misc"),
           radioButtons("line_type",
                        label = "Reference line:",
                        choices = c("Default" = "default",
                                    "Alternative" = "alternative"),
                        inline = TRUE
           ),
           downloadButton("dl_plot",
                          label = "Download plot(s)"
           )
    )
  )
)