library(shiny)
library(shinythemes)
library(ggplot2)
library(cowplot)
library(shinyFeedback)


server <- function(input, output) {
  data_generated <- reactiveVal(FALSE)
  max_obs <<- 500000
  valid_obs <- reactive({
    input$n*input$n_rows*input$n_cols <= max_obs
  })
  
  valid_cols <- reactive({
    (input$n_cols>=1) & (input$n_cols%%1 == 0)
  })
  
  valid_rows <- reactive({
    (input$n_rows>=1) & (input$n_rows%%1 == 0)
  })
  
  
  observe({
    shinyFeedback::feedbackWarning("n", !valid_obs(), "You have exceeded the maximum number of observations. Try fewer observations or fewer plots.")
    shinyFeedback::feedbackWarning("n_cols", !valid_cols(), "Number of columns must be an integer >= 1.")
    shinyFeedback::feedbackWarning("n_rows", !valid_rows(), "Number of rows must be an integer >= 1.")
  })
  
  dat <- eventReactive(input$generate, {
    if (valid_obs() & valid_rows() & valid_cols()) {
      y <- rnorm(round(input$n)*input$n_rows*input$n_cols, 0, 1)
      data.frame(y)
    }
    else {
      data.frame()
    }
  })
  
  observeEvent(input$generate, {
    data_generated(TRUE)
  })
  
  observeEvent(input$n_cols, {
    data_generated(FALSE)
  })
  
  observeEvent(input$n_rows, {
    data_generated(FALSE)
  })
  
  observeEvent(input$n, {
    data_generated(FALSE)
  })
  
  
  plot <- reactive({
    if (data_generated() & valid_obs() & valid_rows() & valid_cols()) {
      plots <- list()
      for (i in 1:(input$n_rows*input$n_cols)) {
        subdat <- as.data.frame(dat()[((i-1)*input$n+1):(i*input$n),])
        colnames(subdat) <- c("y")
        plots[[i]] <- ggplot(subdat, aes(sample=y)) + theme_minimal() + labs(x="", y="") + 
          coord_cartesian(xlim=c(-2,2), ylim=c(-2, 2)) +
          stat_qq() + stat_qq_line() + theme_cowplot()
      }
      plot_grid_args <- c(plots[seq(1,(input$n_rows*input$n_cols))], list(ncol=input$n_cols))
      do.call(plot_grid, plot_grid_args)
    } 
    else {
      ggplot()
    }
  })
  
  output$scatterPlot <- renderPlot({plot()})
}