library(shiny)
library(shinythemes)
library(ggplot2)
library(cowplot)
library(shinyFeedback)

server <- function(input, output) {
  data_generated <- reactiveVal(FALSE)
  max_obs <- 500000
  
  valid <- reactive({
    input$n >= 1 & (input$n %% 1) == 0 & input$n*input$n_rows*input$n_cols <= max_obs & input$n_cols >= 1 & 
      (input$n_cols %% 1) == 0 & input$n_rows >= 1 & (input$n_rows %% 1) == 0
  })
  
  # Throw warnings if inputs aren't valid. 
  observe({
    feedbackWarning("n_cols", input$n_cols < 1 | (input$n_cols %% 1) != 0, "Number of columns must be an integer >= 1.")
    feedbackWarning("n_rows", input$n_rows < 1 | (input$n_rows %% 1) != 0, "Number of rows must be an integer >= 1.")
    
    # For the number of observations there are two cases that we wish to
    # treat separately. This is done with the following trick.
    hideFeedback("n")
    case1 <- input$n*input$n_rows*input$n_cols > max_obs
    case2 <- (input$n < 1) | ((input$n %% 1) != 0)
    feedback_text <- ""
    if (case1) {
      feedback_text <- paste0("You have exceeded the maximum number of observations. Try fewer observations or fewer plots.")
    }
    else if (case2) {
      feedback_text <- paste0("Number of observations needs to be an integer >= 1.")
    }
    feedbackWarning("n", case1 | case2, feedback_text)
  })
  
  # Simulate data from a standard normal distribution upon 
  # pressing the generate button if inputs are valid.
  dat <- eventReactive(input$generate, {
    if (valid()) {
      y <- rnorm(input$n*input$n_rows*input$n_cols, 0, 1)
      data.frame(y)
    }
    else {
      NULL
    }
  })
  
  # When pressing generate, we also create the plots.
  plot <- eventReactive(input$generate, {
    if (!is.null(dat())) {
      plots <- list()
      for (i in 1:(input$n_rows*input$n_cols)) {
        subdat <- as.data.frame(dat()[((i-1)*input$n+1):(i*input$n),])
        colnames(subdat) <- c("y")
        plots[[i]] <- ggplot(subdat, 
                             aes(sample = y)) + 
          theme_minimal() +
          labs(x = "", y = "") +
          coord_cartesian(xlim = c(-2,2),
                          ylim = c(-2,2)) +
          stat_qq() +
          stat_qq_line() +
          theme_cowplot()
      }
      plot_grid_args <- c(plots[seq(1,(input$n_rows*input$n_cols))], list(ncol = input$n_cols))
      do.call(plot_grid, plot_grid_args)
    }
    else {
      NULL
    }
  })
  
  output$main_plot <- renderPlot({plot()})
}