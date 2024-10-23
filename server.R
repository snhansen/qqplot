library(shiny)
library(shinythemes)
library(ggplot2)
library(patchwork)
library(shinyFeedback)

server <- function(input, output) {
  data_generated <- reactiveVal(FALSE)
  max_obs <- 500000
  
  valid <- reactive({
    (!is.na(input$n)) & (input$n >= 1) & (input$n %% 1) == 0 & (input$n*input$n_rows*input$n_cols <= max_obs) & (input$n_cols >= 1) & (input$n_cols %% 1) == 0 & (input$n_rows >= 1) & (input$n_rows %% 1) == 0
  })
  
  # Throw warnings if inputs aren't valid. 
  observe({
    feedbackWarning("n_cols", input$n_cols < 1 | (input$n_cols %% 1) != 0, "Number of columns must be an integer >= 1.")
    feedbackWarning("n_rows", input$n_rows < 1 | (input$n_rows %% 1) != 0, "Number of rows must be an integer >= 1.")
    
    # For the number of observations there are two cases that we wish to
    # treat separately. This is done with the following trick.
    hideFeedback("n")
    
    case1 <- (!is.na(input$n)) & input$n*input$n_rows*input$n_cols > max_obs
    case2 <- (is.na(input$n)) | (input$n < 1) | ((input$n %% 1) != 0)
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
  
  plots_list <- eventReactive(input$generate, {
    if (!is.null(dat())) {
      plots <- list()
      for (i in 1:(input$n_rows*input$n_cols)) {
        subdat <- as.data.frame(dat()[((i-1)*input$n+1):(i*input$n),])
        colnames(subdat) <- c("y")
        plots[[i]] <- ggplot(subdat, aes(sample = y)) + 
          labs(x = "", y = "") +
          coord_cartesian(xlim = c(-2,2),
                          ylim = c(-2,2)) +
          stat_qq() +
          theme_minimal()
      }
      list(plots, input$n_cols)
    }
    else {
      NULL
    }
  })
  
  # When pressing generate, we also create the plots.
  plot <- reactive({
    if (!is.null(plots_list())) {
      plots <- plots_list()[[1]]
      n_cols <- plots_list()[[2]]
      if (input$line_type == "default") {
        for (i in 1:length(plots)) {
          plots[[i]] <- plots[[i]] + stat_qq_line()
        }
      }
      else if (input$line_type == "alternative") {
        for (i in 1:length(plots)) {
          plots[[i]] <- plots[[i]] + geom_abline(slope = 1, intercept = 0)
        }
      }
      wrap_plots(plots, ncol = n_cols)
    }
    else {
      NULL
    }
  })
  
  plot_to_save <- reactive({
    res <- isolate(plot())
    for (i in 1:length(res)) {
      res[[i]]$layers[[1]]$aes_params$size <- 0.4
    }
    res
  })
  
  output$main_plot <- renderPlot({plot()})
  
  output$dl_plot <- downloadHandler(
    filename = "qqplots.png",
    content = function(file) {
      ggsave(file, plot_to_save())
    }
  )
}