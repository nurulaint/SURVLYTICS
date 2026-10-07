library(shiny)
library(bslib)
library(nortest)
library(ggplot2)

normality_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Normality Test Controls",
      width = 320,
      
      selectInput(ns("select_var"), "Select Numeric Variable:", choices = NULL),
      selectInput(ns("test_method"), "Select Normality Test:", 
                  choices = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Anderson-Darling")),
      
      actionButton(
        ns("run_test"), 
        "Run Test", 
        icon = icon("calculator"), 
        class = "btn-primary w-100 mb-3"
      ),
      
      hr(),
      tags$label(class = "control-label", "Report & Data Actions:"),
      
      # Moved Action Buttons to Left Sidebar
      actionButton(
        ns("save_report"), 
        "Save to Report", 
        icon = icon("bookmark"), 
        class = "btn-success w-100 mb-2"
      ),
      downloadButton(
        ns("download_norm_csv"), 
        "Download Metrics CSV", 
        class = "btn-outline-secondary w-100"
      )
    ),
    
    # Main Output Panel
    card(
      card_header("Normality Analysis Results"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Test Summary & Interpretation"),
          tableOutput(ns("norm_table")),
          uiOutput(ns("norm_interpretation"))
        ),
        card(
          card_header("Distribution Plot"),
          plotOutput(ns("norm_plot"), height = "300px")
        )
      )
    )
  )
}

normality_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Populate numeric variable selection
    observe({
      req(app_state$data)
      numeric_cols <- names(app_state$data)[sapply(app_state$data, is.numeric)]
      updateSelectInput(session, "select_var", choices = numeric_cols)
    })
    
    # Reactive test calculation
    test_results <- eventReactive(input$run_test, {
      req(input$select_var, app_state$data)
      vec <- na.omit(app_state$data[[input$select_var]])
      
      res <- list(variable = input$select_var, test_name = input$test_method)
      
      if (input$test_method == "Shapiro-Wilk") {
        # Shapiro test limit check
        if (length(vec) > 5000) vec <- sample(vec, 5000)
        sw <- shapiro.test(vec)
        res$statistic <- unname(sw$statistic)
        res$p_value <- sw$p.value
      } else if (input$test_method == "Kolmogorov-Smirnov") {
        ks <- ks.test(vec, "pnorm", mean(vec), sd(vec))
        res$statistic <- unname(ks$statistic)
        res$p_value <- ks$p.value
      } else if (input$test_method == "Anderson-Darling") {
        ad <- ad.test(vec)
        res$statistic <- unname(ad$statistic)
        res$p_value <- ad$p.value
      }
      
      res
    })
    
    # Output Test Summary Table
    output$norm_table <- renderTable({
      res <- test_results()
      data.frame(
        Metric = c("Variable", "Test Selected", "Test Statistic", "p-value"),
        Value = c(res$variable, res$test_name, round(res$statistic, 4), round(res$p_value, 4)),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # Output Interpretation Guide
    output$norm_interpretation <- renderUI({
      res <- test_results()
      if (res$p_value > 0.05) {
        div(class = "alert alert-success mt-2",
            tags$b("Interpretation: "), 
            "The p-value is greater than 0.05. The data follows a normal distribution. Parametric statistical tests are recommended.")
      } else {
        div(class = "alert alert-warning mt-2",
            tags$b("Interpretation: "), 
            "The p-value is less than or equal to 0.05. The data significantly deviates from a normal distribution. Non-parametric alternatives or data transformations may be considered.")
      }
    })
    
    # Render QQ Plot / Density Plot
    output$norm_plot <- renderPlot({
      req(input$select_var, app_state$data)
      df <- app_state$data
      var <- input$select_var
      
      ggplot(df, aes(x = .data[[var]])) + 
        geom_histogram(aes(y = ..density..), fill = "#3498db", color = "white", bins = 20, alpha = 0.6) + 
        geom_density(color = "#e74c3c", linewidth = 1) + 
        theme_minimal() + 
        labs(title = paste("Distribution of", var), x = var, y = "Density")
    })
    
    # Save results to shared state for Word export
    observeEvent(input$save_report, {
      req(test_results())
      res <- test_results()
      
      app_state$final_models$normality <- list(
        variable = res$variable,
        test_name = res$test_name,
        statistic = res$statistic,
        p_value = res$p_value
      )
      
      showNotification(
        paste("Saved normality test for", res$variable, "to summary report!"), 
        type = "message"
      )
    })
    
    # CSV Download Handler
    output$download_norm_csv <- downloadHandler(
      filename = function() {
        paste0("Normality_Test_", input$select_var, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        res <- test_results()
        df_out <- data.frame(
          Variable = res$variable,
          Test_Method = res$test_name,
          Statistic = res$statistic,
          p_value = res$p_value,
          Is_Normal = ifelse(res$p_value > 0.05, "Yes", "No")
        )
        write.csv(df_out, file, row.names = FALSE)
      }
    )
  })
}