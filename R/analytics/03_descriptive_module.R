library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

descriptive_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Descriptive Controls",
      width = 320,
      
      selectInput(ns("select_var"), "Select Variable:", choices = NULL),
      selectInput(ns("plot_type"), "Select Plot Type:", 
                  choices = c("Histogram", "Boxplot", "Bar Chart", "Density Plot")),
      
      hr(),
      tags$label(class = "control-label", "Report Actions:"),
      
      # Moved Action Buttons to Left Sidebar
      actionButton(
        ns("save_report"), 
        "Save to Report", 
        icon = icon("bookmark"), 
        class = "btn-primary w-100 mb-2"
      ),
      downloadButton(
        ns("download_metrics"), 
        "Download Metrics CSV", 
        class = "btn-outline-secondary w-100"
      )
    ),
    
    # Main Output Panel
    card(
      card_header("Summary Statistics & Visualizations"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Metric Summary"),
          tableOutput(ns("metric_table"))
        ),
        card(
          card_header("Data Visualization"),
          plotOutput(ns("desc_plot"), height = "300px")
        )
      )
    )
  )
}

descriptive_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Populate variable selection based on uploaded dataset
    observe({
      req(app_state$data)
      updateSelectInput(session, "select_var", choices = names(app_state$data))
    })
    
    # Calculate summary metrics for numeric/categorical columns
    metrics_data <- reactive({
      req(input$select_var, app_state$data)
      vec <- app_state$data[[input$select_var]]
      
      if (is.numeric(vec)) {
        data.frame(
          Metric = c("Mean", "SD", "Median", "Min", "Max", "Count"),
          Value = c(
            round(mean(vec, na.rm = TRUE), 3),
            round(sd(vec, na.rm = TRUE), 3),
            round(median(vec, na.rm = TRUE), 3),
            round(min(vec, na.rm = TRUE), 3),
            round(max(vec, na.rm = TRUE), 3),
            length(na.omit(vec))
          ),
          stringsAsFactors = FALSE
        )
      } else {
        tbl <- table(vec)
        df <- as.data.frame(tbl)
        names(df) <- c("Category", "Count")
        df
      }
    })
    
    # Render table on UI
    output$metric_table <- renderTable({
      metrics_data()
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # Render Plot
    output$desc_plot <- renderPlot({
      req(input$select_var, app_state$data)
      df <- app_state$data
      var <- input$select_var
      
      if (is.numeric(df[[var]])) {
        if (input$plot_type == "Histogram") {
          ggplot(df, aes(x = .data[[var]])) + 
            geom_histogram(fill = "#3498db", color = "white", bins = 20) + 
            theme_minimal()
        } else if (input$plot_type == "Boxplot") {
          ggplot(df, aes(y = .data[[var]])) + 
            geom_boxplot(fill = "#e74c3c") + 
            theme_minimal()
        } else if (input$plot_type == "Density Plot") {
          ggplot(df, aes(x = .data[[var]])) + 
            geom_density(fill = "#2ecc71", alpha = 0.5) + 
            theme_minimal()
        } else {
          ggplot(df, aes(x = .data[[var]])) + 
            geom_histogram(fill = "#3498db", color = "white", bins = 20) + 
            theme_minimal()
        }
      } else {
        ggplot(df, aes(x = factor(.data[[var]]))) + 
          geom_bar(fill = "#9b59b6") + 
          theme_minimal() + 
          labs(x = var)
      }
    })
    
    # Save results into state for Report module
    observeEvent(input$save_report, {
      req(metrics_data(), input$select_var)
      app_state$final_models$descriptive <- list(
        variable = input$select_var,
        summary = metrics_data()
      )
      showNotification(
        paste("Saved", input$select_var, "descriptives to summary report!"), 
        type = "message"
      )
    })
    
    # CSV Download Handler
    output$download_metrics <- downloadHandler(
      filename = function() {
        paste0("Descriptive_Metrics_", input$select_var, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(metrics_data(), file, row.names = FALSE)
      }
    )
  })
}