# R/analytics/02_cleaning_module.R

cleaning_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Data Wrangling Controls"),
      selectInput(ns("col_to_convert"), "Select Variable:", choices = NULL),
      selectInput(ns("target_type"), "Convert To Type:", choices = c("Numeric", "Factor/Categorical", "Character")),
      actionButton(ns("apply_convert"), "Apply Type Conversion", class = "btn-primary w-100"),
      
      hr(),
      
      h5("Handle Missing Data"),
      selectInput(ns("na_strategy"), "Strategy:", choices = c("Remove Rows with NA", "Impute Numeric with Mean", "Impute Numeric with Median")),
      actionButton(ns("apply_na"), "Clean Missing Data", class = "btn-warning w-100"),
      
      hr(),
      
      # UI ID is "download_csv"
      downloadButton(ns("download_csv"), "Download Cleaned CSV", class = "btn-outline-success w-100")
    ),
    
    card(
      card_header("Cleaned Data Overview"),
      verbatimTextOutput(ns("cleaning_log")),
      DTOutput(ns("cleaned_table"))
    )
  )
}

cleaning_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    
    # Store dynamic text log for the console
    log_text <- reactiveVal("No data wrangling actions performed yet.")
    
    # Update choices when dataset changes
    observe({
      req(shared_state$data)
      updateSelectInput(session, "col_to_convert", choices = names(shared_state$data))
    })
    
    # 1. Type Conversion Logic
    observeEvent(input$apply_convert, {
      req(input$col_to_convert, input$target_type, shared_state$data)
      col <- input$col_to_convert
      
      tryCatch({
        if (input$target_type == "Numeric") {
          shared_state$data[[col]] <- as.numeric(as.character(shared_state$data[[col]]))
        } else if (input$target_type == "Factor/Categorical") {
          shared_state$data[[col]] <- as.factor(shared_state$data[[col]])
        } else if (input$target_type == "Character") {
          shared_state$data[[col]] <- as.character(shared_state$data[[col]])
        }
        
        msg <- paste0("[", Sys.time(), "] Converted column '", col, "' to type: ", input$target_type)
        log_text(msg)
        showNotification(paste("Converted", col, "to", input$target_type), type = "message")
        
        # Save to final models list for summary report
        shared_state$final_models[["Data Wrangling"]] <- data.frame(
          Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          Module    = "Data Wrangling",
          Action    = "Type Conversion",
          Details   = paste("Converted", col, "to", input$target_type),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        showNotification(paste("Error during conversion:", e$message), type = "error")
      })
    })
    
    # 2. Missing Data Treatment Logic
    observeEvent(input$apply_na, {
      req(shared_state$data, input$na_strategy)
      
      before_rows <- nrow(shared_state$data)
      
      if (input$na_strategy == "Remove Rows with NA") {
        shared_state$data <- na.omit(shared_state$data)
        after_rows <- nrow(shared_state$data)
        details_msg <- paste("Removed", before_rows - after_rows, "rows containing NA values.")
      } else if (input$na_strategy == "Impute Numeric with Mean") {
        shared_state$data <- shared_state$data %>%
          mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
        details_msg <- "Imputed missing numeric values using column mean."
      } else if (input$na_strategy == "Impute Numeric with Median") {
        shared_state$data <- shared_state$data %>%
          mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
        details_msg <- "Imputed missing numeric values using column median."
      }
      
      msg <- paste0("[", Sys.time(), "] Missing Data Strategy Applied: ", input$na_strategy, " (", details_msg, ")")
      log_text(msg)
      showNotification("Missing data strategy applied.", type = "message")
      
      # Save to final models list for summary report
      shared_state$final_models[["Data Wrangling"]] <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module    = "Data Wrangling",
        Action    = "Missing Data Handling",
        Details   = paste(input$na_strategy, "-", details_msg),
        stringsAsFactors = FALSE
      )
    })
    
    # Render Console Output Log
    output$cleaning_log <- renderText({ log_text() })
    
    # Render Cleaned Table
    output$cleaned_table <- renderDT({
      req(shared_state$data)
      datatable(shared_state$data, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Download Cleaned CSV (Fixed ID mismatch: changed from dl_clean_data to download_csv)
    output$download_csv <- downloadHandler(
      filename = function() { paste0("cleaned_data_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(shared_state$data, file, row.names = FALSE) }
    )
  })
}