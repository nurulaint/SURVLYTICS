# R/analytics/05_linear_regression_module.R

linear_regression_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Linear Regression"),
      selectInput(ns("target_y"), "Dependent Variable Y (Continuous only):", choices = NULL),
      selectizeInput(ns("predictors_x"), "Independent Variable(s) X:", choices = NULL, multiple = TRUE),
      hr(),
      actionButton(ns("run_lm"), "Fit Regression Model(s)", class = "btn-primary w-100"),
      hr(),
      actionButton(ns("save_lm_report"), "Save Models to Report", icon = icon("check"), class = "btn-outline-success w-100")
    ),
    div(
      card(
        card_header("1. Simple Linear Regressions (Individual Predictors)"),
        DTOutput(ns("lm_simple_table"))
      ),
      uiOutput(ns("multi_model_ui"))
    )
  )
}

linear_regression_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Restrict Dependent Variable Y strictly to numeric variables
    observe({
      req(shared_state$data)
      df <- shared_state$data                     
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]              
      all_cols <- names(df)                     
      updateSelectInput(session, "target_y", choices = num_cols)              
      updateSelectizeInput(session, "predictors_x", choices = all_cols)          
    })                    
    # Helper function to format tables identically          
    format_lm_df <- function(df, scope) {              
      colnames(df)[1:4] <- c("Estimate", "Std. Error", "Statistic", "p-value")                     # Safeguard against NA p-values (e.g., singular fits or zero-variance)       
      df$`p-value`[is.na(df$`p-value`)] <- 1
      df$`Significance` <- ifelse(df$`p-value` <= 0.001, "***",
                           ifelse(df$`p-value` <= 0.01, "**",
                           ifelse(df$`p-value` <= 0.05, "*", "NS")))
      
      df$`Model Scope` <- scope
      df <- df[, c("Model Scope", "Term", "Estimate", "Std. Error", "Statistic", "p-value", "Significance")]
      
      df$Estimate <- round(df$Estimate, 4)                     
    df$`Std. Error` <- round(df$`Std. Error`, 4)
      df$Statistic <- round(df$Statistic, 3)                     
    df$`p-value` <- round(df$`p-value`, 5)
      return(df)
  }

    # 2. Store Fit Results (Separated into Simple and Multi)
# 2. Store Fit Results (Separated into Simple and Multi)
    lm_results <- eventReactive(input$run_lm, {
      req(shared_state$data, input$target_y, input$predictors_x)
      df <- shared_state$data
      
      simple_results <- data.frame()
      
      # A. Always run Simple Linear Regression for EACH predictor individually
      for (i in seq_along(input$predictors_x)) {
        pred <- input$predictors_x[i]
        
        # Using backticks safely handles any spaces in column names
        f_ind <- as.formula(paste0("`", input$target_y, "` ~ `", pred, "`"))
        fit_ind <- lm(f_ind, data = df)
        s_ind <- summary(fit_ind)$coefficients
        
        df_ind <- as.data.frame(s_ind)
        df_ind$Term <- rownames(df_ind)
        df_ind <- format_lm_df(df_ind, paste("Individual:", pred))
        
        # ---> ADDED: Insert explicit Model ID column on the far left <---
        df_ind <- cbind(`Model ID` = paste("Simple Model", i), df_ind)
        
        simple_results <- rbind(simple_results, df_ind)
      }
      
      multi_results <- NULL
      details_str <- NULL
      adj_r2 <- NA
      
      # B. Run Combined Multiple Linear Regression (Only if > 1 predictor)
      if (length(input$predictors_x) > 1) {
        # Safely wrap all predictors in backticks
        safe_preds <- paste0("`", input$predictors_x, "`", collapse = " + ")
        f_multi <- as.formula(paste0("`", input$target_y, "` ~ ", safe_preds))
        
        fit_multi <- lm(f_multi, data = df)
        s_multi <- summary(fit_multi)
        
        df_multi <- as.data.frame(s_multi$coefficients)
        df_multi$Term <- rownames(df_multi)       
        multi_results <- format_lm_df(df_multi, "Combined Multiple Model")
        
        # ---> ADDED: Insert explicit Model ID for the Multiple Model <---
        multi_results <- cbind(`Model ID` = "Multiple Model", multi_results)
        
        adj_r2 <- round(s_multi$adj.r.squared, 3)
        details_str <- paste0("Multiple R-Squared: ", round(s_multi$r.squared, 4), 
                              " | Adjusted R-Squared: ", adj_r2,
                              "\nOverall Model p-value: ", round(pf(s_multi$fstatistic[1], s_multi$fstatistic[2], s_multi$fstatistic[3], lower.tail = FALSE), 5))
      }
      
      # Safe assignment to shared_state$final_models (Prevents silent subsetting errors)
      current_models <- shared_state$final_models
      if (is.null(current_models)) current_models <- list()
      
      current_models[["Linear Regression"]] <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module    = "Linear Regression",
        Action    = if(length(input$predictors_x) > 1) "Individual & Multiple Linear Regression" else "Simple Linear Regression",
        Details   = paste("Y:", input$target_y, "~ X:", paste(input$predictors_x, collapse = ", "), 
                          if(!is.na(adj_r2)) paste("| Adj R2 =", adj_r2) else ""),
        stringsAsFactors = FALSE
      )
      shared_state$final_models <- current_models
      
      list(simple_table = simple_results, multi_table = multi_results, details = details_str)
    })
    
    # 3. Render Outputs independently
    output$lm_simple_table <- renderDT({
      req(lm_results()$simple_table)
      datatable(lm_results()$simple_table, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$multi_model_ui <- renderUI({
      req(lm_results()$multi_table)
      card(
        card_header("2. Multiple Linear Regression (All Predictors Combined)"),
        DTOutput(ns("lm_multi_table")),
        hr(),
        verbatimTextOutput(ns("lm_details"))
      )
    })
    
    output$lm_multi_table <- renderDT({
      req(lm_results()$multi_table)
      datatable(lm_results()$multi_table, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$lm_details <- renderText({ 
      req(lm_results()$details)
      lm_results()$details 
    })

    # 4. Save to Word Report Logic
    observeEvent(input$save_lm_report, {
      req(lm_results()$simple_table)
      
      # Initialize safely if NULL
      if (is.null(shared_state$report_items)) {
        shared_state$report_items <- list()
      }
      
      # Save simple results
      new_simple <- list(
        type = "table", 
        title = paste("Simple Linear Regressions for", input$target_y), 
        data = lm_results()$simple_table
      )
      shared_state$report_items <- append(shared_state$report_items, list(new_simple))
      
      # Save multi results if they exist
      if (!is.null(lm_results()$multi_table)) {
        new_multi <- list(
          type = "table", 
          title = paste("Multiple Linear Regression for", input$target_y), 
          data = lm_results()$multi_table
        )
        shared_state$report_items <- append(shared_state$report_items, list(new_multi))
      }
      
      showNotification("Linear regression results added to Word report!", type = "message")
    })
    
  })
}