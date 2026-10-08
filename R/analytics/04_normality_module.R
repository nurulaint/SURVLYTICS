normality_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Normality Diagnostics"),
      selectInput(ns("num_var"), "1. Select Numeric Variable:", choices = NULL),
      selectInput(ns("group_var"), "2. Group by Categorical Variable (Optional):", choices = "None"),
      selectInput(ns("selected_test"), "3. Select Normality Test:", 
                  choices = c("Shapiro-Wilk", "Lilliefors (K-S)", "Anderson-Darling", "Standard K-S")),
      
      uiOutput(ns("bin_control")),
      hr(),
      
      # Accumulated Table Controls
      # Accumulated Table Controls
      actionButton(ns("execute_test"), "Execute & Add to Summary Table", class = "btn-primary w-100 mb-2"),
      actionButton(ns("delete_selected"), "Delete Selected Test(s)", class = "btn-outline-danger w-100 mb-2"),
      actionButton(ns("clear_tests"), "Clear All Accumulated Tests", class = "btn-light w-100 mb-2"),
      downloadButton(ns("dl_norm_stats"), "Download Metrics CSV", class = "btn-outline-primary btn-sm w-100 mb-3"),
      
      # Save to Report Button
      actionButton(ns("save_report"), "Save Table to Report", icon = icon("bookmark"), class = "btn-success w-100")
    ),
    navset_card_tab(
      nav_panel("Statistical Tests & Metrics", 
        card(
          card_header("Accumulated Test Results Summary Table"),
          DTOutput(ns("norm_table")),
          card_footer("Note: Select rows to delete them. Tests are accumulated as you execute them.")
        )
      ),
      nav_panel("Histogram & Distribution", 
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Histogram with Normal Curve",
            downloadButton(ns("dl_hist"), "Download Plot (PNG)", class = "btn-sm btn-outline-primary")
          ),
          plotOutput(ns("norm_hist"), height = "450px")
        )
      ),
      nav_panel("Q-Q Plot", 
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Normal Q-Q Plot",
            downloadButton(ns("dl_qq"), "Download Plot (PNG)", class = "btn-sm btn-outline-primary")
          ),
          plotOutput(ns("norm_qq"), height = "450px")
        )
      )
    )
  )
}

normality_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Dynamic Dropdown Updates
    observe({
      req(shared_state$data)
      req(ncol(shared_state$data) > 0)
      df <- shared_state$data
      
      num_cols <- names(df)[sapply(df, is.numeric)]
      cat_cols <- names(df)[sapply(df, function(col) is.character(col) || is.factor(col))]
      
      updateSelectInput(session, "num_var", choices = num_cols)
      updateSelectInput(session, "group_var", choices = c("None", cat_cols))
    })
    
    # Dynamic Bin Width Slider
    output$bin_control <- renderUI({
      req(input$num_var, shared_state$data)
      x_data <- shared_state$data[[input$num_var]]
      req(is.numeric(x_data))
      
      rng <- diff(range(x_data, na.rm = TRUE))
      init_bin <- round(rng / 20, 2)
      init_bin <- if(init_bin <= 0) 0.5 else init_bin
      
      sliderInput(ns("bin_width"), "Histogram Bin Width:", 
                  min = round(rng / 100, 2) + 0.01, 
                  max = round(rng / 5, 2) + 0.1, 
                  value = init_bin, 
                  step = 0.01)
    })
    
    # Helper Math Functions for Skewness & Kurtosis
    calc_skewness <- function(x) {
      n <- length(x)
      if (n < 3) return(NA)
      m3 <- sum((x - mean(x))^3) / n
      s3 <- (sd(x) * sqrt((n - 1) / n))^3
      m3 / s3
    }
    
    calc_kurtosis <- function(x) {
      n <- length(x)
      if (n < 4) return(NA)
      m4 <- sum((x - mean(x))^4) / n
      s4 <- (sd(x) * sqrt((n - 1) / n))^4
      (m4 / s4) - 3  # Excess Kurtosis
    }

    # Reactive storage for accumulated tests
    accumulated_norm_results <- reactiveVal(data.frame())

    # 2. Compute Normality Statistics
    current_test_metrics <- reactive({
      req(input$num_var, shared_state$data)
      df <- shared_state$data
      var_name <- input$num_var
      grp_name <- input$group_var
      
      compute_group_stats <- function(sub_x, grp_label = "All Data") {
        sub_x <- na.omit(sub_x)
        n <- length(sub_x)
        
        if (n < 3) {
          return(data.frame(
            Group = grp_label, N = n, Mean = mean(sub_x), SD = sd(sub_x),
            Skewness = NA, Kurtosis = NA, 
            `p-value` = NA,
            Interpretation = "Insufficient Data (N < 3)", check.names = FALSE
          ))
        }
        
        sw_p <- if (n >= 3 && n <= 5000) round(shapiro.test(sub_x)$p.value, 4) else NA
        lillie_p <- if (length(unique(sub_x)) > 1) round(nortest::lillie.test(sub_x)$p.value, 4) else NA
        ad_p <- if (n >= 8) round(nortest::ad.test(sub_x)$p.value, 4) else NA
        ks_p <- if (n >= 1) round(suppressWarnings(ks.test(sub_x, "pnorm", mean(sub_x), sd(sub_x))$p.value), 4) else NA
        
        eval_p <- switch(input$selected_test,
                         "Shapiro-Wilk" = sw_p,
                         "Lilliefors (K-S)" = lillie_p,
                         "Anderson-Darling" = ad_p,
                         "Standard K-S" = ks_p)
        
        verdict <- if (!is.na(eval_p) && eval_p > 0.05) {
          "Normal (p > 0.05)"
        } else if (!is.na(eval_p)) {
          "Non-Normal (p <= 0.05)"
        } else {
          "Test N/A (Insufficient N)"
        }
        
        data.frame(
          Group = grp_label,
          N = n,
          Mean = round(mean(sub_x), 3),
          SD = round(sd(sub_x), 3),
          Skewness = round(calc_skewness(sub_x), 3),
          Kurtosis = round(calc_kurtosis(sub_x), 3),
          `p-value` = ifelse(is.na(eval_p), "N/A", eval_p),
          Interpretation = verdict,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
      
      if (grp_name == "None") {
        compute_group_stats(df[[var_name]], "All Data")
      } else {
        split_data <- split(df[[var_name]], df[[grp_name]])
        res <- lapply(names(split_data), function(g) compute_group_stats(split_data[[g]], g))
        do.call(rbind, res)
      }
    })

    # --- Execute & Accumulate ---
    observeEvent(input$execute_test, {
      tryCatch({
        req(current_test_metrics())
        new_test <- current_test_metrics()
        
        final_row <- data.frame(
          Variable = input$num_var,
          Group = new_test$Group,
          N = new_test$N,
          Mean = new_test$Mean,
          SD = new_test$SD,
          Skewness = new_test$Skewness,
          Kurtosis = new_test$Kurtosis,
          `Executed Test` = input$selected_test,
          `p-value` = as.character(new_test$`p-value`), 
          Interpretation = new_test$Interpretation,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        current_accumulated <- accumulated_norm_results()
        if (nrow(current_accumulated) == 0) {
          updated_df <- final_row
        } else {
          updated_df <- rbind(current_accumulated, final_row)
        }
        
        accumulated_norm_results(updated_df)
        
        if (is.null(shared_state$final_models)) {
          shared_state$final_models <- list()
        }
        shared_state$final_models$normality <- updated_df
        
        if (is.null(shared_state$audit_log)) {
          shared_state$audit_log <- data.frame(Timestamp=character(), Module=character(), Action=character(), Details=character(), stringsAsFactors=FALSE)
        }
        log_entry <- data.frame(
          Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          Module = "Normality Diagnostics",
          Action = "Normality Test Executed",
          Details = paste("Variable:", input$num_var, "| Test Selected:", input$selected_test),
          stringsAsFactors = FALSE
        )
        shared_state$audit_log <- rbind(shared_state$audit_log, log_entry)
        
        showNotification("Test added to summary table!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("ERROR:", e$message), type = "error", duration = 15)
        print(paste("ERROR IN EXECUTE:", e$message))
      })
    })

    # --- Save Accumulated Table to Word Report ---
    observeEvent(input$save_report, {
      req(accumulated_norm_results())
      df <- accumulated_norm_results()
      
      if(nrow(df) == 0) {
         showNotification("Table is empty! Execute a test first.", type = "warning")
         return()
      }
      
      new_item <- list(
        type = "table", 
        title = "Accumulated Normality Diagnostics", 
        data = df
      )
      shared_state$report_items <- append(shared_state$report_items, list(new_item))
      
      showNotification("Accumulated table successfully added to Word report!", type = "message")
    })

    # --- Delete Selected Rows ---
    observeEvent(input$delete_selected, {
      req(input$norm_table_rows_selected)
      current_df <- accumulated_norm_results()
      current_df <- current_df[-input$norm_table_rows_selected, , drop = FALSE]
      accumulated_norm_results(current_df)
      shared_state$final_models$normality <- current_df 
    })

    # --- Clear All Rows ---
    observeEvent(input$clear_tests, {
      accumulated_norm_results(data.frame())
      shared_state$final_models$normality <- NULL 
    })

    # --- Render the Accumulated Table ---
    output$norm_table <- renderDT({
      df <- accumulated_norm_results()
      req(nrow(df) > 0)
      datatable(df, rownames = FALSE, selection = "multiple", 
                options = list(dom = 't', scrollX = TRUE))
    })

    # --- Download Handler for CSV ---
    output$dl_norm_stats <- downloadHandler(
      filename = function() { paste0("Accumulated_Normality_Tests_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(accumulated_norm_results(), file, row.names = FALSE) }
    )

    # 3. Visualizations: Reactive Histogram
    hist_plot_obj <- reactive({
      req(input$num_var, shared_state$data)
      df <- shared_state$data
      x <- input$num_var
      grp <- if(input$group_var != "None") input$group_var else NULL
      bw <- if(!is.null(input$bin_width)) input$bin_width else NULL
      
      p <- ggplot(df, aes(x = .data[[x]]))
      
      if (!is.null(grp)) {
        p <- p + geom_histogram(aes(y = after_stat(density), fill = as.factor(.data[[grp]])), 
                                binwidth = bw, color = "white", alpha = 0.7) +
                 facet_wrap(vars(.data[[grp]])) +
                 labs(fill = grp)
      } else {
        p <- p + geom_histogram(aes(y = after_stat(density)), 
                                binwidth = bw, fill = "steelblue", color = "white", alpha = 0.7) +
                 stat_function(fun = dnorm, 
                               args = list(mean = mean(df[[x]], na.rm = TRUE), sd = sd(df[[x]], na.rm = TRUE)), 
                               color = "red", linewidth = 1)
      }
      
      p + theme_minimal(base_size = 14) + 
          labs(title = paste("Distribution & Normal Overlay for", x), y = "Density") +
          theme(legend.position = "top")
    })

    output$norm_hist <- renderPlot({ hist_plot_obj() })

    output$dl_hist <- downloadHandler(
      filename = function() { paste0("Histogram_", input$num_var, "_", Sys.Date(), ".png") },
      content = function(file) { ggsave(file, plot = hist_plot_obj(), width = 8, height = 5, bg = "white") }
    )

    # 4. Visualizations: Reactive Q-Q Plot
    qq_plot_obj <- reactive({
      req(input$num_var, shared_state$data)
      df <- shared_state$data
      x <- input$num_var
      grp <- if(input$group_var != "None") input$group_var else NULL
      
      p <- ggplot(df, aes(sample = .data[[x]])) +
        stat_qq(color = "steelblue", size = 2) +
        stat_qq_line(color = "red", linewidth = 1)
      
      if (!is.null(grp)) {
        p <- p + facet_wrap(vars(.data[[grp]]))
      }
      
      p + theme_minimal(base_size = 14) + 
          labs(title = paste("Normal Q-Q Plot for", x), x = "Theoretical Quantiles", y = "Sample Quantiles")
    })

    output$norm_qq <- renderPlot({ qq_plot_obj() })

    output$dl_qq <- downloadHandler(
      filename = function() { paste0("QQ_Plot_", input$num_var, "_", Sys.Date(), ".png") },
      content = function(file) { ggsave(file, plot = qq_plot_obj(), width = 8, height = 5, bg = "white") }
    )
  })
}