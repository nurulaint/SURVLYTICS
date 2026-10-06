# R/analytics/08_summary_report_module.R

summary_report_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Report Builder & Completion"),
      p("Select the modules you wish to include in your final Word summary report:"),
      
      checkboxGroupInput(
        ns("include_modules"),
        "Include Modules in Summary:",
        choices = c(
          "Data Preprocessing",
          "Descriptive Statistics",
          "Hypothesis Testing",
          "Linear Regression",
          "Logistic Regression"
        ),
        selected = c(
          "Data Wrangling",
          "Descriptive Statistics",
          "Hypothesis Testing",
          "Linear Regression",
          "Logistic Regression"
        )
      ),
      
      hr(),
      actionButton(ns("finish_analysis"), "Refresh Audit & Diagram", class = "btn-success w-100 mb-3"),
      downloadButton(ns("dl_word_summary"), "Download Word Summary (.docx)", class = "btn-primary w-100")
    ),
    navset_card_tab(
      nav_panel("Workflow Diagram", 
        card(
          card_header("Analytical Execution Flowchart"),
          DiagrammeR::grVizOutput(ns("workflow_diagram"), height = "450px")
        )
      ),
      nav_panel("Executed Analyses Audit", 
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Session Audit Log (Select rows to remove unwanted runs)",
            actionButton(ns("delete_selected_row"), "Delete Selected Row(s)", class = "btn-sm btn-outline-danger")
          ),
          DTOutput(ns("audit_table"))
        )
      )
    )
  )
}

summary_report_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Consolidate Audit Log or Final Models Safely
    raw_audit_df <- reactive({
      input$finish_analysis
      
      models <- shared_state$final_models
      
      if (!is.null(models) && length(models) > 0) {
        # Filter out NULL or empty entries
        valid_models <- compact(models)
        
        if (length(valid_models) == 0) {
          return(empty_audit_df())
        }
        
        # Standardize data frames before binding if columns differ
        standardized_list <- lapply(names(valid_models), function(mod_name) {
          df <- valid_models[[mod_name]]
          if (is.null(df) || nrow(df) == 0) return(NULL)
          
          # Ensure Module column exists for filtering
          if (!"Module" %in% names(df)) {
            df$Module <- mod_name
          }
          
          # Convert all values to character to prevent binding class mismatches
          data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
        })
        
        # Remove empty items
        standardized_list <- Filter(Negate(is.null), standardized_list)
        
        if (length(standardized_list) == 0) return(empty_audit_df())
        
        # Safe binding across data frames with mismatched columns
        if (requireNamespace("dplyr", quietly = TRUE)) {
          return(as.data.frame(dplyr::bind_rows(standardized_list)))
        } else {
          # Base R fallback for mismatched column binding
          all_cols <- unique(unlist(lapply(standardized_list, names)))
          aligned_list <- lapply(standardized_list, function(df) {
            missing_cols <- setdiff(all_cols, names(df))
            for (col in missing_cols) df[[col]] <- ""
            df[, all_cols, drop = FALSE]
          })
          return(do.call(rbind, aligned_list))
        }
        
      } else if (!is.null(shared_state$audit_log) && nrow(shared_state$audit_log) > 0) {
        return(shared_state$audit_log)
      } else {
        return(empty_audit_df())
      }
    })
    
    # Helper for fallback empty state
    empty_audit_df <- function() {
      data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module = "None",
        Action = "No Analysis Executed",
        Details = "No actions recorded during this session.",
        stringsAsFactors = FALSE
      )
    }

    # Filter Audit DF based on user-selected checkboxes
    filtered_audit_df <- reactive({
      df <- raw_audit_df()
      if ("Module" %in% names(df) && !is.null(input$include_modules)) {
        df <- df[df$Module %in% c(input$include_modules, "None"), , drop = FALSE]
      }
      df
    })
    
    # 2. Delete Selected Rows from Audit Table
    observeEvent(input$delete_selected_row, {
      req(input$audit_table_rows_selected)
      selected_idx <- input$audit_table_rows_selected
      
      if (!is.null(shared_state$final_models) && length(shared_state$final_models) > 0) {
        shared_state$final_models <- shared_state$final_models[-selected_idx]
      } else if (!is.null(shared_state$audit_log) && nrow(shared_state$audit_log) > 0) {
        shared_state$audit_log <- shared_state$audit_log[-selected_idx, , drop = FALSE]
      }
      
      showNotification("Removed selected analysis result(s) from audit log.", type = "warning")
    })
    
    # 3. Generate Workflow Diagram using DiagrammeR Graphviz
    output$workflow_diagram <- DiagrammeR::renderGrViz({
      df_log <- filtered_audit_df()
      active_modules <- if ("Module" %in% names(df_log)) df_log$Module else character(0)
      
      has_clean <- "Data Wrangling" %in% active_modules
      has_desc  <- "Descriptive Statistics" %in% active_modules
      has_norm  <- "Normality Diagnostics" %in% active_modules
      has_ht    <- "Hypothesis Testing" %in% active_modules
      has_lm    <- "Linear Regression" %in% active_modules
      has_glm   <- "Logistic Regression" %in% active_modules
      
      total_rows <- if(!is.null(shared_state$data)) nrow(shared_state$data) else 0
      total_cols <- if(!is.null(shared_state$data)) ncol(shared_state$data) else 0
      
      dot_script <- paste0("
        digraph workflow {
          graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.8]
          node [shape = rectangle, style = filled, fontname = Helvetica, fontsize = 11, fixedsize = false]
          
          Start [label = 'Data Loaded\\n(", total_rows, " rows, ", total_cols, " cols)', fillcolor = '#d4edda', color = '#28a745']
          Clean [label = 'Data Wrangling', fillcolor = '", if(has_clean) "#cce5ff" else "#e2e3e5", "', color = '", if(has_clean) "#004085" else "#6c757d", "']
          Desc  [label = 'Descriptives & Visuals', fillcolor = '", if(has_desc) "#cce5ff" else "#e2e3e5", "', color = '", if(has_desc) "#004085" else "#6c757d", "']
          Norm  [label = 'Normality Tests', fillcolor = '", if(has_norm) "#cce5ff" else "#e2e3e5", "', color = '", if(has_norm) "#004085" else "#6c757d", "']
          HT    [label = 'Hypothesis Testing', fillcolor = '", if(has_ht) "#cce5ff" else "#e2e3e5", "', color = '", if(has_ht) "#004085" else "#6c757d", "']
          LM    [label = 'Linear Regression', fillcolor = '", if(has_lm) "#cce5ff" else "#e2e3e5", "', color = '", if(has_lm) "#004085" else "#6c757d", "']
          GLM   [label = 'Logistic Regression', fillcolor = '", if(has_glm) "#cce5ff" else "#e2e3e5", "', color = '", if(has_glm) "#004085" else "#6c757d", "']
          End   [label = 'Final Summary Report', fillcolor = '#f8d7da', color = '#dc3545']
          
          Start -> Clean -> Desc -> Norm -> HT
          HT -> LM -> End
          HT -> GLM -> End
          HT -> End
        }
      ")
      
      DiagrammeR::grViz(dot_script)
    })
    
    # 4. Render Audit Log Table
    output$audit_table <- renderDT({
      datatable(
        filtered_audit_df(), 
        rownames = FALSE, 
        selection = "multiple",
        options = list(pageLength = 10, dom = 'ftip', scrollX = TRUE)
      )
    })
    
    # 5. Native .docx Download Handler via officer
    output$dl_word_summary <- downloadHandler(
      filename = function() { 
        paste0("Executive_Analytics_Summary_", Sys.Date(), ".docx") 
      },
      content = function(file) {
        raw_df <- filtered_audit_df()
        req(nrow(raw_df) > 0)
        
        log_data <- data.frame(
          lapply(raw_df, function(col) {
            clean_col <- as.character(col)
            clean_col[is.na(clean_col)] <- ""
            clean_col
          }),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
        tryCatch({
          req(requireNamespace("officer", quietly = TRUE))
          
          doc <- officer::read_docx()
          
          doc <- officer::body_add_par(doc, "Executive Data Science & Statistical Analysis Report", style = "heading 1")
          doc <- officer::body_add_par(doc, paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          doc <- officer::body_add_par(doc, "1. Executive Overview", style = "heading 2")
          doc <- officer::body_add_par(doc, paste("- Total Dataset Records (Rows):", if(!is.null(shared_state$data)) nrow(shared_state$data) else 0), style = "Normal")
          doc <- officer::body_add_par(doc, paste("- Total Attributes (Columns):", if(!is.null(shared_state$data)) ncol(shared_state$data) else 0), style = "Normal")
          doc <- officer::body_add_par(doc, paste("- Included Modules:", paste(input$include_modules, collapse = ", ")), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          doc <- officer::body_add_par(doc, "2. Validated Audit Trail of Executed Models", style = "heading 2")
          doc <- officer::body_add_table(doc, value = log_data, style = "table_template")
          
          print(doc, target = file)
          
        }, error = function(e) {
          showNotification(paste("Error creating Word file:", e$message), type = "error", duration = 10)
        })
      }
    )
  })
}