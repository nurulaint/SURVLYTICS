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
          "Data Wrangling",
          "Descriptive Statistics",
          "Normality Diagnostics",
          "Hypothesis Testing",
          "Linear Regression",
          "Logistic Regression"
        ),
        selected = c(
          "Data Wrangling",
          "Descriptive Statistics",
          "Normality Diagnostics",
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
    
    # 1. Consolidate Audit Log Safely
    raw_audit_df <- reactive({
      all_logs <- list()
      
      # A. Pull in Data Wrangling, Descriptives, and Normality (from audit_log)
      if (!is.null(shared_state$audit_log) && nrow(shared_state$audit_log) > 0) {
        clean_audit <- data.frame(lapply(shared_state$audit_log, as.character), stringsAsFactors = FALSE)
        all_logs[[length(all_logs) + 1]] <- clean_audit
      }
      
      # B. Pull in Regressions and Hypothesis Testing (from final_models)
      models <- shared_state$final_models
      if (!is.null(models) && length(models) > 0) {
        valid_models <- Filter(Negate(is.null), models)
        if (length(valid_models) > 0) {
          for (mod_name in names(valid_models)) {
            df <- valid_models[[mod_name]]
            if (is.null(df) || nrow(df) == 0) next
            if (!"Module" %in% names(df)) df$Module <- mod_name
            clean_df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
            all_logs[[length(all_logs) + 1]] <- clean_df
          }
        }
      }
      
      # C. Bind them all together
      if (length(all_logs) == 0) return(empty_audit_df())
      
      if (requireNamespace("dplyr", quietly = TRUE)) {
        return(as.data.frame(dplyr::bind_rows(all_logs)))
      } else {
        all_cols <- unique(unlist(lapply(all_logs, names)))
        aligned_list <- lapply(all_logs, function(df) {
          missing_cols <- setdiff(all_cols, names(df))
          for (col in missing_cols) df[[col]] <- ""
          df[, all_cols, drop = FALSE]
        })
        return(do.call(rbind, aligned_list))
      }
    })
    
    empty_audit_df <- function() {
      data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module = "None",
        Action = "No Analysis Executed",
        Details = "No actions recorded during this session.",
        stringsAsFactors = FALSE
      )
    }

    filtered_audit_df <- reactive({
      df <- raw_audit_df()
      if ("Module" %in% names(df) && !is.null(input$include_modules)) {
        df <- df[df$Module %in% c(input$include_modules, "None"), , drop = FALSE]
      }
      df
    })
    
    # 2. Dynamic Workflow Diagram (Now with Intelligent Module Detection!)
    output$workflow_diagram <- DiagrammeR::renderGrViz({
      df_log <- filtered_audit_df()
      active_modules <- if ("Module" %in% names(df_log)) df_log$Module else character(0)
      
      # --- SMART INFERENCE: Detect modules that bypassed the text log ---
      if (!is.null(shared_state$report_items)) {
        saved_titles <- sapply(shared_state$report_items, function(x) x$title)
        
        # If any Descriptive items are saved, activate the Descriptives node
        if (any(grepl("Numerical Summary|Categorical Summary|Visualization", saved_titles))) {
          active_modules <- c(active_modules, "Descriptive Statistics")
        }
        # If any Normality items are saved, activate the Normality node
        if (any(grepl("Normality|Histogram|Q-Q", saved_titles))) {
          active_modules <- c(active_modules, "Normality Diagnostics")
        }
      }
      
      # If a dataset exists, Data Wrangling is inherently active
      if (!is.null(shared_state$data)) {
        active_modules <- c(active_modules, "Data Wrangling")
      }
      # ------------------------------------------------------------------
      
      total_rows <- if(!is.null(shared_state$data)) nrow(shared_state$data) else 0
      total_cols <- if(!is.null(shared_state$data)) ncol(shared_state$data) else 0
      
      # Initialize the starting node
      nodes <- c(paste0("Start [label = 'Data Loaded\\n(", total_rows, " rows, ", total_cols, " cols)', fillcolor = '#28a745', fontcolor = white]"))
      edges <- c()
      last_node <- "Start"
      
      if ("Data Wrangling" %in% active_modules) {
        nodes <- c(nodes, "Clean [label = 'Data Wrangling', fillcolor = '#007bff', fontcolor = white]")
        edges <- c(edges, paste(last_node, "-> Clean"))
        last_node <- "Clean"
      }
      
      if ("Descriptive Statistics" %in% active_modules) {
        nodes <- c(nodes, "Desc [label = 'Descriptives & Visuals', fillcolor = '#007bff', fontcolor = white]")
        edges <- c(edges, paste(last_node, "-> Desc"))
        last_node <- "Desc"
      }
      
      if ("Normality Diagnostics" %in% active_modules) {
        nodes <- c(nodes, "Norm [label = 'Normality Tests', fillcolor = '#007bff', fontcolor = white]")
        edges <- c(edges, paste(last_node, "-> Norm"))
        last_node <- "Norm"
      }
      
      if ("Hypothesis Testing" %in% active_modules) {
        nodes <- c(nodes, "HT [label = 'Hypothesis Testing', fillcolor = '#007bff', fontcolor = white]")
        edges <- c(edges, paste(last_node, "-> HT"))
        last_node <- "HT"
      }
      
      # Branching for Regression
      if ("Linear Regression" %in% active_modules) {
        nodes <- c(nodes, "LM [label = 'Linear Regression', fillcolor = '#007bff', fontcolor = white]")
        edges <- c(edges, paste(last_node, "-> LM"))
        lm_last <- "LM"
      } else { lm_last <- NULL }
      
      if ("Logistic Regression" %in% active_modules) {
        nodes <- c(nodes, "GLM [label = 'Logistic Regression', fillcolor = '#007bff', fontcolor = white]")
        edges <- c(edges, paste(last_node, "-> GLM"))
        glm_last <- "GLM"
      } else { glm_last <- NULL }
      
      # Connect to the final End Report node
      nodes <- c(nodes, "End [label = 'Final Summary Report', fillcolor = '#dc3545', fontcolor = white]")
      
      if (!is.null(lm_last)) { edges <- c(edges, paste(lm_last, "-> End")) }
      if (!is.null(glm_last)) { edges <- c(edges, paste(glm_last, "-> End")) }
      if (is.null(lm_last) && is.null(glm_last)) { edges <- c(edges, paste(last_node, "-> End")) }
      
      dot_script <- paste0("
        digraph workflow {
          graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.8]
          node [shape = rectangle, style = filled, fontname = Helvetica, fontsize = 11, fixedsize = false]
          
          ", paste(nodes, collapse = "\n          "), "
          ", paste(edges, collapse = "\n          "), "
        }
      ")
      
      DiagrammeR::grViz(dot_script)
    })
    
    # 3. Render Audit Table
    output$audit_table <- renderDT({
      datatable(filtered_audit_df(), rownames = FALSE, selection = "multiple",
                options = list(pageLength = 10, dom = 'ftip', scrollX = TRUE))
    })
    
    # Delete Selected Row
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
    
    # 4. Native .docx Download Handler (Readable Text Wrapping)
    output$dl_word_summary <- downloadHandler(
      filename = function() { paste0("Executive_Analytics_Summary_", Sys.Date(), ".docx") },
      content = function(file) {
        raw_df <- filtered_audit_df()
        req(nrow(raw_df) > 0)
        
        log_data <- data.frame(
          lapply(raw_df, function(col) {
            clean_col <- as.character(col)
            clean_col[is.na(clean_col)] <- ""
            clean_col
          }),
          stringsAsFactors = FALSE, check.names = FALSE
        )
        
        tryCatch({
          req(requireNamespace("officer", quietly = TRUE))
          req(requireNamespace("flextable", quietly = TRUE)) 
          
          doc <- officer::read_docx()
          
          # --- Section 1: Headers ---
          doc <- officer::body_add_par(doc, "Executive Data Science & Statistical Analysis Report", style = "heading 1")
          doc <- officer::body_add_par(doc, paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          doc <- officer::body_add_par(doc, "1. Executive Overview", style = "heading 2")
          doc <- officer::body_add_par(doc, paste("- Total Dataset Records (Rows):", if(!is.null(shared_state$data)) nrow(shared_state$data) else 0), style = "Normal")
          doc <- officer::body_add_par(doc, paste("- Total Attributes (Columns):", if(!is.null(shared_state$data)) ncol(shared_state$data) else 0), style = "Normal")
          doc <- officer::body_add_par(doc, paste("- Included Modules:", paste(input$include_modules, collapse = ", ")), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          # --- Section 2: Inject Saved Report Items (TEXT WRAPPING FORMATTING) ---
          if (!is.null(shared_state$report_items) && length(shared_state$report_items) > 0) {
            doc <- officer::body_add_par(doc, "2. Saved Statistical Outputs & Visualizations", style = "heading 2")
            
            for (item in shared_state$report_items) {
              doc <- officer::body_add_par(doc, item$title, style = "heading 3")
              
              if (item$type == "table") {
                ft <- flextable::flextable(item$data)
                ft <- flextable::theme_vanilla(ft)
                ft <- flextable::fontsize(ft, size = 9, part = "all")
                ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
                
                doc <- flextable::body_add_flextable(doc, value = ft)
              } else if (item$type == "plot") {
                doc <- officer::body_add_gg(doc, value = item$data, width = 6, height = 4)
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          # --- Section 3: The Audit Log ---
          audit_heading <- if (!is.null(shared_state$report_items) && length(shared_state$report_items) > 0) {
            "3. Validated Audit Trail of Executed Models"
          } else {
            "2. Validated Audit Trail of Executed Models"
          }
          
          doc <- officer::body_add_par(doc, audit_heading, style = "heading 2")
          
          ft_log <- flextable::flextable(log_data)
          ft_log <- flextable::theme_vanilla(ft_log)
          ft_log <- flextable::fontsize(ft_log, size = 9, part = "all")
          ft_log <- flextable::set_table_properties(ft_log, layout = "autofit", width = 1)
          
          doc <- flextable::body_add_flextable(doc, value = ft_log)
          
          print(doc, target = file)
          
        }, error = function(e) {
          showNotification(paste("Error creating Word file:", e$message), type = "error", duration = 10)
        })
      }
    )
  })
}