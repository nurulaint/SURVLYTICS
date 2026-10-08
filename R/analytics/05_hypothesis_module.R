# R/analytics/04_hypothesis_module.R

hypothesis_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Hypothesis Testing"),
      selectInput(ns("test_type"), "Select Test Family:", 
                  choices = c("One-Sample t-test", 
                              "Two-Sample t-test", 
                              "One-way ANOVA", 
                              "Wilcoxon Signed-Rank Test (1-Sample Non-parametric)",
                              "Mann-Whitney U / Wilcoxon Rank-Sum (2-Sample Non-parametric)",
                              "Kruskal-Wallis Test (ANOVA Non-parametric)",
                              "Correlation Test", 
                              "Chi-Square Test of Independence")),
      
      uiOutput(ns("test_controls")),
      
      hr(),
      actionButton(ns("add_test"), "Execute & Add to Summary Table", class = "btn-primary w-100 mb-2"),
      actionButton(ns("delete_selected"), "Delete Selected Test(s)", class = "btn-outline-danger w-100"),
      actionButton(ns("clear_tests"), "Clear All Accumulated Tests", class = "btn-outline-secondary w-100 mb-2"),
      downloadButton(ns("dl_pdf"), "Download Test Summary PDF", class = "btn-outline-danger w-100")
    ),
    card(
      card_header("Accumulated Test Results Summary Table"),
      # ... (the rest of your table output code)
      ),
      DTOutput(ns("ht_table")),
      hr(),
      h5("Latest Test Execution Details"),
      verbatimTextOutput(ns("ht_details"))
    )
}

hypothesis_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Text notes for the latest executed run
    latest_details <- reactiveVal("No tests executed in this session yet.")
    
    # 1. Dynamic Controls UI
    output$test_controls <- renderUI({
      req(shared_state$data)
      df <- shared_state$data
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      cat_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
      
      # 1-Sample Tests
      if (input$test_type %in% c("One-Sample t-test", "Wilcoxon Signed-Rank Test (1-Sample Non-parametric)")) {
        tagList(
          selectInput(ns("num_var"), "Target Numeric Variable (Y):", choices = num_cols),
          numericInput(ns("mu_val"), "Hypothesized Baseline (μ₀ / Median):", value = 0),
          selectInput(ns("alt_hypothesis"), "Alternative (H₁):", 
                      choices = c("Two-sided (≠)" = "two.sided", "Greater (>)" = "greater", "Less (<)" = "less"))
        )
      } 
      # 2-Sample Tests
      else if (input$test_type %in% c("Two-Sample t-test", "Mann-Whitney U / Wilcoxon Rank-Sum (2-Sample Non-parametric)")) {
        tagList(
          selectInput(ns("t2_design"), "Comparison Setup:", 
                      choices = c("Compare 1 Variable across 2 Selected Groups" = "by_group", 
                                  "Compare 2 Continuous Variables" = "two_vars")),
          
          uiOutput(ns("t2_var_selection")),
          
          selectInput(ns("paired_choice"), "Sample Design:", 
                      choices = c("Independent Samples" = "FALSE", "Paired Samples" = "TRUE")),
          
          selectInput(ns("alt_hypothesis"), "Alternative Hypothesis (H₁):", 
                      choices = c("Two-sided (≠)" = "two.sided", "Greater (>)" = "greater", "Less (<)" = "less")),
          
          if (input$test_type == "Two-Sample t-test") {
            checkboxInput(ns("var_equal"), "Assume Equal Variances (Student's t-test)", value = FALSE)
          }
        )
      } 
      # ANOVA Tests
      else if (input$test_type %in% c("One-way ANOVA", "Kruskal-Wallis Test (ANOVA Non-parametric)")) {
        tagList(
          selectInput(ns("num_var"), "Outcome Variable (Y):", choices = num_cols),
          selectInput(ns("group_var"), "Grouping Variable (X):", choices = cat_cols)
        )
      } 
      # Correlation Test
      else if (input$test_type == "Correlation Test") {
        tagList(
          selectInput(ns("cor_x"), "Numeric Variable X:", choices = num_cols),
          selectInput(ns("cor_y"), "Numeric Variable Y:", choices = num_cols),
          selectInput(ns("cor_method"), "Method:", 
                      choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
          selectInput(ns("alt_hypothesis"), "Alternative (H₁):", 
                      choices = c("Two-sided (≠)" = "two.sided", "Greater (>)" = "greater", "Less (<)" = "less"))
        )
      } 
      # Chi-Square Test
      else if (input$test_type == "Chi-Square Test of Independence") {
        tagList(
          selectInput(ns("cat_x"), "Variable X:", choices = cat_cols),
          selectInput(ns("cat_y"), "Variable Y:", choices = cat_cols),
          checkboxInput(ns("correct_yates"), "Yates' Continuity Correction", value = TRUE)
        )
      }
    })
    
    # Sub-UI for 2-Sample Level Selection
    output$t2_var_selection <- renderUI({
      req(input$test_type %in% c("Two-Sample t-test", "Mann-Whitney U / Wilcoxon Rank-Sum (2-Sample Non-parametric)"), 
          input$t2_design, shared_state$data)
      df <- shared_state$data
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      cat_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
      
      if (input$t2_design == "by_group") {
        tagList(
          selectInput(ns("num_var"), "Continuous Outcome (Y):", choices = num_cols),
          selectInput(ns("group_var"), "Categorical Grouping Variable (X):", choices = cat_cols),
          uiOutput(ns("group_levels_ui"))
        )
      } else {
        tagList(
          selectInput(ns("num_var1"), "First Numeric Variable:", choices = num_cols),
          selectInput(ns("num_var2"), "Second Numeric Variable:", choices = num_cols)
        )
      }
    })
    
    output$group_levels_ui <- renderUI({
      req(input$group_var, shared_state$data)
      levels_avail <- unique(na.omit(as.character(shared_state$data[[input$group_var]])))
      
      if (length(levels_avail) < 2) return(p("Select variable with >= 2 categories", class="text-danger"))
      
      tagList(
        selectInput(ns("grp1_level"), "Select Group 1:", choices = levels_avail, selected = levels_avail[1]),
        selectInput(ns("grp2_level"), "Select Group 2:", choices = levels_avail, selected = levels_avail[min(2, length(levels_avail))])
      )
    })
    
    # 2. Accumulative Execution Engine (Appends Results)
    observeEvent(input$add_test, {
      req(shared_state$data, input$test_type)
      df <- shared_state$data
      
      new_row <- NULL
      details_str <- ""
      
      # ONE-SAMPLE TESTS
      if (input$test_type %in% c("One-Sample t-test", "Wilcoxon Signed-Rank Test (1-Sample Non-parametric)")) {
        req(input$num_var)
        x <- na.omit(df[[input$num_var]])
        
        if (input$test_type == "One-Sample t-test") {
          res <- t.test(x, mu = input$mu_val, alternative = input$alt_hypothesis)
          stat_type <- "t"
        } else {
          res <- wilcox.test(x, mu = input$mu_val, alternative = input$alt_hypothesis)
          stat_type <- "V"
        }
        
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%H:%M:%S"),
          `Test Name` = input$test_type,
          `Variables / Groups` = input$num_var,
          `Statistic` = paste0(stat_type, " = ", round(res$statistic, 3)),
          `p-value` = round(res$p.value, 5),
          `Conclusion` = if(res$p.value <= 0.05) "Reject H₀" else "Fail to Reject H₀",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        details_str <- paste0(input$test_type, " on ", input$num_var, " against μ₀ = ", input$mu_val)
      }
      
      # TWO-SAMPLE TESTS
      else if (input$test_type %in% c("Two-Sample t-test", "Mann-Whitney U / Wilcoxon Rank-Sum (2-Sample Non-parametric)")) {
        req(input$t2_design, input$paired_choice, input$alt_hypothesis)
        is_paired <- as.logical(input$paired_choice)
        
        if (input$t2_design == "by_group") {
          req(input$num_var, input$group_var, input$grp1_level, input$grp2_level)
          sub_df <- df[df[[input$group_var]] %in% c(input$grp1_level, input$grp2_level) & !is.na(df[[input$num_var]]), ]
          val1 <- sub_df[[input$num_var]][sub_df[[input$group_var]] == input$grp1_level]
          val2 <- sub_df[[input$num_var]][sub_df[[input$group_var]] == input$grp2_level]
          
          if (input$test_type == "Two-Sample t-test") {
            res <- t.test(val1, val2, alternative = input$alt_hypothesis, var.equal = input$var_equal, paired = is_paired)
            stat_type <- "t"
          } else {
            res <- wilcox.test(val1, val2, alternative = input$alt_hypothesis, paired = is_paired)
            stat_type <- "W"
          }
          vars_str <- paste0(input$num_var, " (", input$grp1_level, " vs ", input$grp2_level, ")")
        } else {
          req(input$num_var1, input$num_var2)
          sub_df <- na.omit(df[, c(input$num_var1, input$num_var2)])
          val1 <- sub_df[[input$num_var1]]
          val2 <- sub_df[[input$num_var2]]
          
          if (input$test_type == "Two-Sample t-test") {
            res <- t.test(val1, val2, alternative = input$alt_hypothesis, var.equal = input$var_equal, paired = is_paired)
            stat_type <- "t"
          } else {
            res <- wilcox.test(val1, val2, alternative = input$alt_hypothesis, paired = is_paired)
            stat_type <- "V/W"
          }
          vars_str <- paste0(input$num_var1, " vs ", input$num_var2)
        }
        
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%H:%M:%S"),
          `Test Name` = paste0(input$test_type, if(is_paired) " (Paired)" else " (Independent)"),
          `Variables / Groups` = vars_str,
          `Statistic` = paste0(stat_type, " = ", round(res$statistic, 3)),
          `p-value` = round(res$p.value, 5),
          `Conclusion` = if(res$p.value <= 0.05) "Reject H₀" else "Fail to Reject H₀",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        details_str <- paste0("Alternative hypothesis: ", res$alternative)
      }
      
      # ANOVA & KRUSKAL-WALLIS
      else if (input$test_type %in% c("One-way ANOVA", "Kruskal-Wallis Test (ANOVA Non-parametric)")) {
        req(input$num_var, input$group_var)
        sub_df <- df[!is.na(df[[input$num_var]]) & !is.na(df[[input$group_var]]), ]
        f <- as.formula(paste(input$num_var, "~ as.factor(", input$group_var, ")"))
        
        if (input$test_type == "One-way ANOVA") {
          fit <- aov(f, data = sub_df)
          s <- summary(fit)[[1]]
          p_val <- s[["Pr(>F)"]][1]
          stat_val <- paste0("F = ", round(s[["F value"]][1], 3))
        } else {
          res <- kruskal.test(f, data = sub_df)
          p_val <- res$p.value
          stat_val <- paste0("Chi-Sq = ", round(res$statistic, 3))
        }
        
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%H:%M:%S"),
          `Test Name` = input$test_type,
          `Variables / Groups` = paste(input$num_var, "by", input$group_var),
          `Statistic` = stat_val,
          `p-value` = round(p_val, 5),
          `Conclusion` = if(p_val <= 0.05) "Reject H₀ (Group diffs)" else "Fail to Reject H₀",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        details_str <- paste("Evaluated across categories of", input$group_var)
      }
      
      # CORRELATION TEST
      else if (input$test_type == "Correlation Test") {
        req(input$cor_x, input$cor_y)
        sub_df <- na.omit(df[, c(input$cor_x, input$cor_y)])
        res <- cor.test(sub_df[[input$cor_x]], sub_df[[input$cor_y]], method = input$cor_method, alternative = input$alt_hypothesis)
        
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%H:%M:%S"),
          `Test Name` = paste("Correlation (", toupper(input$cor_method), ")"),
          `Variables / Groups` = paste(input$cor_x, "&", input$cor_y),
          `Statistic` = paste0("r = ", round(unname(res$estimate), 3)),
          `p-value` = round(res$p.value, 5),
          `Conclusion` = if(res$p.value <= 0.05) "Reject H₀ (Correlated)" else "Fail to Reject H₀",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        details_str <- paste("Correlation coefficient r =", round(unname(res$estimate), 4))
      }
      
      # CHI-SQUARE TEST
      else if (input$test_type == "Chi-Square Test of Independence") {
        req(input$cat_x, input$cat_y)
        tbl <- table(df[[input$cat_x]], df[[input$cat_y]])
        res <- chisq.test(tbl, correct = input$correct_yates)
        
        new_row <- data.frame(
          Timestamp = format(Sys.time(), "%H:%M:%S"),
          `Test Name` = "Chi-Square Test",
          `Variables / Groups` = paste(input$cat_x, "&", input$cat_y),
          `Statistic` = paste0("Chi-Sq = ", round(res$statistic, 3)),
          `p-value` = round(res$p.value, 5),
          `Conclusion` = if(res$p.value <= 0.05) "Reject H₀ (Dependent)" else "Fail to Reject H₀",
          check.names = FALSE, stringsAsFactors = FALSE
        )
        details_str <- paste("Degrees of freedom df =", res$parameter)
      }
      
      # Append to existing history dataframe (Accumulative Behavior)
      if (!is.null(new_row)) {
        current_history <- shared_state$final_models[["Hypothesis Testing"]]
        
        if (is.null(current_history) || nrow(current_history) == 0) {
          updated_history <- new_row
        } else {
          updated_history <- rbind(current_history, new_row)
        }
        
        # Save accumulated table back to shared state
        shared_state$final_models[["Hypothesis Testing"]] <- updated_history
        latest_details(details_str)
        showNotification("Test added to accumulated summary table!", type = "message")
      }
    })
    
    # 3. Selective Deletion of Highlighted Row(s)
    observeEvent(input$delete_selected_test, {
      req(input$ht_table_rows_selected)
      current_history <- shared_state$final_models[["Hypothesis Testing"]]
      
      if (!is.null(current_history) && nrow(current_history) > 0) {
        selected_idx <- input$ht_table_rows_selected
        shared_state$final_models[["Hypothesis Testing"]] <- current_history[-selected_idx, , drop = FALSE]
        showNotification("Selected test(s) removed from history.", type = "warning")
      }
    })
    
    # 4. Clear All Tests Button
    observeEvent(input$clear_tests, {
      shared_state$final_models[["Hypothesis Testing"]] <- NULL
      latest_details("Table reset. All accumulated tests cleared.")
      showNotification("All hypothesis test records cleared.", type = "warning")
    })
    
    # Render DT Table with Multi-row Selection Enabled
    output$ht_table <- renderDT({
      req(shared_state$final_models[["Hypothesis Testing"]])
      datatable(
        shared_state$final_models[["Hypothesis Testing"]], 
        rownames = FALSE, 
        selection = "multiple",
        options = list(pageLength = 10, dom = 'ftip', scrollX = TRUE)
      )
    })
    
    output$ht_details <- renderText({ latest_details() })
    
    # PDF Summary Report Download Handler
    output$dl_pdf <- downloadHandler(
      filename = function() { paste0("Hypothesis_Tests_Summary_", Sys.Date(), ".pdf") },
      content = function(file) {
        req(shared_state$final_models[["Hypothesis Testing"]])
        
        tempReport <- file.path(tempdir(), "report.Rmd")
        
        rmd_content <- c(
          "---",
          "title: 'Hypothesis Testing Audit Summary'",
          paste0("date: '", Sys.Date(), "'"),
          "output: pdf_document",
          "---",
          "",
          "### Executed Test Results",
          "",
          "```{r echo=FALSE}",
          "knitr::kable(params$table_data)",
          "```"
        )
        
        writeLines(rmd_content, con = tempReport)
        
        rmarkdown::render(
          tempReport, 
          output_file = file,
          params = list(table_data = shared_state$final_models[["Hypothesis Testing"]]),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}