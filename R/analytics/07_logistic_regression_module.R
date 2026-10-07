# R/analytics/06_logistic_regression_module.R

logistic_regression_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Generalized Logistic Regression"),
      selectInput(ns("target_y"), "Dependent Variable Y (Categorical):", choices = NULL),
      selectizeInput(ns("predictors_x"), "Independent Variable(s) X:", choices = NULL, multiple = TRUE),
      hr(),
      actionButton(ns("run_glm"), "Fit Model", class = "btn-primary w-100"),
      hr(),
      actionButton(ns("save_glm_report"), "Save Models to Report", icon = icon("check"), class = "btn-outline-success w-100")
    ),
    div(
      card(
        card_header("1. Simple Logistic Regressions (Individual Predictors)"),
        DTOutput(ns("glm_simple_table"))
      ),
      uiOutput(ns("multi_model_ui"))
    )
  )
}

logistic_regression_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Restrict Dependent Variable Y strictly to Categorical/Factor columns
    observe({
      req(shared_state$data)
      df <- shared_state$data
      
      cat_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]
      all_cols <- names(df)
      
      updateSelectInput(session, "target_y", choices = cat_cols)
      updateSelectizeInput(session, "predictors_x", choices = all_cols)
    })
    
    # Helper Engine to run and format either Binary or Multinomial Logistic Models
    process_logistic_model <- function(f_str, sub_df, num_levels, y_factor, scope_label) {
      if (num_levels == 2) {
        fit_glm <- glm(as.formula(f_str), data = sub_df, family = binomial(link = "logit"))
        s_glm <- summary(fit_glm)
        
        coef_mat <- s_glm$coefficients
        or_val <- exp(coef_mat[, 1])
        ci_val <- suppressMessages(exp(confint.default(fit_glm)))
        
        if (is.null(dim(ci_val))) {
          ci_val <- matrix(ci_val, ncol = 2, dimnames = list(names(coef_mat[,1]), c("2.5 %", "97.5 %")))
        }
        
        df_res <- as.data.frame(coef_mat)
        df_res$Term <- rownames(df_res)                 
        df_res$`Model Scope` <- scope_label
        df_res$`Target Level` <- paste0(levels(y_factor)[2], " (vs ", levels(y_factor)[1], ")")
        df_res$`Odds Ratio (OR)` <- round(or_val, 4)
        df_res$`2.5 % CI` <- round(ci_val[, 1], 4)
        df_res$`97.5 % CI` <- round(ci_val[, 2], 4)
        
        colnames(df_res)[1:4] <- c("Estimate (Log-Odds)", "Std. Error", "Statistic", "p-value")
        
        # Safeguard against NA p-values
        df_res$`p-value`[is.na(df_res$`p-value`)] <- 1
        df_res$`Significance` <- ifelse(df_res$`p-value` <= 0.001, "***",
                                 ifelse(df_res$`p-value` <= 0.01, "**",
                                 ifelse(df_res$`p-value` <= 0.05, "*", "NS")))
        
        final_df <- df_res[, c("Model Scope", "Target Level", "Term", "Estimate (Log-Odds)", 
                               "Odds Ratio (OR)", "2.5 % CI", "97.5 % CI", "Statistic", "p-value", "Significance")]
        
        final_df$`Estimate (Log-Odds)` <- round(final_df$`Estimate (Log-Odds)`, 4)
        final_df$Statistic <- round(final_df$Statistic, 3)                 
        final_df$`p-value` <- round(final_df$`p-value`, 5)
        
        return(list(df = final_df, deviance = fit_glm$deviance, aic = fit_glm$aic))
      } 
      else {
        # Multinomial Logistic Regression
        fit_multi <- suppressMessages(nnet::multinom(as.formula(f_str), data = sub_df, trace = FALSE))
        s_multi <- summary(fit_multi)
        
        coeff_mat <- s_multi$coefficients
        se_mat <- s_multi$standard.errors
        
        # Ensure matrices even for single predictors
        if (is.null(dim(coeff_mat))) {
          coeff_mat <- t(as.matrix(coeff_mat))
          se_mat <- t(as.matrix(se_mat))
          rownames(coeff_mat) <- rownames(se_mat) <- s_multi$lab[2:length(s_multi$lab)]
        }
        
        z_mat <- coeff_mat / se_mat
        p_mat <- (1 - pnorm(abs(z_mat), 0, 1)) * 2
        or_mat <- exp(coeff_mat)
        
        res_rows <- list()
        target_levels <- rownames(coeff_mat)
        
        for (lvl in target_levels) {
          terms <- colnames(coeff_mat)
          sub_res <- data.frame(
            `Model Scope` = scope_label,
            `Target Level` = paste0(lvl, " (vs ", levels(y_factor)[1], ")"),
            Term = terms,
            `Estimate (Log-Odds)` = round(coeff_mat[lvl, ], 4),
            `Odds Ratio (OR)` = round(or_mat[lvl, ], 4),
            `2.5 % CI` = round(exp(coeff_mat[lvl, ] - 1.96 * se_mat[lvl, ]), 4),
            `97.5 % CI` = round(exp(coeff_mat[lvl, ] + 1.96 * se_mat[lvl, ]), 4),
            Statistic = round(z_mat[lvl, ], 3),
            `p-value` = round(p_mat[lvl, ], 5),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          
          sub_res$`p-value`[is.na(sub_res$`p-value`)] <- 1
          sub_res$Significance <- ifelse(sub_res$`p-value` <= 0.001, "***",
                                  ifelse(sub_res$`p-value` <= 0.01, "**",
                                  ifelse(sub_res$`p-value` <= 0.05, "*", "NS")))
          
          res_rows[[lvl]] <- sub_res
        }
        
        final_df <- do.call(rbind, res_rows)
        return(list(df = final_df, deviance = fit_multi$deviance, aic = fit_multi$AIC))
      }
    }

    # 2. Store Fit Results (Separated into Simple and Multi)
    glm_results <- eventReactive(input$run_glm, {
      req(shared_state$data, input$target_y, input$predictors_x)
      df <- shared_state$data
      
      cols_needed <- c(input$target_y, input$predictors_x)
      sub_df <- na.omit(df[, cols_needed, drop = FALSE])
      
      y_factor <- as.factor(sub_df[[input$target_y]])
      num_levels <- length(levels(y_factor))
      
      if (num_levels < 2) {
        showNotification("Target variable Y must have at least 2 unique levels.", type = "error")
        return(NULL)
      }
      
      simple_results <- data.frame()
      
      # A. Always run Simple Logistic Regression for EACH predictor individually
      for (pred in input$predictors_x) {
        f_ind <- paste0("as.factor(`", input$target_y, "`) ~ `", pred, "`")
        res_ind <- process_logistic_model(f_ind, sub_df, num_levels, y_factor, paste("Individual:", pred))
        simple_results <- rbind(simple_results, res_ind$df)
      }
      
      multi_results <- NULL
      details_str <- NULL
      
      # B. Run Combined Multiple Logistic Regression (Only if > 1 predictor)
      if (length(input$predictors_x) > 1) {
        safe_preds <- paste0("`", input$predictors_x, "`", collapse = " + ")
        f_multi <- paste0("as.factor(`", input$target_y, "`) ~ ", safe_preds)
        
        res_multi <- process_logistic_model(f_multi, sub_df, num_levels, y_factor, "Combined Multiple Model")
        multi_results <- res_multi$df
        
        model_type_label <- if (num_levels == 2) "Binary Logistic Regression" else "Multinomial Logistic Regression"
        details_str <- paste0("Model Type: ", model_type_label, "\n",
                              if(num_levels > 2) paste0("Outcome Categories (", num_levels, "): ", paste(levels(y_factor), collapse = ", "), "\n") else "",
                              "Baseline Reference Level: '", levels(y_factor)[1], "'\n",
                              "Residual Deviance: ", round(res_multi$deviance, 2), 
                              " | AIC: ", round(res_multi$aic, 2))
      }
      
      # Cache for Final Summary Audit Trail
      current_models <- shared_state$final_models
      if (is.null(current_models)) current_models <- list()
      
      current_models[["Logistic Regression"]] <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module    = "Logistic Regression",
        Action    = if (num_levels == 2) "Binary Logistic Regression" else "Multinomial Logistic Regression",
        Details   = paste("Y:", input$target_y, "(", num_levels, "levels) ~ X:", paste(input$predictors_x, collapse = ", ")),
        stringsAsFactors = FALSE
      )
      shared_state$final_models <- current_models
      
      list(simple_table = simple_results, multi_table = multi_results, details = details_str)
    })
    
    # 3. Render Outputs independently
    output$glm_simple_table <- renderDT({
      req(glm_results()$simple_table)
      datatable(glm_results()$simple_table, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$multi_model_ui <- renderUI({
      req(glm_results()$multi_table)
      card(
        card_header("2. Multiple Logistic Regression (All Predictors Combined)"),
        DTOutput(ns("glm_multi_table")),
        hr(),
        verbatimTextOutput(ns("glm_details"))
      )
    })
    
    output$glm_multi_table <- renderDT({
      req(glm_results()$multi_table)
      datatable(glm_results()$multi_table, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$glm_details <- renderText({ 
      req(glm_results()$details)
      glm_results()$details 
    })

    # 4. Save to Word Report Logic
    observeEvent(input$save_glm_report, {
      req(glm_results()$simple_table)
      
      # Initialize safely if NULL
      if (is.null(shared_state$report_items)) {
        shared_state$report_items <- list()
      }
      
      # Save simple results
      new_simple <- list(
        type = "table", 
        title = paste("Simple Logistic Regressions for", input$target_y), 
        data = glm_results()$simple_table
      )
      shared_state$report_items <- append(shared_state$report_items, list(new_simple))
      
      # Save multi results if they exist
      if (!is.null(glm_results()$multi_table)) {
        new_multi <- list(
          type = "table", 
          title = paste("Multiple Logistic Regression for", input$target_y), 
          data = glm_results()$multi_table
        )
        shared_state$report_items <- append(shared_state$report_items, list(new_multi))
      }
      
      showNotification("Logistic regression results added to Word report!", type = "message")
    })
    
  })
}