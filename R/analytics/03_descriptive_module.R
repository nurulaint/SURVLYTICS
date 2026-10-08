descriptive_ui <- function(id) {
  ns <- NS(id)
  
  # Global Layout Sidebar for the entire module
  layout_sidebar(
    sidebar = sidebar(
      h4("Descriptive Stats"),
      # This dynamically changes based on the active tab!
      uiOutput(ns("dynamic_sidebar"))
    ),
    
    # Give the tabset an ID so the server knows which tab is active
    navset_card_tab(
      id = ns("desc_tabs"), 
      
      # TAB 1: NUMERICAL SUMMARY TABLE
      nav_panel("Numerical Summary", value = "num_tab",
        card(
          card_header("Summary Statistics for Continuous Variables"),
          DTOutput(ns("num_summary_table"))
        )
      ),
      
      # TAB 2: CATEGORICAL SUMMARY TABLE
      nav_panel("Frequency Tables", value = "cat_tab",
        card(
          card_header("Frequency Distributions for Categorical Variables"),
          DTOutput(ns("cat_summary_table"))
        )
      ),
      
      # TAB 3: SMART VISUALIZATIONS ENGINE
      nav_panel("Visualizations", value = "viz_tab",
        card(
          card_header("Dynamic Visual Output"),
          plotOutput(ns("main_plot"), height = "520px")
        )
      )
    )
  )
}

descriptive_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ==========================================
    # 0. DYNAMIC GLOBAL SIDEBAR
    # ==========================================
    output$dynamic_sidebar <- renderUI({
      req(input$desc_tabs)
      
      if (input$desc_tabs == "num_tab") {
        tagList(
          p("Generate and export numerical summaries for continuous variables."),
          hr(),
          actionButton(ns("save_num_report"), "Save Table to Report", icon = icon("bookmark"), class = "btn-success w-100 mb-2"),
          downloadButton(ns("dl_num_stats"), "Download Numerical CSV", class = "btn-outline-primary btn-sm w-100")
        )
      } else if (input$desc_tabs == "cat_tab") {
        tagList(
          p("Generate and export frequency distributions for categorical variables."),
          hr(),
          actionButton(ns("save_cat_report"), "Save Table to Report", icon = icon("bookmark"), class = "btn-success w-100 mb-2"),
          downloadButton(ns("dl_cat_stats"), "Download Categorical CSV", class = "btn-outline-primary btn-sm w-100")
        )
      } else if (input$desc_tabs == "viz_tab") {
        tagList(
          selectInput(ns("analysis_dim"), "1. Select Analysis Dimension:", 
                      choices = c("Univariate Analysis", "Bivariate / Multivariate Analysis")),
          uiOutput(ns("dynamic_variable_controls")),
          uiOutput(ns("dynamic_plot_type_ui")),
          uiOutput(ns("dynamic_param_controls")),
          hr(),
          actionButton(ns("save_plot_report"), "Save Plot to Report", icon = icon("bookmark"), class = "btn-success w-100 mb-2"),
          downloadButton(ns("dl_plot"), "Download High-Res Plot (PNG)", class = "btn-outline-primary btn-sm w-100")
        )
      }
    })
    
    # ==========================================
    # 1. NUMERICAL SUMMARY LOGIC
    # ==========================================
    num_summary_df <- reactive({
      req(shared_state$data)
      df <- shared_state$data
      num_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
      
      if (ncol(num_df) == 0) return(NULL)
      
      data.frame(
        Variable = names(num_df),
        Mean   = round(vapply(num_df, mean, numeric(1), na.rm = TRUE), 3),
        SD     = round(vapply(num_df, sd, numeric(1), na.rm = TRUE), 3),
        Median = round(vapply(num_df, median, numeric(1), na.rm = TRUE), 3),
        IQR    = round(vapply(num_df, IQR, numeric(1), na.rm = TRUE), 3),
        Min    = round(vapply(num_df, min, numeric(1), na.rm = TRUE), 3),
        Max    = round(vapply(num_df, max, numeric(1), na.rm = TRUE), 3),
        check.names = FALSE
      )
    })
    
    output$num_summary_table <- renderDT({
      df <- num_summary_df()
      req(df)
      datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = 't', scrollX = TRUE))
    })
    
    output$dl_num_stats <- downloadHandler(
      filename = function() { paste0("numerical_summary_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(num_summary_df(), file, row.names = FALSE) }
    )
    
    observeEvent(input$save_num_report, {
      req(num_summary_df())
      new_item <- list(
        type = "table",
        title = "Numerical Summary Statistics",
        data = num_summary_df()
      )
      shared_state$report_items <- append(shared_state$report_items, list(new_item))
      showNotification("Numerical Summary added to Word report!", type = "message")
    })
    
    # ==========================================
    # 2. CATEGORICAL SUMMARY LOGIC
    # ==========================================
    cat_summary_df <- reactive({
      req(shared_state$data)
      df <- shared_state$data
      cat_cols <- names(df)[vapply(df, function(c) is.character(c) || is.factor(c), logical(1))]
      
      if (length(cat_cols) == 0) return(NULL)
      
      res_list <- lapply(cat_cols, function(col) {
        tbl <- table(df[[col]], useNA = "ifany")
        prop <- prop.table(tbl) * 100
        data.frame(
          Variable   = col,
          Category   = names(tbl),
          Count      = as.numeric(tbl),
          Percentage = round(as.numeric(prop), 2),
          stringsAsFactors = FALSE
        )
      })
      
      do.call(rbind, res_list)
    })
    
    output$cat_summary_table <- renderDT({
      df <- cat_summary_df()
      req(df)
      datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = 'ftip'))
    })
    
    output$dl_cat_stats <- downloadHandler(
      filename = function() { paste0("categorical_summary_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(cat_summary_df(), file, row.names = FALSE) }
    )
    
    observeEvent(input$save_cat_report, {
      req(cat_summary_df())
      new_item <- list(
        type = "table",
        title = "Categorical Frequency Distributions",
        data = cat_summary_df()
      )
      shared_state$report_items <- append(shared_state$report_items, list(new_item))
      showNotification("Categorical Summary added to Word report!", type = "message")
    })
    
    # ==========================================
    # 3. DYNAMIC SMART VISUALIZATION ENGINE
    # ==========================================
    output$dynamic_variable_controls <- renderUI({
      req(shared_state$data, input$analysis_dim)
      df <- shared_state$data
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      cat_cols <- names(df)[vapply(df, function(c) is.character(c) || is.factor(c), logical(1))]
      all_cols <- names(df)
      
      if (input$analysis_dim == "Univariate Analysis") {
        tagList(
          selectInput(ns("uni_var"), "2. Select Target Variable:", choices = all_cols),
          selectInput(ns("uni_group"), "Faceting Group (Optional Categorical):", choices = c("None", cat_cols))
        )
      } else {
        tagList(
          selectInput(ns("biv_type"), "2. Relationship Type:", 
                      choices = c("Continuous vs Continuous", "Continuous vs Categorical", "Categorical vs Categorical", "Multi-Variable Matrix")),
          uiOutput(ns("biv_variable_inputs"))
        )
      }
    })
    
    output$biv_variable_inputs <- renderUI({
      req(input$biv_type, shared_state$data)
      df <- shared_state$data
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      cat_cols <- names(df)[vapply(df, function(c) is.character(c) || is.factor(c), logical(1))]
      
      if (input$biv_type == "Continuous vs Continuous") {
        tagList(
          selectInput(ns("biv_x_num"), "Variable X (Continuous):", choices = num_cols),
          selectInput(ns("biv_y_num"), "Variable Y (Continuous):", choices = num_cols, selected = num_cols[min(2, length(num_cols))])
        )
      } else if (input$biv_type == "Continuous vs Categorical") {
        tagList(
          selectInput(ns("biv_num"), "Continuous Variable (Y):", choices = num_cols),
          selectInput(ns("biv_cat"), "Categorical Grouping (X):", choices = cat_cols)
        )
      } else if (input$biv_type == "Categorical vs Categorical") {
        tagList(
          selectInput(ns("biv_cat1"), "Primary Variable (X):", choices = cat_cols),
          selectInput(ns("biv_cat2"), "Secondary Grouping (Fill):", choices = cat_cols)
        )
      } else if (input$biv_type == "Multi-Variable Matrix") {
        tagList(
          selectizeInput(ns("multi_vars"), "Select Continuous Variables (2+):", 
                         choices = num_cols, selected = num_cols[1:min(4, length(num_cols))], multiple = TRUE)
        )
      }
    })
    
    output$dynamic_plot_type_ui <- renderUI({
      req(shared_state$data, input$analysis_dim)
      df <- shared_state$data
      
      if (input$analysis_dim == "Univariate Analysis") {
        req(input$uni_var)
        x_val <- df[[input$uni_var]]
        if (is.numeric(x_val)) {
          allowed_plots <- c("Histogram", "Density Plot (KDE)")
        } else {
          allowed_plots <- c("Bar Chart")
        }
      } else {
        req(input$biv_type)
        if (input$biv_type == "Continuous vs Continuous") allowed_plots <- c("Scatter Plot")
        else if (input$biv_type == "Continuous vs Categorical") allowed_plots <- c("Grouped Boxplot")
        else if (input$biv_type == "Categorical vs Categorical") allowed_plots <- c("Grouped Bar Chart", "Stacked Bar Chart")
        else allowed_plots <- c("Multi-Scatter Pair Plot", "Correlation Heatmap")
      }
      
      selectInput(ns("plot_type"), "3. Select Graph Type:", choices = allowed_plots)
    })
    
    output$dynamic_param_controls <- renderUI({
      req(input$plot_type, shared_state$data)
      df <- shared_state$data
      
      if (input$plot_type == "Histogram") {
        req(input$uni_var)
        x_vec <- na.omit(df[[input$uni_var]])
        if (is.numeric(x_vec)) {
          rng <- diff(range(x_vec))
          init_bin <- if(rng > 0) round(rng / 25, 2) else 0.5
          sliderInput(ns("bin_width"), "Histogram Bin Width:", 
                      min = round(rng / 100, 2) + 0.01, max = round(rng / 5, 2) + 0.1, 
                      value = init_bin, step = 0.01)
        }
      } else if (input$plot_type == "Density Plot (KDE)") {
        sliderInput(ns("bw_adjust"), "KDE Bandwidth Adjuster:", min = 0.1, max = 3.0, value = 1.0, step = 0.1)
      }
    })
    
    plot_obj <- reactive({
      req(shared_state$data, input$plot_type)
      df <- shared_state$data
      
      if (input$plot_type == "Histogram") {
        req(input$uni_var)
        bw <- if (!is.null(input$bin_width)) input$bin_width else NULL
        p <- ggplot(df, aes(x = .data[[input$uni_var]])) + 
          geom_histogram(binwidth = bw, fill = "#007bff", color = "white") + 
          theme_minimal(base_size = 14) + 
          labs(title = paste("Histogram of", input$uni_var), x = input$uni_var, y = "Frequency")
        if (!is.null(input$uni_group) && input$uni_group != "None") p <- p + facet_wrap(vars(.data[[input$uni_group]]))
        return(p)
          
      } else if (input$plot_type == "Density Plot (KDE)") {
        req(input$uni_var)
        adj <- if (!is.null(input$bw_adjust)) input$bw_adjust else 1.0
        p <- ggplot(df, aes(x = .data[[input$uni_var]])) + 
          geom_density(adjust = adj, fill = "#17a2b8", color = "#0f6674", alpha = 0.6) + 
          theme_minimal(base_size = 14) + 
          labs(title = paste("Density Plot (KDE) of", input$uni_var), x = input$uni_var, y = "Density")
        if (!is.null(input$uni_group) && input$uni_group != "None") p <- p + facet_wrap(vars(.data[[input$uni_group]]))
        return(p)
        
      } else if (input$plot_type == "Bar Chart") {
        req(input$uni_var)
        p <- ggplot(df, aes(x = as.factor(.data[[input$uni_var]]))) + 
          geom_bar(fill = "#6f42c1") + 
          theme_minimal(base_size = 14) + 
          labs(title = paste("Bar Chart of", input$uni_var), x = input$uni_var, y = "Count")
        if (!is.null(input$uni_group) && input$uni_group != "None") p <- p + facet_wrap(vars(.data[[input$uni_group]]))
        return(p)
        
      } else if (input$plot_type == "Scatter Plot") {
        req(input$biv_x_num, input$biv_y_num)
        return(ggplot(df, aes(x = .data[[input$biv_x_num]], y = .data[[input$biv_y_num]])) + 
          geom_point(color = "#007bff", alpha = 0.7, size = 2.5) + 
          geom_smooth(method = "lm", col = "red", se = FALSE) + 
          theme_minimal(base_size = 14) + 
          labs(title = paste("Scatter Plot:", input$biv_x_num, "vs", input$biv_y_num)))
        
      } else if (input$plot_type == "Grouped Boxplot") {
        req(input$biv_num, input$biv_cat)
        return(ggplot(df, aes(x = as.factor(.data[[input$biv_cat]]), y = .data[[input$biv_num]], fill = as.factor(.data[[input$biv_cat]]))) + 
          geom_boxplot() + 
          theme_minimal(base_size = 14) + 
          labs(title = paste("Boxplot of", input$biv_num, "by", input$biv_cat), x = input$biv_cat, y = input$biv_num, fill = input$biv_cat))
        
      } else if (input$plot_type %in% c("Grouped Bar Chart", "Stacked Bar Chart")) {
        req(input$biv_cat1, input$biv_cat2)
        pos_val <- if(input$plot_type == "Grouped Bar Chart") "dodge" else "fill"
        return(ggplot(df, aes(x = as.factor(.data[[input$biv_cat1]]), fill = as.factor(.data[[input$biv_cat2]]))) + 
          geom_bar(position = pos_val) + 
          theme_minimal(base_size = 14) + 
          labs(title = paste(input$plot_type, ":", input$biv_cat1, "and", input$biv_cat2), x = input$biv_cat1, fill = input$biv_cat2))
        
      } else if (input$plot_type == "Correlation Heatmap") {
        req(input$multi_vars, length(input$multi_vars) >= 2)
        sub_df <- na.omit(df[, input$multi_vars, drop = FALSE])
        cor_mat <- round(cor(sub_df), 2)
        cor_df <- as.data.frame(as.table(cor_mat))
        return(ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
          geom_tile(color = "white") +
          scale_fill_gradient2(low = "#dc3545", mid = "#ffffff", high = "#007bff", midpoint = 0, limit = c(-1, 1)) +
          geom_text(aes(label = Freq), color = "black", size = 4) +
          theme_minimal(base_size = 14) +
          labs(title = "Correlation Heatmap Matrix", x = "", y = "", fill = "Corr") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))
      }
    })
    
    output$main_plot <- renderPlot({
      if (input$plot_type == "Multi-Scatter Pair Plot") {
        req(input$multi_vars, length(input$multi_vars) >= 2)
        sub_df <- na.omit(shared_state$data[, input$multi_vars, drop = FALSE])
        pairs(sub_df, col = "#007bff", main = "Multi-Scatter Matrix")
      } else {
        p <- plot_obj()
        if (is.ggplot(p)) print(p)
      }
    })
    
    observeEvent(input$save_plot_report, {
      req(plot_obj())
      new_item <- list(
        type = "plot",
        title = paste("Descriptive Plot:", input$plot_type),
        plot = plot_obj() # Use the generated ggplot object
      )
      shared_state$report_items <- append(shared_state$report_items, list(new_item))
      showNotification(paste(input$plot_type, "added to Word report!"), type = "message")
    })
    
    output$dl_plot <- downloadHandler(
      filename = function() { paste0("plot_", tolower(gsub(" ", "_", input$plot_type)), ".png") },
      content = function(file) {
        png(file, width = 900, height = 600, res = 100)
        if (input$plot_type == "Multi-Scatter Pair Plot") {
          sub_df <- na.omit(shared_state$data[, input$multi_vars, drop = FALSE])
          pairs(sub_df, col = "#007bff", main = "Multi-Scatter Matrix")
        } else {
          print(plot_obj())
        }
        dev.off()
      }
    )
  })
}