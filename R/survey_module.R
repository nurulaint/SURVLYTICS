# R/survey_module.R

survey_builder_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      h4("1. Define Questions"),
      textInput(ns("var_name"), "Variable Name (e.g., BLOOD_PRESSURE):"),
      textInput(ns("q_prompt"), "Question Prompt:"),
      selectInput(ns("q_type"), "Input Type:", 
                  choices = c("Numeric Input", "1-10 Scale", "Text Input", "MCQ", "Dropdown")),
      
      uiOutput(ns("conditional_options")),
      
      actionButton(ns("add_q"), "Add Question", class = "btn-primary"),
      hr(),
      h4("2. Link Database"),
      textInput(ns("sheet_url"), "Google Sheet URL:"),
      actionButton(ns("init_sheet"), "Initialize Sheet Headers", class = "btn-warning"),
      hr(),
      h4("3. Share Survey"),
      textInput(ns("app_url"), "Deployed App URL (for QR):", value = "https://your-name.shinyapps.io/OmniForm/"),
      plotOutput(ns("qr_code"), width = "100%", height = "200px")
    ),
    card(
      card_header("Current Survey Schema"),
      DTOutput(ns("schema_table")),
      actionButton(ns("delete_q"), "Delete Selected Question", class = "btn-danger mt-2"),
      verbatimTextOutput(ns("builder_status"))
    )
  )
}

survey_active_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Respondent Form"),
    uiOutput(ns("dynamic_form")),
    hr(),
    actionButton(ns("submit_response"), "Submit Response", class = "btn-success")
  )
}

survey_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 0. QR CODE GENERATOR ---
    output$qr_code <- renderPlot({
      req(input$app_url)
      plot(qrcode::qr_code(input$app_url))
    })
    
    # --- 1. SCHEMA GENERATION ---
    output$conditional_options <- renderUI({
      req(input$q_type)
      if (input$q_type %in% c("MCQ", "Dropdown")) {
        textInput(ns("q_options"), "Options (comma-separated):", placeholder = "MALE, FEMALE")
      }
    })
    
    observeEvent(input$add_q, {
      req(input$var_name, input$q_prompt)
      
      # REQUIREMENT 1 & 2: Remove spaces and convert to uppercase
      clean_var <- trimws(input$var_name)
      clean_var <- toupper(gsub("\\s+", "_", clean_var))
      clean_prompt <- toupper(input$q_prompt)
      
      # REQUIREMENT 3: English Spell Check Warning
      bad_words <- hunspell::hunspell(clean_prompt)[[1]]
      if (length(bad_words) > 0) {
        showNotification(paste("Typo detected in prompt:", paste(bad_words, collapse = ", ")), type = "warning", duration = 8)
      }
      
      opts <- ""
      if (input$q_type %in% c("MCQ", "Dropdown")) {
        req(input$q_options)
        # Convert options to uppercase as well
        opts <- toupper(input$q_options)
      }
      
      new_q <- data.frame(
        Variable = clean_var, 
        Prompt = clean_prompt, 
        Type = input$q_type, 
        Options = opts,
        stringsAsFactors = FALSE
      )
      
      shared_state$schema <- rbind(shared_state$schema, new_q)
      
      updateTextInput(session, "var_name", value = "")
      updateTextInput(session, "q_prompt", value = "")
      if (input$q_type %in% c("MCQ", "Dropdown")) {
        updateTextInput(session, "q_options", value = "")
      }
    })
    
    output$schema_table <- renderDT({ 
      datatable(shared_state$schema, options = list(dom = 't')) 
    })
  
    # to delete question
    observeEvent(input$delete_q, {
      req(input$schema_table_rows_selected) 
      selected_rows <- input$schema_table_rows_selected
      shared_state$schema <- shared_state$schema[-selected_rows, ]
    })

    # --- 2. CLOUD DATABASE INITIALIZATION ---
    observeEvent(input$init_sheet, {
      req(input$sheet_url)
      if (nrow(shared_state$schema) == 0) {
        showNotification("Please add at least one question first!", type = "warning")
        return()
      }
      tryCatch({
        shared_state$sheet_id <- googlesheets4::as_sheets_id(input$sheet_url)
        col_names <- c("Timestamp", shared_state$schema$Variable)
        
        dummy_df <- data.frame(matrix(ncol = length(col_names), nrow = 1))
        colnames(dummy_df) <- col_names
        dummy_df[1, ] <- "SETUP_ROW"
        
        googlesheets4::sheet_write(dummy_df, ss = shared_state$sheet_id, sheet = 1)
        output$builder_status <- renderText("Sheet Initialized! (You can delete the SETUP_ROW in Google Sheets)")
      }, error = function(e) {
        output$builder_status <- renderText(paste("Error:", e$message))
      })
    })
    
    # --- 3. DYNAMIC FORM GENERATION ---
    output$dynamic_form <- renderUI({
      req(nrow(shared_state$schema) > 0)
      
      lapply(1:nrow(shared_state$schema), function(i) {
        var <- shared_state$schema$Variable[i]
        prompt <- shared_state$schema$Prompt[i]
        type <- shared_state$schema$Type[i]
        
        opts <- if (shared_state$schema$Options[i] != "") {
          trimws(unlist(strsplit(shared_state$schema$Options[i], ",")))
        } else { NULL }
        
        if (type == "Numeric Input") {
          # REQUIREMENT 5: step = 0.01 allows decimal inputs
          numericInput(inputId = ns(var), label = prompt, value = NA, step = 0.01)
        } else if (type == "1-10 Scale") {
          sliderInput(inputId = ns(var), label = prompt, min = 1, max = 10, value = 5)
        } else if (type == "Text Input") {
          textInput(inputId = ns(var), label = prompt, value = "")
        } else if (type == "MCQ") {
          radioButtons(inputId = ns(var), label = prompt, choices = opts)
        } else if (type == "Dropdown") {
          selectInput(inputId = ns(var), label = prompt, choices = opts)
        }
      })
    })
    
    # --- 4. SUBMITTING RESPONSES ---
    observeEvent(input$submit_response, {
      req(shared_state$sheet_id, nrow(shared_state$schema) > 0)
      
      response_list <- list(Timestamp = as.character(Sys.time()))
      
      # Validation Loop
      for (i in 1:nrow(shared_state$schema)) {
        var <- shared_state$schema$Variable[i]
        type <- shared_state$schema$Type[i]
        val <- input[[var]]
        
        if (type == "Text Input" && !is.null(val) && val != "") {
          # REQUIREMENT 4: Check if they typed only numbers into a text field
          if (grepl("^[0-9.]+$", val)) {
            showNotification(paste("Error: '", var, "' requires text, but a number was entered."), type = "error")
            return() # Instantly halts the submission process
          }
          # REQUIREMENT 2: Force respondent text answer to uppercase
          val <- toupper(val)
        }
        
        response_list[[var]] <- if (is.null(val) || val == "") NA else val
      }
      
      new_data <- as.data.frame(response_list)
      
      # Send to Google Sheets
      tryCatch({
        googlesheets4::sheet_append(shared_state$sheet_id, new_data)
        showNotification("Data submitted!", type = "message")
        
        # Form Reset
        for (i in 1:nrow(shared_state$schema)) {
          var <- shared_state$schema$Variable[i]
          type <- shared_state$schema$Type[i]
          
          if (type == "Numeric Input") {
            updateNumericInput(session, var, value = NA)
          } else if (type == "1-10 Scale") {
            updateSliderInput(session, var, value = 5)
          } else if (type == "Text Input") {
            updateTextInput(session, var, value = "")
          } else if (type %in% c("MCQ", "Dropdown")) {
            opts <- trimws(unlist(strsplit(shared_state$schema$Options[i], ",")))
            if (type == "MCQ") updateRadioButtons(session, var, selected = character(0))
            if (type == "Dropdown") updateSelectInput(session, var, selected = opts[1])
          }
        }
      }, error = function(e) {
        showNotification(paste("Submit error:", e$message), type = "error")
      })
    })
  })
}