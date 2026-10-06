library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(ggplot2)
library(DT)
library(hunspell)
library(qrcode)
library(nortest)
library(DiagrammeR) # <-- MAKE SURE THIS IS LOADED HERE
library(rmarkdown)
library(tinytex)
library(knitr)
library(nnet)
library(officer)
library(flextable)

if (!rmarkdown::pandoc_available()) {
  p_info <- suppressWarnings(try(rmarkdown::find_pandoc(), silent = TRUE))
  if (is.list(p_info) && !is.null(p_info$dir)) {
    Sys.setenv(RSTUDIO_PANDOC = p_info$dir)
  }
}

# Source modules
source("R/survey_module.R")
source("R/analytics/01_summary_meta_module.R")
source("R/analytics/02_cleaning_module.R")
source("R/analytics/03_descriptive_module.R")
source("R/analytics/04_normality_module.R")
source("R/analytics/05_hypothesis_module.R")
source("R/analytics/06_linear_regression_module.R")
source("R/analytics/07_logistic_regression_module.R")
source("R/analytics/08_summary_report_module.R")

ui <- page_navbar(
  title = "SURVLYTICS",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("1. Survey Builder", survey_builder_ui("survey_mod")),
  nav_panel("2. Active Survey", survey_active_ui("survey_mod")),
  
  nav_menu("3. Analytics Engine",
    nav_panel("Metadata & Overview", summary_meta_ui("meta_mod")),
    nav_panel("Data Wrangling", cleaning_ui("clean_mod")),
    nav_panel("Descriptive & Visuals", descriptive_ui("desc_mod")),
    nav_panel("Normality Tests", normality_ui("norm_mod")), #
    nav_panel("Hypothesis Testing", hypothesis_ui("ht_mod")),
    nav_panel("Linear Regression", linear_regression_ui("lm_mod")),
    nav_panel("Logistic Regression", logistic_regression_ui("glm_mod")), 
    nav_panel("Final Summary & Workflow", summary_report_ui("sum_mod"))
  )
)


server <- function(input, output, session) {
  
  app_state <- reactiveValues(
    schema = data.frame(Variable = character(), Prompt = character(), Type = character(), Options = character(), stringsAsFactors = FALSE),
    sheet_id = NULL,
    data = iris,
   # data = data.frame()
   final_models = list() # Holds the single latest result per module
  )
  
  # Initialize Servers
  survey_server("survey_mod", app_state)
  summary_meta_server("meta_mod", app_state)
  cleaning_server("clean_mod", app_state)
  descriptive_server("desc_mod", app_state)
  normality_server("norm_mod", app_state)
  hypothesis_server("ht_mod", app_state)
  linear_regression_server("lm_mod", app_state)
  logistic_regression_server("glm_mod", app_state)
  summary_report_server("sum_mod", app_state)
}

shinyApp(ui, server)