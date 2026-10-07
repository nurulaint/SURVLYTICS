helper_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("SURVLYTICS User & Analysis Guide"),
      accordion(
        accordion_panel(
          "1. Survey Builder & Deployment",
          "Create custom web surveys, configure parameters, and link responses to Google Sheets for live data collection."
        ),
        accordion_panel(
          "2. Data Wrangling & Descriptive Analysis",
          "Filter missing observations, view variable summaries (Mean, SD, frequencies), and plot distribution histograms."
        ),
        accordion_panel(
          "3. Normality Testing",
          "Assess data normality using Shapiro-Wilk, Kolmogorov-Smirnov, or Anderson-Darling tests to decide between parametric and non-parametric statistics."
        ),
        accordion_panel(
          "4. Hypothesis & Regression Modeling",
          "Fit Linear or Logistic Regression models based on variable types and view automated interpretations."
        ),
        accordion_panel(
          "5. Workflow Tracking & Report Export",
          "Track analysis milestones dynamically using the workflow engine and export compiled results directly into Word (.docx) documents."
        )
      )
    )
  )
}

helper_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    # Server logic for dynamic helper guides
  })
}