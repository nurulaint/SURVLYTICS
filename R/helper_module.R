library(shiny)
library(bslib)

helper_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("SURVLYTICS User Guide & Beginner Manual"),
    p(class = "text-muted", "Welcome to SURVLYTICS! Follow this simple step-by-step guide to build surveys, analyze datasets, and export complete Word reports without writing any code."),
    
    accordion(
      open = "Getting Started",
      
      # 1. Overview
      accordion_panel(
        "1. Getting Started & Recommended Workflow",
        icon = icon("play-circle"),
        markdown("
          ### Recommended Workflow
          To get a complete statistical report, follow these tabs in order:
          1. **Survey Builder / Active Survey:** Create a survey or load your active responses.
          2. **Metadata & Data Wrangling:** Inspect your variables and clean missing values.
          3. **Descriptive & Visuals:** Calculate averages, counts, and generate charts.
          4. **Normality Tests:** Check if numeric variables follow a normal distribution.
          5. **Hypothesis Testing:** Compare differences between groups.
          6. **Regression:** Identify drivers and predict outcomes.
          7. **Final Summary & Workflow:** Export everything into a Word document (`.docx`)!
        ")
      ),
      
      # 2. Descriptive Module Guide
      accordion_panel(
        "2. How to Use Descriptive & Visuals",
        icon = icon("chart-bar"),
        markdown("
          ### Steps:
          1. Go to **3. Analytics Engine > Descriptive & Visuals**.
          2. Use the **Left Sidebar** to select a column from your dataset.
          3. Pick a chart type (*Histogram, Boxplot, Density Plot, or Bar Chart*).
          4. Click **Save to Report** (in the left sidebar) to save these stats into your final Word document.
          5. Click **Download Metrics CSV** if you want an Excel/CSV copy of the metrics table.
        ")
      ),
      
      # 3. Normality Testing Guide
      accordion_panel(
        "3. How to Test for Normality",
        icon = icon("square-root-variable"),
        markdown("
          ### What is Normality?
          Normality testing tells you whether a numeric variable follows a bell curve distribution.
          
          ### Rules of Thumb:
          * **If p > 0.05:** The variable **is normal** (Use parametric tests like standard t-test or ANOVA).
          * **If p <= 0.05:** The variable **is not normal**.
          
          ### Steps:
          1. Select a numeric column.
          2. Select **Shapiro-Wilk** (best for small datasets) or **Kolmogorov-Smirnov**.
          3. Click **Run & Save to Report**.
        ")
      ),
      
      # 4. Hypothesis Testing Guide
      accordion_panel(
        "4. How to Compare Groups (Hypothesis Testing)",
        icon = icon("scale-balanced"),
        markdown("
          ### Selecting the Right Test:
          * **t-test:** Comparing a continuous variable across **exactly 2 groups** (e.g., Male vs Female).
          * **ANOVA:** Comparing a continuous variable across **3 or more groups** (e.g., Species, Regions, Departments).
          * **Chi-Square:** Comparing two categorical variables (e.g., Gender vs Preferred Product Choice).
          
          ### Steps:
          1. Choose your categorical/grouping variable.
          2. Choose your numeric outcome variable.
          3. Select **ANOVA** or **t-test** and click **Run & Save to Report**.
        ")
      ),
      
      # 5. Regression Guide
      accordion_panel(
        "5. How to Run Regression Analysis",
        icon = icon("chart-line"),
        markdown("
          ### Choosing the Correct Regression:
          * **Linear Regression:** Use when your outcome 𝒀 is a **numeric number** (e.g., Salary, Height, Sales).
          * **Logistic Regression:** Use when your outcome 𝒀 is **binary / two categories** (e.g., Yes/No, Pass/Fail, Churn/Keep).
          
          ### Steps:
          1. Select your target variable 𝒀.
          2. Select one or more predictor variables 𝒀.
          3. Click **Fit Model & Save**.
        ")
      ),
      
      # 6. Exporting Guide
      accordion_panel(
        "6. Exporting Your Final Report",
        icon = icon("file-word"),
        markdown("
          ### Steps:
          1. Click **Final Summary & Workflow**.
          2. Verify that the workflow diagram nodes have turned **green**.
          3. Click **Export Professional Word Report (.docx)** to download your final report!
        ")
      )
    )
  )
}

helper_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    # Server logic for helper module if dynamic content is needed in the future
  })
}