fluidPage(
  theme = shinytheme("flatly"), title = "Simple Mortgage Refinance Dashboard",
  titlePanel("Simple Mortgage Refinance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h2("Original Mortgage"),
      currencyInput(
        "P_orig", "Principal",
        value = 400000, format = "dollar", align = "left"
      ),
      numericInput(
        "r_a_orig", "Interest Rate",
        value = 4.0,
        min = 0.5, max = 10, step = 0.001
      ),
      selectInput(
        "n_orig", "Length",
        choices = c("15 year", "30 year"),
        selected = "30 year", multiple = FALSE
      ),
      dateInput(
        "t0_orig", "Start Date",
        value = ymd("2019-07-01"), min = ymd("1990-01-01"), max = Sys.Date()
      ),
      hr(), h2("Refinance Terms"),
      numericInput(
        "r_a_refi", "Interest Rate",
        value = 3.3,
        min = 0.5, max = 10, step = 0.001
      ),
      selectInput(
        "n_refi", "Length",
        choices = c("15 year", "30 year"),
        selected = "30 year", multiple = FALSE
      ),
      dateInput(
        "t0_refi", "Start Date",
        value = ymd("2021-05-01"), min = ymd("1990-01-01"), max = ymd("2021-12-01")
      ),
      hr(),
      currencyInput(
        "close_cost", "Closing Costs",
        value = 5000, format = "dollar", align = "left"
      ),
      actionButton(
        "calc", "Calculate!"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(uiOutput("total_text")),
      fluidRow(plotlyOutput("total_plot")),
      fluidRow(uiOutput("monthly_text")),
      fluidRow(plotlyOutput("monthly_plot"))
      # fluidRow(verbatimTextOutput("debug"))
    )
  )
)