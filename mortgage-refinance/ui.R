fluidPage(
  theme = shinytheme("flatly"), title = "Simple Mortgage Refinance Dashboard",
  titlePanel("Simple Mortgage Refinance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h2("Original Mortgage"),
      currencyInput(
        "P_orig", "Principal",
        value = 625000, format = "dollar", align = "left"
      ),
      numericInput(
        "r_a_orig", "Interest Rate",
        value = 3.99,
        min = 0.5, max = 10, step = 0.001
      ),
      selectInput(
        "n_orig", "Length",
        choices = c("15 year", "30 year"),
        selected = "30 year", multiple = FALSE
      ),
      dateInput(
        "t0_orig", "Start Date",
        value = ymd("2019-08-01"), min = ymd("1990-01-01"), max = Sys.Date()
      ),
      hr(), h2("Refinance Terms"),
      numericInput(
        "r_a_refi", "Interest Rate",
        value = 2.59,
        min = 0.5, max = 10, step = 0.001
      ),
      selectInput(
        "n_refi", "Length",
        choices = c("15 year", "30 year"),
        selected = "15 year", multiple = FALSE
      ),
      dateInput(
        "t0_refi", "Start Date",
        value = ymd("2021-06-01"), min = ymd("1990-01-01"), max = ymd("2021-12-01")
      ),
      hr(),
      currencyInput(
        "close_cost", "Closing Costs",
        value = 3200, format = "dollar", align = "left"
      ),
      radioButtons(
        "points_yn", "Points?",
        choices = c("Yes", "No"),
        selected = "No"
      ),
      conditionalPanel(
        condition = "input.points_yn == 'Yes'",
        numericInput(
          "points", "Points",
          value = 1,
          min = 0.5, max = 10, step = 0.5
        )
      ),
      actionButton(
        "calc", "Calculate!"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Overall",
                 fluidRow(uiOutput("total_text")),
                 fluidRow(plotlyOutput("total_plot"))),
        tabPanel("Monthly",
                 fluidRow(uiOutput("monthly_text")),
                 fluidRow(plotlyOutput("monthly_plot"))),
        tabPanel("Summary Table", fluidRow(verbatimTextOutput("debug")))
      )
    )
  )
)