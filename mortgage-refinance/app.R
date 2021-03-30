library(shiny)

ui <- fluidPage(
    titlePanel("Simple Mortgage Refinance Dashboard"),
    sidebarLayout(
        sidebarPanel(
            h2("Original Mortgage"),
            numericInput(
                "P_orig", "Principal",
                value = 400000,
                min = 1, max = 2000000, step = 1
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
            actionButton(
                "calc", "Calculate!"
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(),
            fluidRow(),
            fluidRow(verbatimTextOutput("debug"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    #debug print
    output$debug <- renderPrint(input$t0_orig)
}

# Run the application 
shinyApp(ui = ui, server = server)
