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
    
    #normalize dates to first of month
    t0_orig <- reactive({
        input$calc
        isolate(
            ymd(paste(year(input$t0_orig), month(input$t0_orig), "01", sep = "-")) 
        )
    })
    t0_refi <- reactive({
        input$calc
        isolate(
            ymd(paste(year(input$t0_refi), month(input$t0_refi), "01", sep = "-")) 
        )
    })
    
    #initial mortgage amortization
    dataDF1 <- reactive({
        input$calc
        isolate(
            my_amort(
                input$P_orig, input$r_a_orig, 
                ifelse(input$n_orig == "30 year", 30*12, 15*12), 
                t0_orig()
            )
        )
    })
    
    #mortgage refinance
    #derive principal from dataDF1
    P_refi <- reactive({
        input$calc
        isolate(
            dataDF1() %>%
                filter(date == t0_refi() - months(1)) %>%
                pull(ending_balance)
        )
    })
    #derive initial prinicpal paid from dataDF1
    P0_refi <- reactive({
        input$calc
        isolate(
            dataDF1() %>%
                filter(date == t0_refi() - months(1)) %>%
                pull(principal_paid))
    })
    #derive initial interest paid from dataDF1
    I0_refi <- reactive({
        input$calc
        isolate(
            dataDF1() %>%
                filter(date == t0_refi() - months(1)) %>%
                pull(interest_paid)
        )
    })
    
    #amortization for refinanced loan
    dataDF2 <- reactive({
        input$calc
        isolate(
            my_amort(P_refi(), input$r_a_refi, 
                     ifelse(input$n_refi == "30 year", 30*12, 15*12), 
                     t0_refi(), P0_refi(), I0_refi())
        )
    })
    
    #debug print
    output$debug <- renderPrint(head(dataDF2()))
    
    #totals
    original_total <- reactive({
        input$calc
        isolate(
            sum(dataDF1()$payment) %>% dollar_format()(.)
        )
    })
    refi_total <- reactive({
        input$calc
        isolate((P0_refi() + sum(dataDF2()$payment)) %>% dollar_format()(.))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
