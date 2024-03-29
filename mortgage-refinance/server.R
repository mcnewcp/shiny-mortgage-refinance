function(input, output) {
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
  
  #calculate total added costs
  add_costs <- reactive({
    input$calc
    isolate(input$close_cost + ifelse(input$points_yn == "Yes", input$points/100*P_refi, 0))
  })
  
  #amortization for refinanced loan
  dataDF2 <- reactive({
    input$calc
    isolate(
      my_amort(P_refi(), input$r_a_refi, 
               ifelse(input$n_refi == "30 year", 30*12, 15*12), 
               t0_refi(), P0_refi(), I0_refi(), add_costs())
    )
  })
  
  #totals
  original_total <- reactive({
    input$calc
    isolate(
      sum(dataDF1()$payment) %>% dollar_format()(.)
    )
  })
  refi_total <- reactive({
    input$calc
    isolate((P0_refi() + add_costs() + sum(dataDF2()$payment)) %>% dollar_format()(.))
  })
  output$total_text <- renderUI({
    h3(paste("Total Amount Repaid: Original =", original_total(), ", Refinanced =", refi_total()))
  })
  
  #df for yearly summary table
  sumDF <- reactive({
    input$calc
    isolate(
      dataDF1() %>%
        select(
          date, principal_paid_orig = principal_paid, 
          interest_paid_orig = interest_paid, total_paid_orig = total_paid
        ) %>%
        full_join(
          dataDF2() %>% select(
            date, principal_paid_refi = principal_paid, 
            interest_paid_refi = interest_paid, total_paid_refi = total_paid
          )
        ) %>%
        #drop all rows before refi
        filter(date >= t0_refi()) %>%
        #fill down blank rows for mismatched dates
        fill(contains("paid"), .direction = "down") %>%
        #generate differences
        mutate(
          principal_paid_diff = principal_paid_refi - principal_paid_orig,
          interest_paid_diff = interest_paid_refi - interest_paid_orig,
          total_paid_diff = total_paid_refi - total_paid_orig
        ) %>%
        mutate_if(is.numeric, round, 2) %>%
        #choose yearly rows
        mutate(
          month = month(date, label = TRUE, abbr = TRUE),
          year = year(date)
        ) %>%
        filter(month == month(t0_refi(), label = TRUE, abbr = TRUE)) %>%
        #add month/year as row numbers
        mutate(myear = paste(month, year)) 
    )
  }) 
  
  #monthly payment
  output$monthly_text <- renderUI({
    h3(paste(
      "Monthly Payment: Original =", dollar_format()(dataDF1()$payment[1]), 
      ", Refinanced =", dollar_format()(dataDF2()$payment[1])
    ))
  })
  
  #debug print
  # output$debug <- renderPrint(head(sumDF()))
  
  #running total plot
  cols <- pal_jco()(5)
  output$total_plot <- renderPlotly(
    plot_ly() %>%
      add_trace(data = dataDF1(),
                x = ~date, y = ~ending_balance,
                name = "Balance",
                line = list(color = cols[1]),
                type = 'scatter', mode = 'lines'
      ) %>%
      add_trace(data = dataDF1(),
                x = ~date, y = ~principal_paid,
                name = "Principal Paid",
                line = list(color = cols[2]),
                type = 'scatter', mode = 'lines'
      ) %>%
      add_trace(data = dataDF1(),
                x = ~date, y = ~interest_paid,
                name = "Interest Paid",
                line = list(color = cols[3]),
                type = 'scatter', mode = 'lines'
      ) %>%
      add_trace(data = dataDF2(),
                x = ~date, y = ~ending_balance,
                name = "Balance",
                line = list(color = cols[1], dash = "dot"),
                type = 'scatter', mode = 'lines'
      ) %>%
      add_trace(data = dataDF2(),
                x = ~date, y = ~principal_paid,
                name = "Principal Paid", 
                line = list(color = cols[2], dash = "dot"),
                type = 'scatter', mode = 'lines'
      ) %>%
      add_trace(data = dataDF2(),
                x = ~date, y = ~interest_paid,
                name = "Interest Paid", 
                line = list(color = cols[3], dash = "dot"),
                type = 'scatter', mode = 'lines'
      ) %>%
      layout(
        hovermode = "x unified", showlegend = FALSE,
        xaxis = list(title = "Date"), yaxis = list(title = "Running Total ($)")
      ) %>%
      add_annotations(
        x= 0.5, y= 1,
        xref = "paper", yref = "paper",
        text = "Solid lines = original mortgage, dotted lines = refinanced",
        showarrow = F
      )
  )
  
  
  #monthly payment plot
  output$monthly_plot <- renderPlotly(
    subplot(
      plot_ly() %>%
        add_trace(data = dataDF1(),
                  x = ~date, y = ~principal_payment,
                  name = "Principal",
                  fillcolor = cols[5],
                  type = "scatter", mode = "none", stackgroup = "one"
        ) %>%
        add_trace(data = dataDF1(),
                  x = ~date, y = ~interest_payment,
                  name = "Interest", 
                  fillcolor = cols[4],
                  type = "scatter", mode = "none", stackgroup = "one"
        ) %>%
        add_annotations(
          x= 0.5, y= 0.2,
          xref = "paper", yref = "paper",
          text = "Original Monthly Payment",
          showarrow = F,
          font = list(color = 'white', size = 14)
        ) %>%
        layout(hovermode = "x unified"),
      plot_ly() %>%
        add_trace(data = dataDF2(),
                  x = ~date, y = ~principal_payment,
                  name = "Principal",
                  fillcolor = cols[5],
                  type = "scatter", mode = "none", stackgroup = "one"
        ) %>%
        add_trace(data = dataDF2(),
                  x = ~date, y = ~interest_payment,
                  name = "Interest", 
                  fillcolor = cols[4],
                  type = "scatter", mode = "none", stackgroup = "one"
        ) %>%
        add_annotations(
          x= 0.5, y= 0.2,
          xref = "paper", yref = "paper",
          text = "Refinanced Monthly Payment",
          showarrow = F,
          font = list(color = 'white', size = 14)
        ) %>%
        layout(hovermode = "x unified"),
      shareX = TRUE, nrows = 2
    ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Original Mortgage<br>Payment ($)"),
        yaxis2 = list(title = "Refinanced Mortgage<br>Payment ($)"),
        showlegend = FALSE
      )
  )
  
  #original mortgage table
  output$orig_table <- renderFormattable(
    formattable(
      sumDF() %>% 
        select(
          Date = myear, Equity = principal_paid_orig, 
          `Interest Paid` = interest_paid_orig, `Total Paid` = total_paid_orig
        )
    )
  )
  
  #refinanced mortgage table
  output$refi_table <- renderFormattable(
    formattable(
      sumDF() %>% 
        select(
          Date = myear, Equity = principal_paid_refi, 
          `Interest Paid` = interest_paid_refi, `Total Paid` = total_paid_refi
        )
    )
  )
  
  #difference table
  output$diff_table <- renderFormattable(
    formattable(
      sumDF() %>% 
        select(
          Date = myear, Equity = principal_paid_diff, 
          `Interest Paid` = interest_paid_diff, `Total Paid` = total_paid_diff
        ),
      list(
        Equity = formatter("span", 
                           style = function(x){
                             style(display            = "block",
                                   padding            = "0 4px",
                                   `border-radius`    = "4px",
                                   `background-color` = my_colors(sumDF()$principal_paid_diff, neg_color = "red", pos_color = "green")
                             )}),
        `Interest Paid` = formatter("span", 
                                    style = function(x){
                                      style(display            = "block",
                                            padding            = "0 4px",
                                            `border-radius`    = "4px",
                                            `background-color` = my_colors(sumDF()$interest_paid_diff)
                                      )}),
        `Total Paid` = formatter("span", 
                                 style = function(x){
                                   style(display            = "block",
                                         padding            = "0 4px",
                                         `border-radius`    = "4px",
                                         `background-color` = my_colors(sumDF()$total_paid_diff)
                                   )})
      )
    )
  )
  
}