### this is a non-shiny version intended for development/debugging

library(here)
### Global
source(here("mortgage-refinance/global.R"))

### load inputs from UI
source(here("00-non-shiny-for-development/01-UI_load.R"))

### server
#normalize dates to first of month
t0_orig <- ymd(paste(year(t0_orig), month(t0_orig), "01", sep = "-"))
t0_refi <- ymd(paste(year(t0_refi), month(t0_refi), "01", sep = "-")) 

#initial mortgage amortization
dataDF1 <- my_amort(
  P_orig, r_a_orig, 
  ifelse(n_orig == "30 year", 30*12, 15*12), 
  t0_orig
)

#mortgage refinance
#derive principal from dataDF1
P_refi <- dataDF1 %>%
  filter(date == t0_refi - months(1)) %>%
  pull(ending_balance)

#derive initial prinicpal paid from dataDF1
P0_refi <- dataDF1 %>%
  filter(date == t0_refi - months(1)) %>%
  pull(principal_paid)

#derive initial interest paid from dataDF1
I0_refi <- dataDF1 %>%
  filter(date == t0_refi - months(1)) %>%
  pull(interest_paid)

#amortization for refinanced loan
dataDF2 <- my_amort(
  P_refi, r_a_refi, 
  ifelse(n_refi == "30 year", 30*12, 15*12), 
  t0_refi, P0_refi, I0_refi
)

#totals
original_total <- sum(dataDF1$payment) %>% dollar_format()(.)
refi_total <- (P0_refi + sum(dataDF2$payment)) %>% dollar_format()(.)

#running total plot
cols <- pal_jco()(5)
plot_ly() %>%
  add_trace(data = dataDF1,
            x = ~date, y = ~ending_balance,
            name = "Balance",
            line = list(color = cols[1]),
            type = 'scatter', mode = 'lines'
  ) %>%
  add_trace(data = dataDF1,
            x = ~date, y = ~principal_paid,
            name = "Principal Paid",
            line = list(color = cols[2]),
            type = 'scatter', mode = 'lines'
  ) %>%
  add_trace(data = dataDF1,
            x = ~date, y = ~interest_paid,
            name = "Interest Paid",
            line = list(color = cols[3]),
            type = 'scatter', mode = 'lines'
  ) %>%
  add_trace(data = dataDF2,
            x = ~date, y = ~ending_balance,
            name = "Balance",
            line = list(color = cols[1], dash = "dot"),
            type = 'scatter', mode = 'lines'
  ) %>%
  add_trace(data = dataDF2,
            x = ~date, y = ~principal_paid,
            name = "Principal Paid", 
            line = list(color = cols[2], dash = "dot"),
            type = 'scatter', mode = 'lines'
  ) %>%
  add_trace(data = dataDF2,
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


#monthly payment plot
subplot(
  plot_ly() %>%
    add_trace(data = dataDF1,
              x = ~date, y = ~principal_payment,
              name = "Principal",
              fillcolor = cols[5],
              type = "scatter", mode = "none", stackgroup = "one"
    ) %>%
    add_trace(data = dataDF1,
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
    add_trace(data = dataDF2,
              x = ~date, y = ~principal_payment,
              name = "Principal",
              fillcolor = cols[5],
              type = "scatter", mode = "none", stackgroup = "one"
    ) %>%
    add_trace(data = dataDF2,
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
