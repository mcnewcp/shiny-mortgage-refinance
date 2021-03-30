library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(ggsci)

#initial mortgage amortization
dataDF1 <- my_amort(625000, 3.990, 30*12, ymd("2019-08-01")) 

#mortgage refinance
#start date of refinance
t0 <- ymd("2021-04-01")
#annual interest rate
r_a <- 3
#total number of months
n <- 30*12
#derive principal from dataDF1
P <- dataDF1 %>% 
  filter(date == t0 - months(1)) %>%
  pull(ending_balance)
#derive initial prinicpal paid from dataDF1
P0 <- dataDF1 %>% 
  filter(date == t0 - months(1)) %>%
  pull(principal_paid)
#derive initial interest paid from dataDF1
I0 <- dataDF1 %>% 
  filter(date == t0 - months(1)) %>%
  pull(interest_paid)

dataDF2 <- my_amort(P, r_a, n, t0, P0, I0)

#totals
original_total <- sum(dataDF1$payment) %>% dollar_format()(.)
refi_total <- (P0 + sum(dataDF2$payment)) %>% dollar_format()(.)

cols <- pal_jco()(5)

#running totals plot
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
  )

#monthly payment plot
p1 <- plot_ly() %>%
  add_trace(data = dataDF1,
            x = ~date, y = ~principal_payment,
            name = "Principal",
            marker = list(color = cols[5]),
            type = "bar"
  ) %>%
  add_trace(data = dataDF1,
            x = ~date, y = ~interest_payment,
            name = "Interest", 
            marker = list(color = cols[4]),
            type = "bar"
  ) %>%
  layout(barmode = "stack", hovermode = "x unified")

p2 <- plot_ly() %>%
  add_trace(data = dataDF2,
            x = ~date, y = ~principal_payment,
            name = "Principal", 
            marker = list(color = cols[5]),
            type = "bar"
  ) %>%
  add_trace(data = dataDF2,
            x = ~date, y = ~interest_payment,
            name = "Interest", 
            marker = list(color = cols[4]),
            type = "bar"
  ) %>%
  layout(barmode = "stack", hovermode = "x unified")

subplot(p1, p2, shareX = TRUE, nrows = 2) %>%
  layout(
    xaxis = list(title = "Date"),
    yaxis = list(title = "Original Mortgage Payment ($)"),
    yaxis2 = list(title = "Refinanced Mortgage Payment ($)"),
    showlegend = FALSE
  )
