if(!require(pacman)) install.packages('pacman')
p_load(tidyverse, lubridate, ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)

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

#running totals plot
plot_ly() %>%
  add_trace(
    data = dataDF1 %>%
      pivot_longer(c("ending_balance", "principal_paid", "interest_paid")) %>%
      mutate(dollars = dollar_format()(value)),
    x = ~date, y = ~value, color = ~name, name = "Original Mortgage",
    text = ~paste0(
      "Date: ", date,
      "<br>orig. ", name, ":", dollars
    ),
    hoverinfo = 'text',
    type = 'scatter', mode = 'lines'
  ) %>%
  add_trace(
    data = dataDF2 %>%
      pivot_longer(c("ending_balance", "principal_paid", "interest_paid")) %>%
      mutate(dollars = dollar_format()(value)),
    x = ~date, y = ~value, color = ~name, name = "Refinance",
    type = 'scatter', mode = 'lines', line = list(dash = "dot")
  ) %>%
  layout(hovermode = "x unified")
