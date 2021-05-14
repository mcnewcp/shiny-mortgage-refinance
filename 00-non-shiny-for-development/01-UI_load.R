###input values from UI

#Original Mortgage Principal
P_orig <- 625000
#Original Interest Rate
r_a_orig <- 3.99
#Mortgage Length: "15 year" or "30 year"
n_orig <- "30 year"
#Mortgage Start Date
t0_orig <- ymd("2019-08-01")

#Refinance Interest Rate
r_a_refi <-  2.59
#Refinance Length: "15 year" or "30 year"
n_refi <- "15 year" 
#Refinance Start Date
t0_refi <- ymd("2021-06-01")
#Closing Costs
close_cost <- 3200
#Points Y/N
points_yn <- "No"
#Points
points <- 3