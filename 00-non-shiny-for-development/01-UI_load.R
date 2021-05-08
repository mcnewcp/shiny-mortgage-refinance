###input values from UI

#Original Mortgage Principal
P_orig <- 625000
#Original Interest Rate
r_a_orig <- 4.0
#Mortgage Length: "15 year" or "30 year"
n_orig <- "30 year"
#Mortgage Start Date
t0_orig <- ymd("2019-07-01")

#Refinance Interest Rate
r_a_refi <-  3.3
#Refinance Length: "15 year" or "30 year"
n_refi <- "30 year" 
#Refinance Start Date
t0_refi <- ymd("2021-05-01")
#Closing Costs
close_cost <- 5000
#Points Y/N
points_yn <- "Yes"
#Points
points <- 3