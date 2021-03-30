my_amort <- function(
  P, #total principal
  r_a, #annual interest rate
  n, #total number of months
  t0, #start date
  P0 = 0, #initial principal paid (for refinance calcs)
  I0 = 0 #initial interest paid (for refinance calcs)
  
) {
  #monthly interest rate
  r <- r_a/100/12
  #monthly payment
  M <- P * r / (1 - (1 + r)^(-n))
  
  #loop through months and generate time series
  #initialize
  outDF <- tibble()
  iP <- P
  iP_paid <- P0
  iI_paid <- I0
  it <- t0
  for (i in 1:n) {
    #monthly interest
    mI <- iP * r
    #monthly principal
    mP <- M - mI
    #ending loan balance
    iP <- iP - mP
    #data rows
    iDF <- tibble(
      date = it, payment = M, 
      principal_payment = mP, interest_payment = mI,
      ending_balance = iP,
      principal_paid = iP_paid + mP,
      interest_paid = iI_paid + mI
    )
    outDF <- outDF %>% bind_rows(iDF)
    it <- it + months(1)
    iP_paid <- iP_paid + mP
    iI_paid <- iI_paid + mI
  }
  return(outDF)
}




