library(shiny)
library(rsconnect)
#run app
runApp("mortgage-refinance")

#deploy to shinyapps.io
rsconnect::deployApp(file.path('mortgage-refinance'))
