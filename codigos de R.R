install.packages("shiny")
install.packages("shinydashboard")

library(shiny)
library(shinydashboard)

shinyServer(function(input, output){
  
})


shinyUI(
  dashboardPage(
    dashboardHeader(),
    dashboardsidebar(),
    dashboardBody()
  )
)
