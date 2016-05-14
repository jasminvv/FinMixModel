library(shiny)

shinyUI(fluidPage(
  
  navbarPage(tabPanel("Home Page", "contents"),
  tabPanel("Upload Data", "contents"),
  tabPanel("Simulate Data", "contents"),
  tabPanel("Results", "contents")),
  
  sidebarLayout(
    sidebarPanel(
    ),
  
    mainPanel(
    )
  )  
))