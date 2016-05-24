library(shiny)

shinyUI(fluidPage(
  
  tabsetPanel(
              tabPanel("Home Page", 
                fluidPage()),
  
              tabPanel("Upload Data", 
                fluidPage(mainPanel(
                  "Upload Data",
                  fileInput("data.file", NULL, accept = c("plain", "x-excel")),
                  actionButton("data.calculate", "Calculate")
              ))),
  
              tabPanel("Simulate Data", 
                fluidPage(sidebarLayout(
                mainPanel(
                  h3("Choose Parameters"),
                  numericInput("sim.mean", "Mean", 1, min = 0, max = 100, step = 1),
                  numericInput("sim.std.dev", "Standard Deviation", 1, min = 0, max = 20, step = 0.1),
                  numericInput("sim.prior", "Prior", 1, min = 0, max = 1, step = 0.01),
                  actionButton("sim.add.gaussian", "Add"),
                  actionButton("sim.reset", "Reset"),
                  h3("Set Sample Size"),
                  numericInput("sim.sample.size", "Sample Size", 100, min = 10, max = 10000, step = 1),
                  actionButton("sim.sample", "Sample")
                ),
             
                sidebarPanel(
                  plotOutput("sim.plot"),
                  actionButton("sim.calculate", "Calculate")
                )
              ))),
  
              tabPanel("Results", 
                fluidPage()))

))