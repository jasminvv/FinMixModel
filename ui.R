library(shiny)

shinyUI(fluidPage(
  
  tabsetPanel(
              tabPanel("Home Page", 
                fluidPage(mainPanel(
                  h3("Finite Mixture Model Calculation"),
                  p("With this application, you can fit a mixture model of multiple gaussians to data you upload or simulate. 
                    The application uses the EM approach. To start, go to the [link to upload page) or [link to simulation page].
                    For further information, see the [link to user manual].")
                ))),
  
              tabPanel("Upload Data", 
                fluidPage(mainPanel(
                  "Upload Data",
                  fileInput("data.file", NULL, accept = c("plain", "x-excel")),
                  numericInput("sim.num.subpopulations", "Number of subpopulations", 2, min = 1, max = 10, step = 1),
                  numericInput("sim.max.times", "Maximum number of iterations", 1500, min = 1, max = 5000, step = 1),
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
                  numericInput("sim.num.subpopulations", "Number of subpopulations", 2, min = 1, max = 10, step = 1),
                  numericInput("sim.max.times", "Maximum number of iterations", 1500, min = 1, max = 5000, step = 1),
                  actionButton("sim.calculate", "Calculate")
                )
              ))),
  
              tabPanel("Results", 
                fluidPage(sidebarLayout(
                mainPanel(
                  plotOutput("res.plot")
                ),
                
                sidebarPanel(
                  h4("Results"),
                  tableOutput("res.table")
                )
                )))

)))