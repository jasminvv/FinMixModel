library(shiny)

shinyUI(fluidPage(
  
  tabsetPanel(
              tabPanel("Home Page", 
                fluidPage(fluidRow(column(6, offset = 3, style = "background-color: #E8E8E8",
                  h3("Finite Mixture Model Calculation", style = "text-align: center;"),
                  p("With this application, you can fit a mixture model of multiple gaussians to data you upload or simulate. 
                    The application uses the EM algorithm. To start, go to the upload or simulation page through the links at the top of the page.
                    \n For further information, see the user manual.", style = "text-align: center;")
                ))),
                value = "home"),
  
              tabPanel("Upload Data", 
                fluidPage(fluidRow(column(6,
                  h3("Upload Data"),
                  fileInput("data.file", NULL, accept = c("plain", "x-excel"))),
                  column(6,
                  h3("Set Mixture Model Parameters"),
                  numericInput("data.num.subpopulations", "Number of subpopulations", 2, min = 1, max = 10, step = 1),
                  numericInput("data.max.times", "Maximum number of iterations", 1500, min = 1, max = 5000, step = 1),
                  actionButton("data.calculate", "Calculate")
              ))),
              value = "data"),
  
              tabPanel("Simulate Data", 
                fluidPage(fluidRow(
                column(3,
                  h3("Choose Parameters"),
                  numericInput("sim.mean", "Mean", 1, min = 0, max = 1000, step = 0.01),
                  numericInput("sim.std.dev", "Standard Deviation", 1, min = 0, max = 333, step = 0.01),
                  numericInput("sim.prior", "Weight", 1, min = 0, max = 1, step = 0.01),
                  actionButton("sim.add.gaussian", "Add"),
                  actionButton("sim.reset", "Reset"),
                  h4("Current Subpopulations"),
                  tableOutput("sim.table")
                ),
                
                column(3,
                  h3("Set Sample Size"),
                  numericInput("sim.sample.size", "Sample Size", 100, min = 10, max = 10000, step = 1),
                  actionButton("sim.sample", "Sample")
                ),
             
                column(6,
                  plotOutput("sim.plot"),
                  h3("Set Mixture Model Parameters"),
                  numericInput("sim.num.subpopulations", "Number of subpopulations", 2, min = 1, max = 10, step = 1),
                  numericInput("sim.max.times", "Maximum number of iterations", 1500, min = 1, max = 5000, step = 1),
                  actionButton("sim.calculate", "Calculate")
                )
              )),
              value = "sim"),
  
              tabPanel("Results", 
                fluidPage(fluidRow(
                column(8,
                  plotOutput("res.plot")
                ),
                
                column(4,
                  h3("Results"),
                  tableOutput("res.table"),
                  htmlOutput("res.BIC")
                )
                )),
                value = "res"),
              
              id = "tabs"

)))