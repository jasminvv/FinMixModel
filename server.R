library(shiny)
source("Functions.R")

# Put anything executed only once during server set-up here

function(input, output) {

  # Put anything executed after user visit here,
  # and in render* functions when executed after user input
  variables <- reactiveValues(
    sim.means    = vector(mode = "numeric", length = 0),
    sim.std.devs = vector(mode = "numeric", length = 0),
    sim.priors   = vector(mode = "numeric", length = 0),
    sim.data     = vector(mode = "numeric", length = 0)
  )
  
  observeEvent(input$sim.add.gaussian, {
    variables$sim.means[length(variables$sim.means) + 1]       <- input$sim.mean
    variables$sim.std.devs[length(variables$sim.std.devs) + 1] <- input$sim.std.dev
    variables$sim.priors[length(variables$sim.priors) + 1]     <- input$sim.prior
    output$sim.plot <- renderPlot({PlotSimulation(variables$sim.data, variables$sim.means, variables$sim.std.devs, variables$sim.priors)})
  })
  observeEvent(input$sim.reset, {
    variables$sim.means    <- vector(mode = "numeric", length = 0)
    variables$sim.std.devs <- vector(mode = "numeric", length = 0)
    variables$sim.priors   <- vector(mode = "numeric", length = 0)
    output$sim.plot <- renderPlot({PlotSimulation(variables$sim.data, variables$sim.means, variables$sim.std.devs, variables$sim.priors)})
  })
  observeEvent(input$sim.sample, {
    variables$sim.data <- SimulateDataset(variables$sim.means, variables$sim.std.devs, variables$sim.priors, input$sim.sample.size)
    output$sim.plot <- renderPlot({PlotSimulation(variables$sim.data, variables$sim.means, variables$sim.std.devs, variables$sim.priors)})
  })
  
}