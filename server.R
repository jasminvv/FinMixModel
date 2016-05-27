library(shiny)
library(xtable)
source("Functions.R")

# Put anything executed only once during server set-up here

function(input, output, session) {

  # Put anything executed after user visit here,
  # and in render* functions when executed after user input
  variables <- reactiveValues(
    sim.means     = vector(mode = "numeric", length = 0),
    sim.std.devs  = vector(mode = "numeric", length = 0),
    sim.priors    = vector(mode = "numeric", length = 0),
    sim.data      = vector(mode = "numeric", length = 0),
    mixture.model = list(),
    user.data     = data.frame()
  )
  
  observeEvent(input$sim.add.gaussian, {
    variables$sim.means[length(variables$sim.means) + 1]       <- input$sim.mean
    variables$sim.std.devs[length(variables$sim.std.devs) + 1] <- input$sim.std.dev
    variables$sim.priors[length(variables$sim.priors) + 1]     <- input$sim.prior
    variables$sim.data                                         <- vector(mode = "numeric", length = 0)
    output$sim.table <- renderTable({data.frame(Mean = variables$sim.means, SD = variables$sim.std.devs, 
                                                Prior = variables$sim.priors)})
    output$sim.plot <- renderPlot({PlotSimulation(variables$sim.data, variables$sim.means, 
                                                  variables$sim.std.devs, variables$sim.priors)})
  })
  observeEvent(input$sim.reset, {
    variables$sim.means     <- vector(mode = "numeric", length = 0)
    variables$sim.std.devs  <- vector(mode = "numeric", length = 0)
    variables$sim.priors    <- vector(mode = "numeric", length = 0)
    variables$sim.data      <- vector(mode = "numeric", length = 0)
    variables$mixture.model <- list()
    output$sim.plot <- renderPlot({plot.new()})
    output$res.plot <- renderPlot({plot.new()})
    output$res.table <- renderTable({})
  })
  observeEvent(input$sim.sample, {
    variables$sim.data <- SimulateDataset(variables$sim.means, variables$sim.std.devs, 
                                          variables$sim.priors, input$sim.sample.size)
    output$sim.plot <- renderPlot({PlotSimulation(variables$sim.data, variables$sim.means, 
                                                  variables$sim.std.devs, variables$sim.priors)})
  })
  observeEvent(input$sim.calculate, {
    variables$mixture.model <- RunMixtureModel(variables$sim.data, input$sim.num.subpopulations, 
                                               input$sim.max.times)
    output$res.plot  <- renderPlot({PlotResults(variables$sim.data, variables$mixture.model$est.means, 
                                                variables$mixture.model$est.vars, 
                                                variables$mixture.model$est.priors)})
    output$res.table <- renderTable({data.frame(Mean = variables$mixture.model$est.means, 
                                                SD = sqrt(variables$mixture.model$est.vars), 
                                                Prior = variables$mixture.model$est.priors)})
    output$res.BIC <- renderUI({str <- HTML(paste("Bayesian Information Criterion:", 
                                                  variables$mixture.model$BIC))})
    updateTabsetPanel(session, inputId = "tabs", selected = "res")  
  })
  observeEvent(input$data.calculate, {
    variables$user.data     <- ReadData(input$data.file[, "datapath"], input$data.file[, "name"])
    variables$mixture.model <- RunMixtureModel(variables$user.data[, 1], input$data.num.subpopulations, 
                                               input$data.max.times)
    output$res.plot  <- renderPlot({PlotResults(variables$user.data[, 1], 
                                                variables$mixture.model$est.means, 
                                                variables$mixture.model$est.vars, 
                                                variables$mixture.model$est.priors)})
    output$res.table <- renderTable({data.frame(Mean = variables$mixture.model$est.means, 
                                                SD = sqrt(variables$mixture.model$est.vars), 
                                                Prior = variables$mixture.model$est.priors)})
    output$res.BIC <- renderUI({str <- HTML(paste("Bayesian Information Criterion:", 
                                                  variables$mixture.model$BIC))})
    updateTabsetPanel(session, inputId = "tabs", selected = "res") 
  })
}