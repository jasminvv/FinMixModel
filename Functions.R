library(gdata)
library(foreign)

# ***READ DATA: Functions: ReadData

ReadData <- function(filepath, filename) {
  # Function that takes the path to the file uploaded by the user
  # and its name, and returns a dataframe containing the data
  # Can read .xls, .txt, .csv and .spss files.
  if (grepl(".xls$", filename)) {
    user.data <- read.xls(filepath)
  }
  if (grepl(".txt$", filename)) {
    user.data <- read.table(filepath)
  }
  if (grepl(".csv$", filename)) {
    user.data <- read.csv(filepath)
  }
  if (grepl(".spss$", filename)) {
    user.data <- read.spss(filepath, to.data.frame = TRUE)
  }
  return(user.data)
} 

# ***SIMULATE DATA: Functions: SetDrawProbability and SimulateDataset

SetDrawProbability <- function(priors) {
  # Function that takes a vector of k priors, and returns a 
  # vector where the priors are added up and divided by the total
  # (newprior2 = prior1+2/total, newprior3 = newprior2+prior3/total, etc)
  draw.p <- vector(mode = "numeric", length = length(priors))
  for (b in seq_along(priors)) {
    if (b == 1) {
      draw.p[b] <- priors[b]/sum(priors)
    }
    else {
      draw.p[b] <- draw.p[b - 1] + (priors[b] / sum(priors))
    }
  }
  return(draw.p)
}

NormalizePriors <- function(priors) {
  # Function that takes in a vector containing the priors, and returns a
  # vector containing the normalized priors, so that they add up to 1.
  norm.priors <- vector(mode = "numeric", length = length(priors))
  for (b in seq_along(priors)) {
    norm.priors[b] <- priors[b]/sum(priors)
  }
  return(norm.priors)
}

SimulateDataset <- function(means, std.devs, priors, sample.size) {
  # Function that takes a vector of k means, a vector of k standard deviations,
  # a vector of k priors, and a sample size n. It returns a vector of n simulated 
  # observations taken from a population with k subpopulations (mean, sd and prior from input).
  if (length(means) != length(std.devs) || length(means) != length(priors) || length(std.devs) != length(priors)) {
    stop("Not all parameters have been set.")
  }
  sample.dataset <- vector(mode="numeric", length=sample.size)
  draw.p <- SetDrawProbability(priors)
  for (i in 1:sample.size) {
    random.number = runif(1, min = 0, max = 1)
    for (b in seq_along(draw.p)) {
      if (random.number <= draw.p[b]) {
        sample.dataset[i] <- rnorm(1, means[b], std.devs[b])
        break
      }
    }	  
  }
  return(sample.dataset)
}

PlotSimulation <- function(data, means, std.devs, priors){
  # Function that takes vectors containing the data values, means, 
  # standard deviations and priors, and produces a plot containing
  # either nothing, a histogram of the data, and/or curves showing 
  # the subpopulations and total population
  priors      <- NormalizePriors(priors)
  x.vector    <- c(min(means) - max(std.devs) * 3, max(means) + max(std.devs) * 3)
  colors.plot <- c("red4", "turquoise4", "darkgoldenrod2", "darkorchid4", "darkgreen", "darkorange1", "deeppink4", "springgreen4", "red3", "mediumvioletred")
  plot(1, type = "n", xlab = "", ylab = "", xlim = x.vector, ylim = c(0, 0.5))
  if (length(data) > 0) {  
    x.vector <- range(data)
    hist(data, breaks = length(data) / 3, xlim = range(data), ylim = NULL, prob = TRUE, xlab = "Data values", main = "Histogram of sample, and density graphs for populations", border = "grey80", col = "grey90")
  }
  for (b in seq_along(means)) {
    if (b == 1) {
      densities <- (dnorm(seq(from = min(x.vector), to = max(x.vector), by = (max(x.vector) - min(x.vector))/100), mean = means[b], sd = std.devs[b]) * priors[b])
      curve(dnorm(x, mean = means[b], sd = std.devs[b]) * priors[b], add = TRUE, col = colors.plot[b])
    }
    else {
      densities <- densities + dnorm(seq(from = min(x.vector), to = max(x.vector), by = (max(x.vector) - min(x.vector))/100), mean = means[b], sd = std.devs[b]) * priors[b]
      curve(dnorm(x, mean = means[b], sd = std.devs[b]) * priors[b], add = TRUE, col = colors.plot[b])
    }
  }
  lines(seq(from = min(x.vector), to = max(x.vector), by = (max(x.vector) - min(x.vector))/100), densities, type = "l")
}

# ***SIMPLE MIXTURE MODEL: Functions: InitiatePMembership, InitiateEstMeans,
# ***InitiateEstVars, InitiateEstPriors, CalculatePMembership, 
# ***CalculateEstMeans, CalculateEstVars and RunMixtureModel

InitiatePMembership <- function(num.subpopulations, sample.size){
  # Function that takes the wanted number of subpopulations (k) and sample size (n),
  # and returns a matrix containing k rows of length n
  p.membership <- matrix(nrow = num.subpopulations, ncol = sample.size)
  return(p.membership)
}

InitiateEstMeans <- function(num.subpopulations, data){
  # Function that takes the wanted number of subpopulations (k) and the data,
  # and returns a vector containing k random means between the smallest and 
  # largest value in the data
  est.means <- vector(mode = "numeric", length = num.subpopulations)
  for (b in 1:num.subpopulations) {
    est.means[b] <- runif(1, min = min(data), max = max(data))
  }
  return(est.means)
}

InitiateEstVars <- function(num.subpopulations, data){
  # Function that takes the wanted number of subpopulations (k) and the data,
  # and returns a vector containing k random variances between
  # 1% and 1/6th of the range of the data
  est.vars   <- vector(mode = "numeric", length = num.subpopulations)
  range.data <- max(data) - min(data)
  for (b in 1:num.subpopulations) {
    est.vars[b] <- runif(1, min = range.data/100, max = range.data/6)
  }
  return(est.vars)
}

InitiateEstPriors <- function(num.subpopulations){
  # Function that takes the wanted number of subpopulations (k),
  # and returns a vector containing k priors that are equal in size (1/k)
  est.priors <- vector(mode = "numeric", length = num.subpopulations)
  for (b in 1:num.subpopulations) {
    est.priors[b] <- 1/num.subpopulations
  }
  return(est.priors)
}

CalculatePMemberships <- function(data, est.vars, est.means, est.priors, p.membership) {
  # Function that takes the data, and estimation of the prior, mean and 
  # std.dev of a subpopulation, and the vector containing current probabilities of membership,
  # and returns one vector of the old and one of the new probabilities that each value 
  # belongs to any of the subpopulations
  old.p.membership <- p.membership
  p.value.given.pop <- vector(mode = "numeric", length = length(est.means))
  p.times.prior <- vector(mode = "numeric", length = length(est.means))
  for (x in seq_along(data)) {
    for (b in seq_along(est.means)) {
      p.value.given.pop[b] <- (1 / (sqrt(2 * pi * est.vars[b]))) * exp(- (((data[x] - est.means[b]) ^ 2) / (2 * est.vars[b])))
      p.times.prior[b] <- est.priors[b] * p.value.given.pop[b]
    }
    for (b in seq_along(est.means)) {
      p.membership[b,x] <- p.times.prior[b] / sum(p.times.prior)
    }
  }
  return(p.membership)
}

CalculateEstMeans <- function(data, p.membership, est.means) {
  # Function that takes the data, membership probability matrix and 
  # vector containing the old estimations of the means, and
  # returns a vector containing the new estimations of the means.
  rel.weight <- vector(mode = "numeric", length = length(data))
  for (b in seq_along(est.means)) {
    for (x in seq_along(data)) {
      rel.weight[x] <- p.membership[b,x] * data[x]
    }
    est.means[b] <- sum(rel.weight) / sum(p.membership[b, ])
  }
  return(est.means)
}

CalculateEstVars <- function(data, p.membership, est.means, est.vars) {
  # Function that takes the data, membership probability matrix, vector containing the 
  # estimations of the means, and vector containing the old estimations of the std. devs
  # and returns a vector containing the new estimations of the std. devs.
  rel.var <- vector(mode = "numeric", length = length(data))
  for (b in seq_along(est.vars)) {
    for (x in seq_along(data)) {
      rel.var[x] <- p.membership[b,x] * ((data[x] - est.means[b]) ^ 2)
    }
    est.vars[b] <- sum(rel.var) / sum(p.membership[b, ])
  }
  return(est.vars)
}

CalculateBIC <- function(data, est.means, p.membership){
  likelihood <- 0
  for (x in seq_along(data)) {
    likelihood <- likelihood + log(sum(p.membership[,x]))
  }
  BIC <- likelihood - 0.5 * 2 * length(est.means) * log(length(data))
  return(BIC)
}

RunMixtureModel <- function(data, num.subpopulations, max.times){
  # Function that takes a vector containing the data, a numeric variable 
  # with the number of subpopulations and a numeric variable with the maximum 
  # number of iterations, and returns a list containing a vector with estimations
  # of the means, a vector with estimations of the variances, a vector with
  # estimations of the priors, a matrix containing the probability each datapoint
  # belongs to one of the subpopulations(rows) and the BIC fitness measure
  n.obs        <- length(data)
  est.means    <- InitiateEstMeans(num.subpopulations, data)
  est.vars <- InitiateEstVars(num.subpopulations, data)
  est.priors   <- InitiateEstPriors(num.subpopulations)
  p.membership <- InitiatePMembership(num.subpopulations, n.obs)
  
  for (n in 1:max.times) {
    p.membership <- CalculatePMemberships(data, est.vars, est.means, est.priors, p.membership)
    est.means    <- CalculateEstMeans(data, p.membership, est.means)
    est.vars <- CalculateEstVars(data, p.membership, est.means, est.vars)
  }
  BIC <- CalculateBIC(data, est.means, p.membership)
  
  mixture.model <- list("est.means" = est.means, "est.vars" = est.vars, "est.priors" = est.priors, "p.membership" = p.membership, "BIC" = BIC)
  
  return(mixture.model)
}

# PLOT RESULTS

PlotResults <- function(data, est.means, est.vars, est.priors){
  # Function that takes a vector containing the data, and vectors
  # containing estimations of the means, variances and priors
  # and produces a histogram of the data with density graphs for the
  # subpopulations and total population
  colors.plot <- c("red4", "turquoise4", "darkgoldenrod2", "darkorchid4", "darkgreen", "darkorange1", "deeppink4", "springgreen4", "red3", "mediumvioletred")
  hist(data, breaks = length(data) / 3, xlim = range(data), ylim = NULL, prob = TRUE, xlab = "Data values", main = "Histogram of Data, with Density Graphs for Subpopulations", border = "grey80", col = "grey90")
  x.vector <- range(data)
  for (b in seq_along(est.means)) {
    if (b == 1) {
      densities <- (dnorm(seq(from = min(x.vector), to = max(x.vector), by = (max(x.vector) - min(x.vector))/100), mean = est.means[b], sd = sqrt(est.vars[b])) * est.priors[b])
      curve(dnorm(x, mean = est.means[b], sd = sqrt(est.vars[b])) * est.priors[b], add = TRUE, col = colors.plot[b])
    }
    else {
      densities <- densities + dnorm(seq(from = min(x.vector), to = max(x.vector), by = (max(x.vector) - min(x.vector))/100), mean = est.means[b], sd = sqrt(est.vars[b])) * est.priors[b]
      curve(dnorm(x, mean = est.means[b], sd = sqrt(est.vars[b])) * est.priors[b], add = TRUE, col = colors.plot[b])
    }
  }
  lines(seq(from = min(x.vector), to = max(x.vector), by = (max(x.vector) - min(x.vector))/100), densities, type = "l")
}