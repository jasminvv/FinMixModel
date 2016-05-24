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
  plot(1, type = "n", xlab = "", ylab = "", xlim = c(min(means) - max(std.devs) * 3, max(means) + max(std.devs) * 3), ylim = c(0, 1))
  if (length(data) > 0) {  
    hist(data, breaks = length(data) / 3, xlim = range(data), ylim = NULL, prob = TRUE, xlab = "Data values", main = "Histogram of Data and Density Graphs for Subpopulations")
  }
  if (length(means) > 0 && length(std.devs) > 0 && length(priors) > 0) {
    for (b in seq_along(means)) {
      curve(dnorm(x, mean = means[b], sd = std.devs[b]) * priors[b], add = TRUE)
    }
  }
}

# ***SIMPLE MIXTURE MODEL: Functions:

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
  # and returns a vector containing k random standard deviations between
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

RunMixtureModel <- function(data, num.subpopulations, max.times){
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
  
  mixture.model <- list("est.means" = est.means, "est.vars" = est.vars, "p.membership" = p.membership)
  
  return(mixture.model)
}

# PLOT RESULTS

PlotResults <- function(data, est.means, est.vars, est.priors){
  if (! missing(data)) {  
    hist(data, breaks = length(data) / 3, xlim = range(data), ylim = NULL, prob = TRUE, xlab = "Data values", main = "Histogram of Data and Density Graphs for Subpopulations")
  }
  if (length(est.means) > 0 && length(est.vars) > 0 && length(est.priors) > 0) {
    for (b in seq_along(est.means)) {
      curve(dnorm(x, mean = est.means[b], sd = sqrt(est.vars[b])) * est.priors[b], add = TRUE)
    }
  }
}