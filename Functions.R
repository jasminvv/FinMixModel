# SIMULATE DATA: Functions: SetDrawProbability and SimulateDataset

SetDrawProbability <- function(prior.all) {
  # Function that takes a vector of k priors, and returns a 
  # vector where the priors are added up and divided by the total
  # (newprior2 = prior1+2/total, newprior3 = newprior2+prior3/total, etc)
  
  draw.p <- vector(mode = "numeric", length = length(prior.all))
  
  for (b in seq_along(prior.all)) {
    if (b == 1) {
      draw.p[b] <- prior.all[b]/sum(prior.all)
    }
    else {
      draw.p[b] <- draw.p[b - 1] + (prior.all[b] / sum(prior.all))
    }
  }
  
  return(draw.p)
}

SimulateDataset <- function(mean.all, sigma.all, prior.all, n) {
  # Function that takes a vector of k means, a vector of k standard deviations,
  # a vector of k priors, and a sample size n. It returns a vector of n simulated 
  # observations taken from a population with k subpopulations (mean, sd and prior from input).
  
  if (length(mean.all) != length(sigma.all) || length(mean.all) != length(prior.all) || length(sigma.all) != length(prior.all)) {
    stop("Not all parameters have been set.")
  }
  
  sample.dataset <- vector(mode="numeric", length=n)
  draw.p <- SetDrawProbability(prior.all)
  
  for (i in 1:n) {
    random.number = runif(1, min = 0, max = 1)
    for (b in seq_along(draw.p)) {
      if (random.number <= draw.p[b]) {
        sample.dataset[i] <- rnorm(1, mean.all[b], sigma.all[b])
        break
      }
      #else {
      # next
      #}
    }	  
  }
  
  return(sample.dataset)
}
