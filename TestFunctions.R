source("Functions.R")

# prior.all <- c(1, 3, 6)
# # prior.all <- c(0.1, 0.3, 0.5)
# # prior.all <- c(0.33, 0.33, 0.33)
# 
# mean.all <- c(100, 200, 300)
# 
# sigma.all <- c(1,1,1)
# 
# data.1 <- SimulateDataset(mean.all, sigma.all, prior.all, 100)
# mean(data.1)

# InitiateEstMeans(2, c(2,2,2,4,4,4))
# est.means <- vector(mode = "numeric", length = 2)
# for (i in 1:2) {
#   est.means[1] <- runif(1, min = 2, max = 4)
# }

# InitiateEstSDs(2, c(2,2,2,4,4,4))

# InitiateEstPriors(2)

k.subs <- 2
means <- c(300, 200)
vars <- c(100, 100) 
priors <- c(0.5, 0.5)
sample.size <- 100
# # data <- c(1,2,2,2,3,5,34,35,36,40,40,40,45)
# data <- c(1,2,2,2,3,5,34,35,36,40,40,40,45)
# est.means <- InitiateEstMeans(k.subs, data)
# # est.means <- c(2,4)
# est.std.devs <- InitiateEstSDs(k.subs, data)
# # est.std.devs <- c(0.33,0.33)
# est.priors <- InitiateEstPriors(k.subs)
# p.membership <- InitiatePMembership(k.subs, length(data))
# old.p.membership <- InitiatePMembership(k.subs, length(data))
# 
# 
# p.membership <- CalculatePMemberships(data, est.std.devs, est.means, est.priors, p.membership)
# 
# est.means <- CalculateEstMeanSubpopulation(data, p.membership, est.means)
# 
# est.std.devs <- CalculateEstSDSubpopulation(data, p.membership, est.means, est.std.devs)

data <- SimulateDataset(means, vars, priors, sample.size)
mixture.model <- RunMixtureModel(data, k.subs, 500)
PlotResults(data, mixture.model$est.means, mixture.model$est.vars, mixture.model$est.priors )




