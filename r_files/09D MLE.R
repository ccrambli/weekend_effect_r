##### Code for illustrating maximum likelihood estimation
##### written by Professor Dieker of Columbia University

library("stats4")

pedestrianDeaths <- c(6,11,6,5,17,9,6)
minusLogLikelihood <- function(lambda) -sum(stats::dpois(pedestrianDeaths, lambda, log = TRUE))

minusLogLikelihood(1)

minusLogLikelihoodVec <- Vectorize(minusLogLikelihood)
curve(minusLogLikelihoodVec, from=0.01, to=25)

MLE <- mle(minusLogLikelihood, start = list(lambda = 5), nobs = NROW(pedestrianDeaths))


methods(class="mle")

summary(MLE)
