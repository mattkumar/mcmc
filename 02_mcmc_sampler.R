# Bayesian Logistic Regression and MCMC
# We will first specify the components of the Bayesian logistic regression
# We will then implement the random walk Metropolis-Hastings algorithm to estimate the parameters
# Note: Requires 01_simulate_data_from_model.R be ran


library(tidyverse)

# Source script containing data
source("01_simulate_data_from_model.R")


# Specify Bayesian Logistic Regression model components: likelihood, priors, (generic) posterior and candidate generating distributions
# Each component is defined as a function, which is then implemented in the algorithm directly in the next section

# likelihood
likelihood <- function(intercept, slope){
  xb <- intercept + slope*x
  p <- 1/(1 + exp(-xb))
  sum(dbinom(y, size=1, prob=p, log=TRUE))
}

# priors 
prior <- function(intercept, slope){
  dnorm(intercept, sd = 1, log = T) + dnorm(slope, sd = 1, log = T)
}

# posterior
posterior <- function(intercept, slope){
  likelihood(intercept, slope) + prior(intercept, slope)
}

# candidate generation
candidate <- function(intercept, slope){
  intercept <- rnorm(1, mean = intercept, sd = 0.1) 
  slope <- rnorm(1, mean = slope, sd = 0.1)
  c(intercept, slope)
}

# Implement the Random Walk Metropolis-Hastings algorithm

# Setup:
# number of steps to run the analysis
n_steps <- 10000

# a place holder to collect results of the analysis  
mc <- matrix(ncol = 2, nrow = n_steps)
dc <- matrix(ncol = 1, nrow = n_steps)

# specify starting values
mc[1,] = c(0,0)
dc[1,] = "Init"


# Random Walk Metropolis-Hastings
for(i in 2:n_steps) {
  # draw a proposal candidate
  cand <- candidate(intercept = mc[i-1, 1], slope = mc[i-1, 2])
  
  # compute acceptance ratio
  ratio = exp(posterior(intercept = cand[1], slope = cand[2]) - posterior(intercept = mc[i-1, 1], slope = mc[i-1, 2]))
  
  # evaluate acceptance ratio
  if (runif(1) < ratio) {
    # if accept, keep proposal candidates, rinse + repeat 
    mc[i,] = c(cand[1], cand[2])
    dc[i,] = "Accept"
  } else {
    # if reject, use previous iterations points, rinse + repeat
    mc[i,] = c(mc[i-1,1], mc[i-1,2])
    dc[i,] = "Reject"
  }
}

# Process results of the chain into a data frame
results <- mc %>% 
  as.data.frame() %>%
  select(Intercept = 1,
         Slope = 2) %>%
  cbind(dc %>% 
          as.data.frame() %>% 
          select(Decision = 1)) %>%
  mutate(z = row_number())

# Check our algorithm results against what glm() produces
rbind(mean(results$Intercept), mean(results$Slope))
glm(y ~ x, family = binomial)

# Not bad! We didn't run the chain for that long or address burn in here. this is all meant to be very crude.
