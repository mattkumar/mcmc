# Examining the effect of different starting values
# This will be a self contained script

# Libs
library(tidyverse)
library(hrbrthemes)
library(viridisLite)
library(gganimate)

# Data
# set seed for reproducibility
set.seed(123454321)

# simulate 5000 'x' or 'predictor' values
# the choice of distribution was chosen arbitrarily 
x <- round(runif(5000, 1, 8))

# linear predictor for logistic model 
a <- 0.25
b <- 0.5
xb <- a + b*x

# probability (inv logit)
p <- 1/(1 + exp(-xb))

# generate outcome (i.e. 1's, 0's) with probability p
y <- rbinom(n = 5000, size = 1, prob = p)


# Bayesian Logistic Regression Analysis
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

# MCMC sampler
runmcmc <- function(n_steps, starting_vals) {

# a place holder to collect results of the analysis  
mc <- matrix(ncol = 2, nrow = n_steps)
dc <- matrix(ncol = 1, nrow = n_steps)                
                    
# specify starting values
mc[1,] = starting_vals
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
mc %>% 
  as.data.frame() %>%
  select(Intercept = 1,
         Slope = 2) %>%
  cbind(dc %>% 
          as.data.frame() %>% 
          select(Decision = 1)) %>%
  mutate(z = row_number())

}

# Run the sampler 4 times, each with different starting values
c1 <- runmcmc(n_steps = 10000, starting_vals = c(3,3))
c2 <- runmcmc(n_steps = 10000, starting_vals = c(-3,3))
c3 <- runmcmc(n_steps = 10000, starting_vals = c(3,-3))
c4 <- runmcmc(n_steps = 10000, starting_vals = c(-3,-3))

# Wrangle
alldata <- rbind(c1 %>% mutate(chain = "3, 3"),
                 c2 %>% mutate(chain = "-3, 3"),
                 c3 %>% mutate(chain = "3, -3"),
                 c4 %>% mutate(chain = "-3, -3"))

# Plot
ggplot(filter(alldata, z <= 5000), aes(x = Intercept, y = Slope,  group = interaction(z, chain), fill = chain)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = viridisLite::viridis(4, alpha = 0.5)) +
  hrbrthemes::theme_ipsum_rc() +
  scale_y_continuous(breaks = seq(-4,4,1), limits = c(-4,4)) +
  scale_x_continuous(breaks = seq(-4,4,1), limits = c(-4,4)) +
  theme(legend.position = "none")  +
  labs(fill = "",
       y = expression(beta[1]),
       x = expression(alpha),
       title = 'Visualizing MCMC in Bayesian Logistic Regression',
       subtitle = expression(paste("Estimating logit(",pi,") = 0.25 + 0.5x with 4 unique starting values.")),
       caption = expression(paste("N = 5000 iterations"))) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5)) +
  transition_reveal(z) -> anim

animate(anim, res = 110,  width = 1200, height = 900, nframes = 250)

  