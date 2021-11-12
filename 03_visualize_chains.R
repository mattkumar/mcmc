# Visualizing MCMC Results
# We will first visualize the chain step-by-step for the first 100 iterations
# We will then visualize the entire chain
# Note: Requires 01_simulate_data_from_model.R and 02_mcmc_sampler.R be ran

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(ggpp)

# Source in script containing data and results
source("01_simulate_data_from_model.R")
source("02_mcmc_sampler.R")


# Visualization 1 - the first 100 iterations
# get result data
results_tmp <- results %>%
  mutate(`Prev Intercept` = lag(Intercept), 
         `Prev Slope` = lag(Slope)) %>%
  select(Iteration = z, 
         Intercept, Slope, 
         `Prev Intercept`, 
         `Prev Slope`, 
         Decision)

# create placeholder for display table
table_data <- data.frame(Iteration = seq(1,100,1),
                         placeholder = NA)

for(i in 1:nrow(table_data)) {
  table_data$placeholder[i] <- results_tmp %>% 
    filter(Iteration == i) %>%  
    list()
}

# add display table data back to main result data 
results_tmp2 <- results_tmp %>% 
  left_join(table_data)

# plot
viz1 <- ggplot(results_tmp2[1:100,], aes(x = Intercept, y = Slope, group = Iteration)) +
  geom_segment(aes(xend = lag(Intercept), yend = lag(Slope)), color = "black", 
               arrow = arrow(type = "closed", 
                             ends = "first", 
                             length = unit(0.27, "cm"))) +
  geom_point(aes(fill = Decision), shape = 21, size = 4) +
  geom_table(y = 1.6, x = 1.375, aes(label = placeholder), size = 4) + 
  scale_fill_manual(values = c(ft_cols$green, ft_cols$red), limits = c('Accept','Reject')) +
  theme_ipsum_rc() +
  scale_y_continuous(limits = c(0,1.8), breaks = seq(0,1.8, 0.2)) +
  scale_x_continuous(limits = c(-0.5,2), breaks = seq(-0.5,2, 0.5)) +
  labs(fill = "",
       y = expression(beta[1]),
       x = expression(alpha),
       title = 'Visualizing MCMC in Bayesian Logistic Regression',
       subtitle = expression(paste("A view into the behavior of a single markov chain when estimating logit(",pi,") = ",alpha," + ",beta[1],"X with known values." )),
       caption = expression(paste("The first N = 100 iterations"))) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5)) +
  annotate(geom = "curve", x = 0.65, y = 0.25, xend = 0.02, yend = 0.02, 
           curvature = .3, linetype = "dashed", arrow = arrow(type = "closed", ends = "last", length = unit(0.27, "cm"))) +
  annotate(geom = "text", x = 0.65, y = 0.25, label = "Initial Values", hjust = "left")

anim1 <- viz1 + transition_reveal(Iteration)
animate(anim1, res = 110,  width = 1200, height = 900, fps = 1)



# Visualization 2 - the full chain
viz2 <- ggplot(results[1:10000,], aes(x = Intercept, y = Slope, group = z)) +
          geom_segment(aes(xend = lag(Intercept), yend = lag(Slope)), color = "black", 
                       arrow = arrow(type = "closed", 
                                     ends = "first", 
                                     length = unit(0.27, "cm"))) +
          geom_point(aes(fill = Decision), shape = 21, size = 4) +
          scale_fill_manual(values = c(ft_cols$green, ft_cols$red), limits = c('Accept','Reject')) +
          theme_ipsum_rc() +
          labs(fill = "",
               y = expression(beta[1]),
               x = expression(alpha),
               title = 'Visualizing MCMC in Bayesian Logistic Regression',
               subtitle = expression(paste("A view into the behavior of a single markov chain when estimating logit(",pi,") = ",alpha," + ",beta[1],"X with known values." )),
               caption = expression(paste("N = 10,000 iterations"))) +
          theme(legend.position = "top",
                axis.title.x = element_text(size = 20, hjust = 0.5),
                axis.title.y = element_text(size = 20, hjust = 0.5)) +
          annotate(geom = "curve", x = 0.65, y = 0.25, xend = 0.02, yend = 0.02, 
                   curvature = .3, linetype = "dashed", arrow = arrow(type = "closed", ends = "last", length = unit(0.27, "cm"))) +
          annotate(geom = "text", x = 0.65, y = 0.25, label = "Initial Values", hjust = "left")

anim2 <- viz2 + transition_reveal(z)
animate(anim2, res = 110,  width = 1200, height = 900, nframes = 250)
 
