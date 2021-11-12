# Generate data from a known statistical model
# We will use a simple binary logistic regression in the form of logit(pi) = alpha + beta*x
# We will set the values of alpha (intercept) and beta (slope) to be 2 and 0.5, respectively.


# set seed for reproducibility
set.seed(123454321)

# simulate 5000 'x' or 'predictor' values
# the choice of distribution was chosen arbitrarily 
x <- round(runif(5000, 1, 8))

# linear predictor for logistic model 
a <- 2
b <- 0.5
xb <- a + b*x

# probability (inv logit)
p <- 1/(1 + exp(-xb))

# generate outcome (i.e. 1's, 0's) with probability p
y <- rbinom(n = 5000, size = 1, prob = p)

# check if model parameters can be recovered by fitting an actual logistic regression to the simulated data
my_model <- glm(y ~ x, family = binomial)
coefficients(my_model)
