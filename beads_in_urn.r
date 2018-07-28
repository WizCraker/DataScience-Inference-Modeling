# Interactive Notes for HarvardX Data Science: Inference & Modeling

library(tidyverse)
library(dslabs)
ds_theme_set()

# E(S) = 25p
take_poll(25)

# Standard error of the sum formula
# (1 - 0) sqrt(p(1 - p)) = 
# sqrt(p(1 - p))

# Standard error of the average
# (SE(X-bar)) = sqrt(p(1 - p) / N)


# Parameters and Estimates

N <- 25
p  <- seq(0, 1, length = 100)
se <- sqrt(p * (1 - p) / N)
plot(p,se)


sample_sizes <- c(25, 100, 1000)

for(N in sample_sizes) {
  se <- sqrt(p * (1 - p) / N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
}

# calculate the standard error of the spread 2X-bar - 1
# where p represents the proportion of democratic voters 0.45
N <- 25
p <- 0.45
2 * sqrt(p * (1 - p) /N) # 2X-bar - 1


# Central Limit Theorem
# 1. Distribution function for a sum of draws is approximately normal.
# 2. When dividing a normally distributed random variable by a nonrandom
#    constant the resulting random variable is also normally distributed.
# This implies that the distribution of X-bar is approximately normal.
# Pr * (Z <= .01 / sqrt(p * (1 - p) / N)) - Pr * (Z <= -.01 sqrt(p * (1 - p) / N))
# With CLT instead of p use X-bar in its place x̄
# SE^(x̄) = sqrt(x̄(1-x̄/ N))
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
se

pnorm(0.01/se) - pnorm(-0.01/se)
# With the CLT is determine what sample sizes are better and once we have
# those larger sample sizes we'll be able to provide a very good estimate
# and some very informative probablilites.

# Margin of Error of 2 errors
pnorm(2) - pnorm(-2)

# 95% is the most common value used to define margins of error


# A Monte Carlo simulation for the CLT 
# This will not run due to p is unknown
B <- 10000
N <- 1000
X_hat <- replicate(B, {
    X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
    mean(X)
})

# assume p is 0.45 with poll of 1,00 beads or people
p <- 0.45
N <- 1000
X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
X_hat <- mean(X)

# Then put it into a Monte Carlo simulation
B <- 10000
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  mean(X)
})

# Verify normal distribution with expected value 0.45 and 
# standard error of about 1.5%
mean(X_hat)
sd(X_hat)

# A histogram and qq plot of this X-hat data confirms that the
# normal approximation is accurate as well.

library(gridExtra)
p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color="black")
p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat)) +
  stat_qq(dparams = list(mean=mean(X_hat), sd=sd(X_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1,p2, nrow=1)


# Bias: Why not run a very large poll
# For a very large poll of ~100,000 people theory would tell us that
# we would predict the election almost perfectly. Since the largest
# possible margin of error is about 0.3%
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p, SE)) +
  geom_line()

# Running polls of a sample size of 100,000 is very expensive.
# Also people might lie to you.
# Theory has its limitation
# This is the bias which is 1-2% 


#####################################
# Assessment Chapter 2              #
#####################################

# 1. Write a function called `take_sample` that takes `p` and `N`
# as arguements and returns the average value of a randomly sampled
# population.
take_sample <- function(p, N) {
  sample <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  mean(sample)
}
# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being
# polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average 
# of `N` randomly selected people from a population containing a 
# proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)

###############

# 2. Define `p` as the proportion of Democrats in the population being 
# polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to 
# be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the 
# result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the errors. Print this value to the console.
mean(errors)

###############

# 3. Use hist function to plot a histogram of the values contained
# in the vector errors. Which statement best describes the distribution
# of the errors?
hist(errors)

# C - The errors are symmetrically distributed around 0

###############

# 4. Define `p` as the proportion of Democrats in the population being 
# polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to
# be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the
# expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual
# proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error.
# Print this value to the console. What is the average size of the 
# error if we define the size by taking the absolute value 
# abs(p - x-bar)?
mean(abs(errors))

###############

# 5. Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be 
# replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual 
# proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

###############

# 6. Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p*(1-p)/N)

###############

# 7. Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of
# picking a Democrat ('1') equal to `p`
X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to
# the console.
sqrt(X_bar * (1 - X_bar) / N)

###############

# 8. Create a plot of the largest standard error for N ranging from 
# 100 to 5,000. Based on this plot, how large does the sample size 
# have to be to have a standard error of about 1%?

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N, se)

# B - 500 for sample size to be about 1%

###############

# 9. For N = 100, the CLT tells us that the distribution of X-hat is?

# B - approximately normal with expected value p and standard error 
# sqrt(p * (1 - p) / N)

###############

# 10. Based on the answer from exercise 8, errors $\bat{X} - p$ are:

# B - approximately normal with expected value 0 and standard error
# sqrt(p * (1 - p) / N)

###############

# 11. Define `p` as the proportion of Democrats in the population 
# being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample
# to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual 
# proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal 
# distribution
qqnorm(errors)
qqline(errors)

###############

# 12. If p = 0.45 and N = 100, use the CLT to estimate the proabability
# that X_bar > 0.5.

# Define `p` as the proportion of Democrats in the population 
# being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of 
# Democrats in the population is greater than 0.5. Print this value 
# to the console. Use pnorm to define the probability that a value will be 
# greater than 0.5.
1 - pnorm(0.5, p, sqrt(p * (1 - p) / N))

###############

# 13. Assume you are in a practical situation and you don't know p. 
# Take a sample of size N = 100 and obtain a sample average of
# X-bar = 0.51.
#
# What is the CLT approximation for the probability that your error
# is equal or larger than 0.01?
#
# Calculate the standard error of the sample average using sqrt()
#
# Use pnorm twice to define the probabilities that a value will be 
# less than 0.01 or -0.01.
#
# Calculate the probability that the error will be 0.01 or larger.
#
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(p * (1 - p) / N)

# Calculate the probability that the error is 0.01 or larger
1 - pnorm(0.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)

#####################################
# Assessment Chapter 2 End          #
#####################################

# Confidence intervalls and p-values




