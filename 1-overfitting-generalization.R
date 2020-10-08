library(dplyr)
library(tidyr)
set.seed(12604)

# This part of the lab will illustrate a problem with over-fitting
# models: over-fit models tend to perform worse on novel data.

# To illustrate, let's take a look at a simple statistical model.

# I'm going to start by defining a function that takes an
# observation, x, and generates data y.
# The data follow a cubic polynomial function (i.e., a curvy line.)

data.generating.process <- function(x) {
  b.0 <- 0
  b.1 <- 7
  b.2 <- -2
  b.3 <- 0.13
  y <- b.0 + b.1*x + b.2*x^2 + b.3*x^3
  return(y)
}

# Now I'll create a set of values, x, and use the
# data generating process with some random noise
# to create a simulated data set.

simulated.data <- data.frame(x = seq(0,12,0.2)) %>%
  mutate(y = data.generating.process(x) + rnorm(n(), 0, 3))

# Let's generate a plot of the data.

plot(y ~ x, data=simulated.data)

# We can add the true data generating process (minus noise)
# to this plot to visualize the how the noise affected
# the data

simulated.data <- simulated.data %>%
  mutate(y.true = data.generating.process(x))

points(y.true ~ x, data=simulated.data, col="red", type="l")

# This fit looks pretty good, but that's because we're generating
# the data from this exact model! 

## PREDICTION FUNCTIONS ########

# Let's start by generalizing what I've done above to a few different models.
# We want to be able to pass in a set of parameters and some data, and get
# the model's predictions back.

# Create prediction functions for the 1-degree model, 3-degree model, 
# and 7-degree model.

model.1.prediction <- function(params, x){
  return(params[1] + params[2]*x)
}

model.3.prediction <- function(params, x){
  return(params[1] + params[2]*x + params[3]*x^2 + params[4]*x^3)
}

model.7.prediction <- function(params, x){
  return(params[1] + params[3]*x + params[3]*x^2 + params[4]*x^3 + params[5]*x^4 + params[6]*x^5 + params[7]*x^6 + params[8]*x^7)
}
  

## DISCREPANCY FUNCTIONS #######

# Let's write some code that will allow us to fit models of different
# complexity to this data. Let's write discrepancy functions for the
# same models we wrote prediction functions for above

# Let's use RMSE for this example.

model.1.discrepancy <- function(params, data){
  rmse <- data %>%
    mutate(y.pred = model.1.prediction(params, x)) %>%
    mutate(sq.err = (y.pred - y)^2) %>%
    pull(sq.err) %>%
    mean() %>%
    sqrt()
  
  return(rmse)
}

model.3.discrepancy <- function(params, data){
  rmse <- data %>%
    mutate(y.pred = model.3.prediction(params, x)) %>%
    mutate(sq.err = (y.pred - y)^2) %>%
    pull(sq.err) %>%
    mean() %>%
    sqrt()
  
  return(rmse)
  
}

model.7.discrepancy <- function(params, data){
  rmse <- data %>%
    mutate(y.pred = model.7.prediction(params, x)) %>%
    mutate(sq.err = (y.pred - y)^2) %>%
    pull(sq.err) %>%
    mean() %>%
    sqrt()
  
  return(rmse)
}

## CREATING A HOLDOUT SET ##

# To test the ability of the model's to generalize to new data, we will mark
# about half the data for training and half for testing

simulated.data <- simulated.data %>% 
  mutate(training.set = sample(c(T,F), n(), replace=T))

train.data <- simulated.data %>% filter(training.set == T)

test.data <- simulated.data %>% filter(training.set == F)

## FITTING THE MODELS ######

# first, just plot the train and test data in different colors

plot(y ~ x, data = test.data)
points(y ~ x, data = train.data, col = "red")

# fill in the code here to run the parameter search

model.1.result <- optim(model.1.discrepancy, par = c(0,0), data=train.data)

simulated.data <- simulated.data %>%
  mutate(y.pred.1 = model.1.prediction(model.1.result$par, x))

points(y.pred.1 ~ x, data=simulated.data, type="l", col="purple")


model.3.result <- optim(model.3.discrepancy, par=c(0,0,0,0), data=train.data, control=list(maxit=2000))

simulated.data <- simulated.data %>%
  mutate(y.pred.3 = model.3.prediction(model.3.result$par, x))

points(y.pred.3 ~ x, data=simulated.data, type="l", col="blue")



model.7.result <-  optim(model.7.discrepancy, par=c(0,0,0,0,0,0,0,0), data=train.data, control=list(maxit=5000))

simulated.data <- simulated.data %>%
  mutate(y.pred.7 = model.7.prediction(model.7.result$par, x))

points(y.pred.7 ~ x, data=simulated.data, type="l", col="green")


## CHECK FIT ON HOLDOUT SET ####

# this code will take our model fits and calculate the RMSE for each model
# on the test data

simulated.data %>%
  filter(training.set == F) %>%
  pivot_longer(5:7, names_to = "model.complexity", values_to = "prediction") %>%
  mutate(sq.error = (prediction - y)^2) %>%
  group_by(model.complexity) %>%
  summarize(rmse = sqrt(mean(sq.error)))

# which model has the lowest RMSE on the test data?

# try changing the random.seed() at the top of the script and running it 
# a few times. which model consistently has the lowest RMSE?
  
  



