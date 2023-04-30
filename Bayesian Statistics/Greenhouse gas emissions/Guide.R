#Libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(xts)
library(dplyr)
library(data.table)
library(imputeTS)
library(ggplot2)
library(MTS)
library(tseries)
library(forecast)
library(bvartools)
library(coda)
library(Matrix)

data <- read.csv("bayes_data.csv")
data <- apply(data, MARGIN=2, log)
data <- data[,-1]
data <- data.frame(data)

#Check for stationarity
adf.test(data$net_greenhouse_pc)
adf.test(data$GDP.pc)
adf.test(data$res_cap)
adf.test(data$Cattle_heads)

#Take first differences
net_greenhouse_pc <- diff(data$net_greenhouse_pc , differences = 2)
GDP.pc <- diff(data$GDP.pc , differences = 2)
res_cap <- diff(data$res_cap , differences = 2)
Cattle_heads <- diff(data$Cattle_heads , differences = 2)

#Test again
adf.test(net_greenhouse_pc)
adf.test(GDP.pc)
adf.test(res_cap)
adf.test(Cattle_heads)

#Still not stationary, take second differences
net_greenhouse_pc <- diff(data$net_greenhouse_pc , differences = 1)
GDP.pc <- diff(data$GDP.pc , differences = 1)
res_cap <- diff(data$res_cap , differences = 1)
Cattle_heads <- diff(data$Cattle_heads , differences = 1)

#Test again
adf.test(net_greenhouse_pc) #stationary I(2)
adf.test(GDP.pc) #not stationary
adf.test(res_cap) #not stationary
adf.test(Cattle_heads) #stationary I(2)

data <- cbind(net_greenhouse_pc, Cattle_heads)
data <- ts(data, frequency = 1)
set.seed(100)

#Prepare the data
temp <- gen_var(data, p = 2, deterministic = "const")
y <- temp$data$Y
x <- temp$data$Z

k <- NROW(y)
tt <- ncol(y)
z <- kronecker(t(x), diag(1, k))
m <- ncol(z)

#Specify the priors
mu_prior <- matrix(0, m) # Prior mean
v_i_prior <- diag(1, m) # Prior variance
diag(v_i_prior)[m - k + 1:k] <- 1 / 10 # Deterministics

df_prior <- k + 4
df_post <- tt + df_prior
scale_prior = diag(1, k)

#Initial values
ols <- tcrossprod(y, x) %*% solve(tcrossprod(x))
epsilon <- y - ols %*% x
sigma <- tcrossprod(epsilon) / tt
sigma_i <- solve(sigma)