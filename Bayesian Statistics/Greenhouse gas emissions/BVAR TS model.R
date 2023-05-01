##############################################################################
#BVAR ATTEMPT NUMBER 2

library(bvartools)
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
library(spam)
library(vars)

bayes_1 <- read.csv("bayes_data_1.csv")

bayes <- bayes_1[,-1]
head(bayes) #check head
bayes_ts <- as.ts(bayes)
plot(bayes_ts)
summary(bayes_ts)

VARselect(bayes_ts, lag.max = 4,type = "const") #1 lag

bmodel <- gen_var(bayes_ts, p = 2, deterministic = "const")

y <- t(bmodel$data$Y)
x <- t(bmodel$data$Z)

#Frequentist estimation
A_freq <- tcrossprod(y, x) %*% solve(tcrossprod(x)) # Calculate estimates
round(A_freq, 3) # Round estimates and print

u_freq <- y - A_freq %*% x
u_sigma_freq <- tcrossprod(u_freq) / (ncol(y) - nrow(x))
round(u_sigma_freq, 2)


#Bayesian estimator
# Reset random number generator for reproducibility
set.seed(1234567)

iter <- 30000 # Number of iterations of the Gibbs sampler
burnin <- 15000 # Number of burn-in draws
store <- iter - burnin

tt <- ncol(y) # Number of observations
k <- nrow(y) # Number of endogenous variables
m <- k * nrow(x) # Number of estimated coefficients

# Set priors
a_mu_prior <- matrix(0, m) # Vector of prior parameter means
a_v_i_prior <- diag(1, m) # Inverse of the prior covariance matrix

u_sigma_df_prior <- 6 # Prior degrees of freedom
u_sigma_scale_prior <- diag(1, k) # Prior covariance matrix
u_sigma_df_post <- tt + u_sigma_df_prior # Posterior degrees of freedom

# Initial values
u_sigma_i <- solve(u_sigma_freq)

# Data containers for posterior draws
draws_a <- matrix(NA, m, store)
draws_sigma <- matrix(NA, k * k, store)

# Start Gibbs sampler
for (draw in 1:iter) {
  # Draw conditional mean parameters
  a <- post_normal(y, x, u_sigma_i, a_mu_prior, a_v_i_prior)
  
  # Draw variance-covariance matrix
  u <- y - matrix(a, k) %*% x # Obtain residuals
  u_sigma_scale_post <- solve(u_sigma_scale_prior + tcrossprod(u))
  u_sigma_i <- matrix(rWishart(1, u_sigma_df_post, u_sigma_scale_post)[,, 1], k)
  u_sigma <- solve(u_sigma_i) # Invert Sigma_i to obtain Sigma
  
  # Store draws
  if (draw > burnin) {
    draws_a[, draw - burnin] <- a
    draws_sigma[, draw - burnin] <- u_sigma
  }
}

A <- rowMeans(draws_a) # Obtain means for every row
A <- matrix(A, k) # Transform mean vector into a matrix
A <- round(A, 3) # Round values
dimnames(A) <- list(dimnames(y)[[1]], dimnames(x)[[1]]) # Rename matrix dimensions

A # Print

Sigma <- rowMeans(draws_sigma) # Obtain means for every row
Sigma <- matrix(Sigma, k) # Transform mean vector into a matrix
Sigma <- round(Sigma, 2) # Round values
dimnames(Sigma) <- list(dimnames(y)[[1]], dimnames(y)[[1]]) # Rename matrix dimensions

Sigma # Print


bvar_est <- bvar(y = bmodel$data$Y, x = bmodel$data$Z, A = draws_a[1:18,],
                 C = draws_a[19:21, ], Sigma = draws_sigma)

summary(bvar_est)

bvar_est <- thin_posterior(bvar_est, thin = 15)

#IMPULSE RESPONSE ANALYSIS
FEIR <- bvartools::irf(bvar_est, impulse = "industrial_production_2", response = "net_greenhouse_pc_d2", n.ahead = 5)
plot(FEIR, main = "Forecast Error Impulse Response", xlab = "Period", ylab = "Response")

OIR <- bvartools::irf(bvar_est, impulse = "industrial_production_2", response = "net_greenhouse_pc_d2", n.ahead = 8, type = "oir")
plot(OIR, main = "Orthogonalised Impulse Response", xlab = "Period", ylab = "Response")

GIR <- bvartools::irf(bvar_est, impulse = "industrial_production_2", response = "net_greenhouse_pc_d2", n.ahead = 8, type = "gir")
plot(GIR, main = "Generalised Impulse Response", xlab = "Period", ylab = "Response")

#ERROR VARIANCE DECOMPOSITION
bvar_fevd_oir <- bvartools::fevd(bvar_est, response = "net_greenhouse_pc_d2")
plot(bvar_fevd_oir, main = "OIR-based FEVD of consumption")

bvar_fevd_gir <- bvartools::fevd(bvar_est, response = "net_greenhouse_pc_d2", type = "gir")
plot(bvar_fevd_gir, main = "GIR-based FEVD of consumption")

#FORECASTS
bvar_pred <- predict(bvar_est, n.ahead = 10, new_d = rep(1, 10))
plot(bvar_pred)
