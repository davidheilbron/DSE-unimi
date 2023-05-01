
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
library(MCMCpack)
library(reshape2)

#Load data
bayes_data <- read.csv("bayes_data.csv")
set.seed(100)

# Set the lag of the VAR
p <- 2

# Gibbs sampler iterations
nsim   <- 20000
burnin <- 1000


Y_tmp <- bayes_data
Yraw  <- as.matrix(Y_tmp[,-1])
Dates <- Y_tmp[,1]
Y0 <- as.matrix(Yraw[1:4,1]) # save the first 4 obs as the initial conditions
Y  <- as.matrix(Yraw[5:nrow(Yraw),1])

T <- nrow(Y) #number of observations
n <- ncol(Y) #number of endogenous features
m <- 3 #number of exogenous features
y <- as.vector(t(Y)) #transform the features matrix into a single vector

k <- n*p + m*p + 1 # number of coefficients in each equation
n_hz  <- 5 # number of horizons for IRs

# Prior hyperparameters
nu0 <- 2*(m + n) #ask the professor
S0 <- diag(m + n)
beta0 <- numeric(n*k)
# precision for coefficients = 1; for intercepts = 1/10
tmp <- rep(1,k*n)
tmp[seq(from = 1,to = k*n, by = k)] <- 1/10
iVbeta <- diag(tmp, nrow = k*n, ncol = k*n)

# Create the lagged variable
tmpY    <- rbind(as.matrix(Y0[(nrow(Y0) - p + 1):nrow(Y0),]), Y)
X_tilde <- matrix(0, nrow = T, ncol = (n + m)*p)

mlag_1 <- Yraw[4:(nrow(Yraw)-1),]
mlag_2 <- Yraw[3:(nrow(Yraw)-2),]
X_tilde <- cbind(mlag_1, mlag_2)
X_tilde <- cbind(rep(1,T), X_tilde)

SURform2 <- function(X, n) {
  repX <- kronecker(X, matrix(1, ncol = n))
  r <- nrow(X)
  c <- ncol(X)
  idi <- kronecker(1:(r*n), matrix(1, ncol = c))
  idj <- matrix(rep(1:(n*c), each = r), ncol = 1)
  X_out <- sparseMatrix(i = idi, j = idj, x = c(repX))
  return(X_out)
}

#X <- SURform2(X_tilde,n)
#X <- kronecker(diag(nrow(X_tilde)), X_tilde)
X <- X_tilde

qr(X)

# Storage of variables
store_Sig  <- array(0, dim = c(nsim,(n + m),(n + m)))
store_beta <- array(0, dim = c(nsim,k*n))
store_yIR  <- matrix(0, nrow = n_hz, ncol = (n + m))

# Initialize the chain 
beta <- solve(t(X) %*% X) %*% t(X) %*% y
e <- (y - X %*% beta)
Sig <- t(e) %*% e/T
iSig <- solve(Sig)
isig <- iSig[1,1]

construct_IR <- function(beta, Sig, n_hz, shock) {
  n <- nrow(Sig)
  p <- (nrow(beta) / n - 1) / n
  CSig <- chol(Sig, lower = TRUE)
  tmpZ1 <- matrix(0, ncol = n, nrow = p)
  tmpZ <- matrix(0, ncol = n, nrow = p)
  Yt1 <- CSig %*% shock
  Yt <- rep(0, n)
  yIR <- matrix(0, ncol = n_hz, nrow = n)
  yIR[1,] <- t(Yt1)
  
  for (t in 2:n_hz) {
    # update the regressors
    tmpZ <- rbind(Yt, tmpZ[1:(p-1),])
    tmpZ1 <- rbind(Yt1, tmpZ1[1:(p-1),])
    
    # evolution of variables if a shock hits
    e <- CSig %*% rnorm(n)
    Z1 <- as.vector(t(tmpZ1))
    Xt1 <- kronecker(diag(n), cbind(1, Z1))
    Yt1 <- Xt1 %*% beta + e
    
    # evolution of variables if no shocks hit
    Z <- as.vector(t(tmpZ))
    Xt <- kronecker(diag(n), cbind(1, Z))
    Yt <- Xt %*% beta + e
    
    # the IR is the difference of the two scenarios
    yIR[t,] <- t(Yt1 - Yt)
  }
  return(yIR)
}



## Gibbs sampler 
tic <- Sys.time()
for (isim in 1:(nsim + burnin)) {
  # Posterior of beta \sim Normal
  XiSig    <-  t(X) * isig
  XiSigX   <- (t(X) %*% X) * isig
  Kbeta    <- iVbeta + XiSigX # posterior variance
  beta_hat <- solve(Kbeta) %*% (iVbeta %*% beta0 + XiSig %*% y) # posterior mean
  beta     <- beta_hat + chol2inv(chol(Kbeta)) %*% rnorm(n*k,0,1)
  
  # posterior of Sigma \sim iWishart
  e    <- matrix(y - X %*% beta)    
  #Sig  <- riwish(nu0 + T, S0 + t(e) %*% e) 
  Sig  <- riwish(nu0 + T, S0)
  iSig <- solve(Sig)
  
  # Count the number of iterations
  #if (isim %% 1000 == 0) {
  #  elapsed_time <- Sys.time() - start_time
  #  print(paste("Iteration:", isim, "of", nsim + burnin, ". Elapsed time is", elapsed_time, "seconds."))
  #}
  
  #Store the parameters
  if (isim > burnin) {
    isave <- isim - burnin
    store_beta[isave,] <- beta
    store_Sig[isave,,] <- Sig
    
    # Compute impulse-responses
    CSig <- chol(Sig, lower = TRUE)
    # 100 basis pts rather than 1 std. dev.
    shock <- c(0, 0, 1) / CSig[n, n]
    yIR <- construct_IR(beta, Sig, n_hz, shock)
    store_yIR <- store_yIR + yIR
  }
}
