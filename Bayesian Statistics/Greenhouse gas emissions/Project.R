#Script for building the database for the Bayesian Analysis project

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
library(BMR)
library(tseries)
library(forecast)
library(bvartools)
library(coda)

data <- read_excel("data.xlsx", sheet = 'Data')

#Turn character features to numeric
data$Pigs_heads <- as.numeric(data$Pigs_heads)
data$Head_sheep <- as.numeric(data$Head_sheep)
data$Head_goat <- as.numeric(data$Head_goat)
data$Share_of_land_under_permanent_crops <- as.numeric(data$Share_of_land_under_permanent_crops)
data$Fertilizer_used_per_area_of_cropland <- as.numeric(data$Fertilizer_used_per_area_of_cropland)
data$Share_in_land_area_Forest_Land <- as.numeric(data$Share_in_land_area_Forest_Land)
str(data)
raw <- data

#Data imputation
data$Rail_tracks_KM <- na_interpolation(data$Rail_tracks_KM, option = 'spline')
data %>% ggplot(aes(Year, Rail_tracks_KM)) + geom_line()

data %>% ggplot(aes(Year, Total_freight_loaded_and_unloaded)) + geom_line()
data$Total_freight_loaded_and_unloaded <- na_interpolation(data$Total_freight_loaded_and_unloaded, option = 'spline')

data %>% ggplot(aes(Year, Chicken_heads)) + geom_line()
data$Chicken_heads <- na_interpolation(data$Chicken_heads, option = 'spline')
data %>% ggplot(aes(Year, Chicken_heads)) + geom_line()

data %>% ggplot(aes(Year, Turkeys_heads)) + geom_line()
data$Turkeys_heads <- na_interpolation(data$Turkeys_heads, option = 'spline')
data %>% ggplot(aes(Year, Turkeys_heads)) + geom_line()

imp <- data

str(data)
summary(data)

#Graphs
par(mfrow = c(2, 2))
plot(x = data$Year, y = data$net_greenhouse_pc, type = "l" )
plot(x = data$Year, y = data$environmental_taxes, type = "l")
plot(x = data$Year, y = data$`GDP pc`, type = "l")
plot(x = data$Year, y = data$industrial_production, type = "l")

par(mfrow = c(2, 2))
plot(x = data$Year, y = data$energy_imp_dep, type = "l" )
plot(x = data$Year, y = data$naturalgas_imports, type = "l")
plot(x = data$Year, y = data$oil_imports, type = "l")
plot(x = data$Year, y = data$total_energy_supply, type = "l")

par(mfrow = c(2, 2))
plot(x = data$Year, y = data$gross_electricity_production, type = "l" )
plot(x = data$Year, y = data$hydro_capacity, type = "l")
plot(x = data$Year, y = data$geothermal_capacity, type = "l")
plot(x = data$Year, y = data$wind_capacity, type = "l")

par(mfrow = c(2, 2))
plot(x = data$Year, y = data$solar_capacity, type = "l" )
plot(x = data$Year, y = data$biofuels_capacity, type = "l")
plot(x = data$Year, y = data$biogas_capacity, type = "l")
plot(x = data$Year, y = data$waste_capacity, type = "l")

par(mfrow = c(2, 2))
plot(x = data$Year, y = data$Bovine_heads, type = "l" )
plot(x = data$Year, y = data$Pigs_heads, type = "l")
plot(x = data$Year, y = data$Share_of_land_under_permanent_crops, type = "l")
plot(x = data$Year, y = data$Area_harvested_Rice, type = "l")

par(mfrow = c(2, 2))
plot(x = data$Year, y = data$Fertilizer_used_per_area_of_cropland, type = "l" )
plot(x = data$Year, y = data$Share_in_land_area_Forest_Land, type = "l")
plot(x = data$Year, y = data$Rail_tracks_KM, type = "l")
plot(x = data$Year, y = data$Length_of_motorways, type = "l")

par(mfrow = c(2, 2))
plot(x = data$Year, y = data$Number_of_motorcycle, type = "l" )
plot(x = data$Year, y = data$Total_freight_loaded_and_unloaded, type = "l")
plot(x = data$Year, y = data$Head_sheep, type = "l")
plot(x = data$Year, y = data$Head_goat, type = "l")

#Merge RES capacity and Livestock features
data <- data %>% 
  mutate(livestock_heads = Pigs_heads + Head_sheep + Head_goat + 
           Chicken_heads + Turkeys_heads + Cattle_heads + Buffalo_head) %>% 
  select(!c(Pigs_heads, Head_goat,Head_sheep, Chicken_heads,Turkeys_heads,Cattle_heads,Buffalo_head))

data <- data %>% 
  mutate(res_capacity = hydro_capacity + geothermal_capacity + 
           wind_capacity + solar_capacity + biofuels_capacity + 
           biogas_capacity + waste_capacity) %>% 
  select(!c(hydro_capacity, geothermal_capacity,
              wind_capacity, solar_capacity, biofuels_capacity,
              biogas_capacity, waste_capacity))

str(data)
par(mfrow = c(1, 2))
plot(x = data$Year, y = data$livestock_heads, type = "l" )
plot(x = data$Year, y = data$res_capacity, type = "l")

final <- data

#Scale data
#data <- data %>% mutate_at(c('net_greenhouse_pc'), ~(scale(.) %>% as.vector))

par(mfrow = c(1,1))


########################################################################
#BAYESIAN ATTEMPT #1

bayes <- data %>% select(Year, net_greenhouse_pc, industrial_production,
                         naturalgas_imports, total_energy_supply)

net_greenhouse_pc <- diff(bayes$net_greenhouse_pc, differences = 2)
industrial_production <- diff(bayes$industrial_production, differences = 2)
naturalgas_imports <- diff(bayes$naturalgas_imports, differences = 2)
total_energy_supply <- diff(bayes$total_energy_supply, differences = 2)

bayes <- as.data.frame(cbind(net_greenhouse_pc, industrial_production,
                         naturalgas_imports, total_energy_supply))

head(bayes) #check head
bayes_sca <- as.data.frame(apply(bayes, MARGIN = 2, scale)) #Apply scaling

write.csv(bayes_sca, 'bayes_data.csv')
plot(bayes_sca)

bayes_ts <- as.ts(bayes_sca)
is.ts(bayes_ts)
ts(bayes_ts)
plot(bayes_ts)
summary(bayes_ts)


###MODEL
bayesian_model <- gen_var(bayes_ts, p = 2, deterministic = "const")
y <- t(bayesian_model$data$Y)
x <- t(bayesian_model$data$Z)
x
y
bayesian_model
sed.seed(1234)
iter <- 10000
burnin <- 1000
store <- iter - burnin
tt <- ncol(y)
k <- nrow(y)
m <- k*nrow(x)

#Set priors
a_mu_prior <- matrix(0,m)
a_v_i_prior <- diag(1,m) 

u_sigma_df_prior <- 13
u_sigma_scale_prior <- diag(1,k)
u_sigma_df_post <- tt + u_sigma_df_prior

#Initial values
u_sigma_i <- matrix(1,k,k)

#Data containers for posterior draws
draws_a <- matrix(NA,m, store)
draws_sigma <- matrix(NA, k*k, store)

#Run Gibbs sampler
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

Sigma # Pr

#Bvar
dim(draws_sigma)
bvar_est <- bvar(y = bayesian_model$data$Y, x = bayesian_model$data$Z, A = draws_a[1:72,],
                 C = draws_a[73:78, ], Sigma = draws_sigma)


summary(bvar_est)

#bvar_est <- thin_posterior(bvar_est, thin = 15)


#Forecast
bvar_pred <- predict(bvar_est, n.ahead = 5, new_d = rep(1, 5))

plot(bvar_pred)


##Forecast IRF
FEIR <- irf(bvar_est, impulse = "industrial_production", response = "net_greenhouse_pc", n.ahead = 5)

plot(FEIR, main = "Forecast Error Impulse Response", xlab = "Period", ylab = "Response")

FEIR <- irf(bvar_est, impulse = "naturalgas_imports", response = "net_greenhouse_pc", n.ahead = 5)

plot(FEIR, main = "Forecast Error Impulse Response", xlab = "Period", ylab = "Response")


GIR <- irf(bvar_est, impulse = "naturalgas_imports", response = "net_greenhouse_pc", n.ahead = 5, type = "gir")

plot(GIR, main = "Generalised Impulse Response", xlab = "Period", ylab = "Response")

#############################################################################

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$net_greenhouse_pc) #dependent variable is not stationary
ndiffs(data$net_greenhouse_pc) #how many diffs should we take?
net_greenhouse_pc <- diff(data$net_greenhouse_pc, differences = 1) #first differentiation
adf.test(net_greenhouse_pc) #Still not stationary
net_greenhouse_pc <- diff(net_greenhouse_pc, differences = 1) #second differentiation
adf.test(net_greenhouse_pc)
plot(net_greenhouse_pc, type="l") #visual check


str(data)
#Scale enviromental taxes and stationarity check
data <- data %>% mutate_at(c('environmental_taxes'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$environmental_taxes) #independent variable is not stationary
ndiffs(data$environmental_taxes) #how many diffs should we take?
environm_taxes <- diff(data$environmental_taxes, differences = 1) #first differentiation
adf.test(environm_taxes) #Still not stationary
environm_taxes <- diff(environm_taxes, differences = 1) #second differentiation
adf.test(environm_taxes) ##Still not stationary
plot(environm_taxes, type="l") #visual check

#KPss test
kpss.test(data$environmental_taxes)
kpss.test(environm_taxes)
#Philips and Perron
pp.test(data$environmental_taxes)
pp.test(environm_taxes)

#BoxCox
lambda_1=BoxCox.lambda(data$environmental_taxes)
plot(data$environmental_taxes, type="l")
lambda_1
plot.ts(BoxCox(data$environmental_taxes, lambda = lambda_1))
environm_bc<-BoxCox(data$environmental_taxes, lambda = lambda_1)
plot.ts(environm_bc)
adf.test(environm_bc)
ndiffs(environm_bc) #how many diffs should we take?
environm_taxes <- diff(environm_bc, differences = 1) #first differentiation
adf.test(environm_taxes) #Still not stationary
environm_taxes <- diff(environm_taxes, differences = 1) #second differentiation
adf.test(environm_taxes) ##Still not stationary
plot(environm_taxes, type="l") #visual check


#Scale and stationarity test GDP
data <- data %>% mutate_at(c('GDP pc'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$`GDP pc`) #independent variable is not stationary
ndiffs(data$`GDP pc`) #how many diffs should we take?
GDP_pc <- diff(data$`GDP pc`, differences = 1) #first differentiation
adf.test(GDP_pc) #Still not stationary
GDP_pc <- diff(GDP_pc, differences = 1) #second differentiation
adf.test(GDP_pc) ##Still not stationary
plot(GDP_pc, type="l") #visual check



#Scale and stationarity test for industrial_production
data <- data %>% mutate_at(c('industrial_production'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$industrial_production) #independent variable is not stationary
ndiffs(data$industrial_production) #how many diffs should we take?
industrial_production <- diff(data$industrial_production, differences = 1) #first differentiation
adf.test(industrial_production) #Still not stationary
industrial_production <- diff(industrial_production, differences = 1) #second differentiation
adf.test(industrial_production) ##Reject Ho 0.05 stationary
plot(industrial_production, type="l") #visual check

#Scale and stationarity test for energy_imp_dep
data <- data %>% mutate_at(c('energy_imp_dep'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$energy_imp_dep) #independent variable is not stationary
ndiffs(data$energy_imp_dep) #how many diffs should we take?
energy_imp_dep <- diff(data$energy_imp_dep, differences = 1) #first differentiation
adf.test(energy_imp_dep) #Still not stationary
energy_imp_dep <- diff(energy_imp_dep, differences = 1) #second differentiation
adf.test(energy_imp_dep) ##Reject Ho 0.05 Stationary
plot(energy_imp_dep, type="l") #visual check

str(data)
#Scale and stationarity test for naturalgas_imports 
data <- data %>% mutate_at(c('naturalgas_imports'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$naturalgas_imports ) #independent variable is not stationary
ndiffs(data$naturalgas_imports ) #how many diffs should we take?
naturalgas_imports  <- diff(data$naturalgas_imports , differences = 1) #first differentiation
adf.test(naturalgas_imports ) #Still not stationary
naturalgas_imports  <- diff(naturalgas_imports , differences = 1) #second differentiation
adf.test(naturalgas_imports ) ##Reject Ho 0.05 Stationary
plot(naturalgas_imports , type="l") #visual check


#Scale and stationarity test for oil_imports 
data <- data %>% mutate_at(c('oil_imports'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$oil_imports ) #independent variable is not stationary
ndiffs(data$oil_imports ) #how many diffs should we take?
oil_imports  <- diff(data$oil_imports , differences = 1) #first differentiation
adf.test(oil_imports ) #Still not stationary
oil_imports <- diff(oil_imports , differences = 1) #second differentiation
adf.test(oil_imports ) ##Still not stationary
plot(oil_imports , type="l") #visual check



#Scale and stationarity test for total_energy_supply 
data <- data %>% mutate_at(c('total_energy_supply'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$total_energy_supply) #independent variable is not stationary
ndiffs(data$total_energy_supply) #how many diffs should we take?
total_energy_supply <- diff(data$total_energy_supply , differences = 1) #first differentiation
adf.test(total_energy_supply) #Still not stationary
total_energy_supply <- diff(total_energy_supply , differences = 1) #second differentiation
adf.test(total_energy_supply) ##On the border
plot(total_energy_supply , type="l") #visual check



#Scale and stationarity test for gross_electricity_production 
data <- data %>% mutate_at(c('gross_electricity_production'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$gross_electricity_production) #independent variable is not stationary
ndiffs(data$gross_electricity_production) #how many diffs should we take?
gross_electricity_production <- diff(data$gross_electricity_production , differences = 2) #first differentiation
adf.test(gross_electricity_production) #Still not stationary
plot(gross_electricity_production , type="l") #visual check


#Scale and stationarity test for Share_of_land_under_permanent_crops 
data <- data %>% mutate_at(c('Share_of_land_under_permanent_crops'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Share_of_land_under_permanent_crops) #independent variable is not stationary
ndiffs(data$Share_of_land_under_permanent_crops) #how many diffs should we take?
Share_of_land_under_permanent_crops <- diff(data$Share_of_land_under_permanent_crops , differences = 1) #first differentiation
adf.test(Share_of_land_under_permanent_crops) #Stationary
plot(Share_of_land_under_permanent_crops , type="l") #visual check


#Scale and stationarity test for Area_harvested_Rice
data <- data %>% mutate_at(c('Area_harvested_Rice'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Area_harvested_Rice) #independent variable is not stationary
ndiffs(data$Area_harvested_Rice) #how many diffs should we take?
Area_harvested_Rice <- diff(data$Area_harvested_Rice, differences = 1) #first differentiation
adf.test(Area_harvested_Rice) #Stationary for 0.05
plot(Area_harvested_Rice , type="l") #visual check



#Scale and stationarity test for Fertilizer_used_per_area_of_cropland
data <- data %>% mutate_at(c('Fertilizer_used_per_area_of_cropland'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Fertilizer_used_per_area_of_cropland) #independent variable is not stationary
ndiffs(data$Fertilizer_used_per_area_of_cropland) #how many diffs should we take?
Fertilizer_used_per_area_of_cropland <- diff(data$Fertilizer_used_per_area_of_cropland, differences = 1) #first differentiation
adf.test(Fertilizer_used_per_area_of_cropland) #Still not stationary
Fertilizer_used_per_area_of_cropland <- diff(Fertilizer_used_per_area_of_cropland , differences = 1) #second differentiation
adf.test(Fertilizer_used_per_area_of_cropland) ##A little bit above
plot(Fertilizer_used_per_area_of_cropland , type="l") #visual check


#Scale and stationarity test for Share_in_land_area_Forest_Land
data <- data %>% mutate_at(c('Share_in_land_area_Forest_Land'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Share_in_land_area_Forest_Land) #independent variable is not stationary
ndiffs(data$Share_in_land_area_Forest_Land) #how many diffs should we take?
Share_in_land_area_Forest_Land <- diff(data$Share_in_land_area_Forest_Land, differences = 2) #first differentiation
adf.test(Share_in_land_area_Forest_Land) #A little bit above
plot(Share_in_land_area_Forest_Land , type="l") #visual check


#Scale and stationarity test for Rail_tracks_KM
data <- data %>% mutate_at(c('Rail_tracks_KM'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Rail_tracks_KM) #independent variable is not stationary
ndiffs(data$Rail_tracks_KM) #how many diffs should we take?
Rail_tracks_KM <- diff(data$Rail_tracks_KM , differences = 1) #first differentiation
adf.test(Rail_tracks_KM) #Still not stationary
Rail_tracks_KM <- diff(Rail_tracks_KM , differences = 1) #second differentiation
adf.test(Rail_tracks_KM) ##Still not stationary
plot(Rail_tracks_KM , type="l") #visual check


#Scale and stationarity test for Length_of_motorways
data <- data %>% mutate_at(c('Length_of_motorways'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Length_of_motorways) #independent variable is not stationary
ndiffs(data$Length_of_motorways) #how many diffs should we take?
Length_of_motorways <- diff(data$Length_of_motorways , differences = 1) #first differentiation
adf.test(Length_of_motorways) #Still not stationary
Length_of_motorways <- diff(Length_of_motorways , differences = 1) #second differentiation
adf.test(Length_of_motorways) ##Still not stationary
plot(Length_of_motorways , type="l") #visual check


#Scale and stationarity test for Number_of_motorcycle
data <- data %>% mutate_at(c('Number_of_motorcycle'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Number_of_motorcycle) #independent variable is not stationary
ndiffs(data$Number_of_motorcycle) #how many diffs should we take?
Number_of_motorcycle <- diff(data$Number_of_motorcycle , differences = 1) #first differentiation
adf.test(Number_of_motorcycle) #Still not stationary
Number_of_motorcycle <- diff(Number_of_motorcycle , differences = 1) #second differentiation
adf.test(Number_of_motorcycle) ##Still not stationary
plot(Number_of_motorcycle , type="l") #visual check


#Scale and stationarity test for Total_freight_loaded_and_unloaded
data <- data %>% mutate_at(c('Total_freight_loaded_and_unloaded'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Total_freight_loaded_and_unloaded) #independent variable is not stationary
ndiffs(data$Total_freight_loaded_and_unloaded) #how many diffs should we take?
Total_freight_loaded_and_unloaded <- diff(data$Total_freight_loaded_and_unloaded , differences = 1) #first differentiation
adf.test(Total_freight_loaded_and_unloaded) #Still not stationary
Total_freight_loaded_and_unloaded <- diff(Total_freight_loaded_and_unloaded , differences = 1) #second differentiation
adf.test(Total_freight_loaded_and_unloaded) ##Still not stationary
plot(Total_freight_loaded_and_unloaded, type="l") #visual check


#Scale and stationarity test for livestock_heads
data <- data %>% mutate_at(c('livestock_heads'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$livestock_heads) #independent variable is not stationary
ndiffs(data$livestock_heads) #how many diffs should we take?
livestock_heads <- diff(data$livestock_heads , differences = 1) #first differentiation
adf.test(livestock_heads) #Stationary
plot(livestock_heads, type="l") #visual check
livestock_heads <- diff(data$livestock_heads , differences = 1) #first differentiation
adf.test(livestock_heads) #Stationary
plot(livestock_heads, type="l") #visual check


#Scale and stationarity test for res_capacity 
data <- data %>% mutate_at(c('res_capacity'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$res_capacity ) #independent variable is not stationary
ndiffs(data$res_capacity ) #how many diffs should we take?
res_capacity  <- diff(data$res_capacity  , differences = 1) #first differentiation
adf.test(res_capacity ) #Still not stationary
res_capacity  <- diff(res_capacity  , differences = 1) #second differentiation
adf.test(res_capacity ) ##Still not stationary
plot(res_capacity , type="l") #visual check





