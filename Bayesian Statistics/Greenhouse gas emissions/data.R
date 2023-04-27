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
library(tseries)
library(forecast)

data <- read_excel("data.xlsx", sheet = 'Data')

#Turn character features to numeric
data$Bovine_heads <- as.numeric(data$Bovine_heads)
data$Pigs_heads <- as.numeric(data$Pigs_heads)
data$Share_of_land_under_permanent_crops <- as.numeric(data$Share_of_land_under_permanent_crops)
data$Fertilizer_used_per_area_of_cropland <- as.numeric(data$Fertilizer_used_per_area_of_cropland)
data$Share_in_land_area_Forest_Land <- as.numeric(data$Share_in_land_area_Forest_Land)

str(data$Rail_tracks_KM)

#Data imputation
data$Rail_tracks_KM <- na_interpolation(data$Rail_tracks_KM, option = 'spline')
data %>% ggplot(aes(Year, Rail_tracks_KM)) + geom_line()

data %>% ggplot(aes(Year, Total_freight_loaded_and_unloaded)) + geom_line()
data$Total_freight_loaded_and_unloaded <- na_interpolation(data$Total_freight_loaded_and_unloaded, option = 'spline')

str(data)
summary(data)

#Standard
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

par(mfrow = c(1, 2))
plot(x = data$Year, y = data$Number_of_motorcycle, type = "l" )
plot(x = data$Year, y = data$Total_freight_loaded_and_unloaded, type = "l")

data <- data %>% 
  mutate(livestock_heads = Pigs_heads + Bovine_heads) %>% 
  select(!c(Pigs_heads, Bovine_heads))

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

#Scale data
data <- data %>% mutate_at(c('net_greenhouse_pc'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$net_greenhouse_pc) #dependent variable is not stationary
ndiffs(data$net_greenhouse_pc) #how many diffs should we take?
net_greenhouse_pc <- diff(data$net_greenhouse_pc, differences = 1) #first differentiation
adf.test(net_greenhouse_pc) #Still not stationary
net_greenhouse_pc <- diff(net_greenhouse_pc, differences = 1) #second differentiation
adf.test(net_greenhouse_pc)
plot(net_greenhouse_pc) #visual check
