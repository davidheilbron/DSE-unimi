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
library(tseries)
library(forecast)
library(bvartools)
library(coda)
library(spam)

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
           Chicken_heads + Turkeys_heads + Cattle_heads + Buffalo_head) 

#data <- data %>% 
#  select(!c(Pigs_heads, Head_goat, Head_sheep, Chicken_heads, Turkeys_heads, Cattle_heads, Buffalo_head))

data <- data %>% 
  mutate(res_capacity = hydro_capacity + geothermal_capacity + 
           wind_capacity + solar_capacity + biofuels_capacity + 
           biogas_capacity + waste_capacity)

#  select(!c(hydro_capacity, geothermal_capacity,
#            wind_capacity, solar_capacity, biofuels_capacity,
#            biogas_capacity, waste_capacity))

str(data)
par(mfrow = c(1, 2))
plot(x = data$Year, y = data$livestock_heads, type = "l" )
plot(x = data$Year, y = data$res_capacity, type = "l")

complete_data <- data %>% dplyr::select(Year, net_greenhouse_pc, environmental_taxes, `GDP pc`,
                                 industrial_production, energy_imp_dep, naturalgas_imports,
                                 oil_imports, total_energy_supply, gross_electricity_production,
                                 res_capacity, livestock_heads,
                                 Share_of_land_under_permanent_crops, Area_harvested_Rice,
                                 Fertilizer_used_per_area_of_cropland, Share_in_land_area_Forest_Land,
                                 Rail_tracks_KM, Length_of_motorways, Number_of_motorcycle, Total_freight_loaded_and_unloaded)

#Scale data
#data <- data %>% mutate_at(c('net_greenhouse_pc'), ~(scale(.) %>% as.vector))

par(mfrow = c(1,1))


#############################################################################

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$net_greenhouse_pc) #dependent variable is not stationary
ndiffs(data$net_greenhouse_pc) #how many diffs should we take?
net_greenhouse_pc_1 <- diff(data$net_greenhouse_pc, differences = 1) #first differentiation
adf.test(net_greenhouse_pc_1) #Still not stationary
net_greenhouse_pc_2 <- diff(net_greenhouse_pc_1, differences = 1) #second differentiation
adf.test(net_greenhouse_pc_2)
plot(net_greenhouse_pc_2, type = "l") #visual check


str(data)
#Scale enviromental taxes and stationarity check
data <- data %>% mutate_at(c('environmental_taxes'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$environmental_taxes) #independent variable is not stationary
ndiffs(data$environmental_taxes) #how many diffs should we take?
environm_taxes_1 <- diff(data$environmental_taxes, differences = 1) #first differentiation
adf.test(environm_taxes_1) #Still not stationary
environm_taxes_2 <- diff(environm_taxes_1, differences = 1) #second differentiation
adf.test(environm_taxes) ##Still not stationary
plot(environm_taxes, type="l") #visual check

#KPss test
kpss.test(data$environmental_taxes)
kpss.test(environm_taxes_1)
kpss.test(environm_taxes_2)
#Philips and Perron
pp.test(data$environmental_taxes)
pp.test(environm_taxes_1) #Stationary

#BoxCox
lambda_1=BoxCox.lambda(data$environmental_taxes)
plot(data$environmental_taxes, type="l")
lambda_1
plot.ts(BoxCox(data$environmental_taxes, lambda = lambda_1))
environm_bc<-BoxCox(data$environmental_taxes, lambda = lambda_1)
plot.ts(environm_bc)
adf.test(environm_bc)
ndiffs(environm_bc) #how many diffs should we take?
environm_taxes_1b <- diff(environm_bc, differences = 1) #first differentiation
adf.test(environm_taxes_1b) #Still not stationary
environm_taxes_2b <- diff(environm_taxes_1b, differences = 1) #second differentiation
adf.test(environm_taxes) ##Still not stationary
plot(environm_taxes, type="l") #visual check


#Scale and stationarity test GDP
data <- data %>% mutate_at(c('GDP pc'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$`GDP pc`) #independent variable is not stationary
ndiffs(data$`GDP pc`) #how many diffs should we take?
GDP_pc_1 <- diff(data$`GDP pc`, differences = 1) #first differentiation
adf.test(GDP_pc_1) #Still not stationary
GDP_pc_2 <- diff(GDP_pc_1, differences = 1) #second differentiation
adf.test(GDP_pc_2) ##Still not stationary
plot(GDP_pc_2, type="l") #visual check

#KPss test
kpss.test(data$`GDP pc`)
kpss.test(GDP_pc_1)#Stationarity in level

#Philips and Perron
pp.test(data$`GDP pc`)
pp.test(GDP_pc_1) ##Stationary

##We stick with the first differentiation


#BoxCox
lambda_1=BoxCox.lambda(data$`GDP pc`)
plot(data$`GDP pc`, type="l")
lambda_1
plot.ts(BoxCox(data$`GDP pc`, lambda = lambda_1))
GDP_bc<-BoxCox(data$`GDP pc`, lambda = lambda_1)
plot.ts(GDP_bc)
GDP_bc=scale(GDP_bc)
adf.test(GDP_bc)
ndiffs(GDP_bc) #how many diffs should we take?
GDP_bc_1 <- diff(GDP_bc, differences = 1) #first differentiation
adf.test(GDP_bc_1) #Still not stationary
GDP_bc_2 <- diff(GDP_bc_1, differences = 1) #second differentiation
adf.test(environm_taxes) ##Still not stationary
plot(environm_taxes, type="l") #visual check



#Scale and stationarity test for industrial_production
data <- data %>% mutate_at(c('industrial_production'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$industrial_production) #independent variable is not stationary
ndiffs(data$industrial_production) #how many diffs should we take?
industrial_production_1 <- diff(data$industrial_production, differences = 1) #first differentiation
adf.test(industrial_production_1) #Still not stationary
industrial_production_2 <- diff(industrial_production_1, differences = 1) #second differentiation
adf.test(industrial_production_2) ##Reject Ho 0.05 Stationary
plot(industrial_production_2, type="l") #visual check

#KPss test
kpss.test(data$industrial_production) #not stationary
kpss.test(industrial_production_1)#Stationarity in level

#Philips and Perron
pp.test(data$industrial_production) #not stationary
pp.test(industrial_production_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$environmental_taxes)
plot(data$`GDP pc`, type="l")
lambda_1
plot.ts(BoxCox(data$industrial_production, lambda = lambda_1))
industrial_production_bc<-BoxCox(data$industrial_production, lambda = lambda_1)
plot.ts(industrial_production_bc)
industrial_production_bc=scale(industrial_production_bc)
adf.test(industrial_production_bc)
ndiffs(industrial_production_bc) #how many diffs should we take?
industrial_production_bc_1 <- diff(industrial_production_bc, differences = 1) #first differentiation
adf.test(industrial_production_bc_1) #Still not stationary
industrial_production_bc_2 <- diff(industrial_production_bc_1, differences = 1) #second differentiation
adf.test(industrial_production_bc_2) #Stationary
plot(industrial_production_bc_2, type="l") #visual check



#Scale and stationarity test for energy_imp_dep
data <- data %>% mutate_at(c('energy_imp_dep'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$energy_imp_dep) #independent variable is not stationary
ndiffs(data$energy_imp_dep) #how many diffs should we take?
energy_imp_dep_1 <- diff(data$energy_imp_dep, differences = 1) #first differentiation
adf.test(energy_imp_dep_1) #Still not stationary
energy_imp_dep_2 <- diff(energy_imp_dep_1, differences = 1) #second differentiation
adf.test(energy_imp_dep_2) ##Reject Ho 0.05 Stationary
plot(energy_imp_dep_2, type="l") #visual check


#KPss test
kpss.test(data$energy_imp_dep) #not stationary
kpss.test(energy_imp_dep_1)#Stationarity in level

#Philips and Perron
pp.test(data$energy_imp_dep) #not stationary
pp.test(energy_imp_dep_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$energy_imp_dep)
plot(data$energy_imp_dep, type="l")
lambda_1
plot.ts(BoxCox(data$energy_imp_dep, lambda = lambda_1))
energy_imp_dep_bc<-BoxCox(data$energy_imp_dep, lambda = lambda_1)
plot.ts(energy_imp_dep_bc)
energy_imp_dep_bc=scale(energy_imp_dep_bc)
adf.test(energy_imp_dep_bc)
ndiffs(energy_imp_dep_bc) #how many diffs should we take?
energy_imp_dep_bc_1 <- diff(energy_imp_dep_bc, differences = 1) #first differentiation
adf.test(energy_imp_dep_bc_1) #Still not stationary
energy_imp_dep_bc_2 <- diff(energy_imp_dep_bc_1, differences = 1) #second differentiation
adf.test(energy_imp_dep_bc_2) #Stationary
plot(energy_imp_dep_bc_2, type="l") #visual check


#Scale and stationarity test for naturalgas_imports 
data <- data %>% mutate_at(c('naturalgas_imports'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$naturalgas_imports ) #independent variable is not stationary
ndiffs(data$naturalgas_imports ) #how many diffs should we take?
naturalgas_imports_1  <- diff(data$naturalgas_imports , differences = 1) #first differentiation
adf.test(naturalgas_imports_1 ) #Still not stationary
naturalgas_imports_2  <- diff(naturalgas_imports_1 , differences = 1) #second differentiation
adf.test(naturalgas_imports_2 ) ##Reject Ho 0.05 Stationary
plot(naturalgas_imports_2 , type="l") #visual check

#KPss test
kpss.test(data$naturalgas_imports) #not stationary
kpss.test(naturalgas_imports_1)#Stationary in level

#Philips and Perron
pp.test(data$naturalgas_imports) #not stationary
pp.test(naturalgas_imports_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$naturalgas_imports)
plot(data$naturalgas_imports, type="l")
lambda_1
plot.ts(BoxCox(data$naturalgas_imports, lambda = lambda_1))
naturalgas_imports_bc<-BoxCox(data$naturalgas_imports, lambda = lambda_1)
plot.ts(naturalgas_imports_bc)
energy_imp_dep_bc=scale(naturalgas_imports_bc)
adf.test(naturalgas_imports_bc)
ndiffs(naturalgas_imports_bc) #how many diffs should we take?
naturalgas_imports_bc_1 <- diff(naturalgas_imports_bc, differences = 1) #first differentiation
adf.test(naturalgas_imports_bc_1) #Still not stationary
naturalgas_imports_bc_2 <- diff(naturalgas_imports_bc_1, differences = 1) #second differentiation
adf.test(naturalgas_imports_bc_2) #Stationary
plot(naturalgas_imports_bc_2, type="l") #visual check


#Scale and stationarity test for oil_imports 
data <- data %>% mutate_at(c('oil_imports'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$oil_imports ) #independent variable is not stationary
ndiffs(data$oil_imports ) #how many diffs should we take?
oil_imports_1  <- diff(data$oil_imports , differences = 1) #first differentiation
adf.test(oil_imports_1 ) #Still not stationary
oil_imports_2 <- diff(oil_imports_1 , differences = 1) #second differentiation
adf.test(oil_imports_2 ) ##Still not stationary
plot(oil_imports_2 , type="l") #visual check

#KPss test
kpss.test(data$oil_imports) #not stationary
kpss.test(oil_imports_1)#Stationary in level

#Philips and Perron
pp.test(data$oil_imports) #not stationary
pp.test(oil_imports_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$oil_imports)
plot(data$oil_imports, type="l")
lambda_1
plot.ts(BoxCox(data$oil_imports, lambda = lambda_1))
oil_imports_bc<-BoxCox(data$oil_imports, lambda = lambda_1)
plot.ts(oil_imports_bc)
oil_imports_bc=scale(oil_imports_bc)
adf.test(oil_imports_bc)
ndiffs(oil_imports_bc) #how many diffs should we take?
oil_imports_bc_1 <- diff(oil_imports_bc, differences = 1) #first differentiation
adf.test(oil_imports_bc_1) #Still not stationary
oil_imports_bc_2 <- diff(oil_imports_bc_1, differences = 1) #second differentiation
adf.test(oil_imports_bc_2) #Still not stationary but more close to it
plot(oil_imports_bc_2, type="l") #visual check




#Scale and stationarity test for total_energy_supply 
data <- data %>% mutate_at(c('total_energy_supply'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$total_energy_supply) #independent variable is not stationary
ndiffs(data$total_energy_supply) #how many diffs should we take?
total_energy_supply_1 <- diff(data$total_energy_supply , differences = 1) #first differentiation
adf.test(total_energy_supply_1) #Still not stationary
total_energy_supply_2 <- diff(total_energy_supply_1 , differences = 1) #second differentiation
adf.test(total_energy_supply_2) ##On the border
plot(total_energy_supply_2 , type="l") #visual check


#KPss test
kpss.test(data$total_energy_supply) #nStationarity in level

#Philips and Perron
pp.test(data$total_energy_supply) #not stationary
pp.test(total_energy_supply_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$total_energy_supply)
plot(data$total_energy_supply, type="l")
lambda_1
plot.ts(BoxCox(data$total_energy_supply, lambda = lambda_1))
total_energy_bc<-BoxCox(data$total_energy_supply, lambda = lambda_1)
plot.ts(total_energy_bc)
total_energy_bc=scale(total_energy_bc)
adf.test(total_energy_bc)
ndiffs(total_energy_bc) #how many diffs should we take?
total_energy_bc_1 <- diff(total_energy_bc, differences = 1) #first differentiation
adf.test(total_energy_bc_1) #Still not stationary
total_energy_bc_2 <- diff(total_energy_bc_1, differences = 1) #second differentiation
adf.test(total_energy_bc_2) #Still not stationary but more close to it
plot(total_energy_bc_2, type="l") #visual check


#Scale and stationarity test for gross_electricity_production 
data <- data %>% mutate_at(c('gross_electricity_production'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$gross_electricity_production) #independent variable is not stationary
ndiffs(data$gross_electricity_production) #how many diffs should we take?
gross_electricity_production_2 <- diff(data$gross_electricity_production , differences = 2) #second differentiation
adf.test(gross_electricity_production_2) #Stationary
plot(gross_electricity_production_2 , type="l") #visual check

#KPss test
kpss.test(data$gross_electricity_production) #not stationary
kpss.test(gross_electricity_production_2) #Stationary

#Philips and Perron
pp.test(data$gross_electricity_production) #not stationary
pp.test(gross_electricity_production_2) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$gross_electricity_production)
plot(data$gross_electricity_production, type="l")
lambda_1
plot.ts(BoxCox(data$gross_electricity_production, lambda = lambda_1))
gross_electricity_prod_bc<-BoxCox(data$gross_electricity_production, lambda = lambda_1)
plot.ts(gross_electricity_prod_bc)
gross_electricity_prod_bc=scale(gross_electricity_prod_bc)
adf.test(gross_electricity_prod_bc)
ndiffs(gross_electricity_prod_bc)#how many diffs should we take?
gross_electricity_prod_bc_2 <- diff(gross_electricity_prod_bc, differences = 2) #Second differentiation
adf.test(gross_electricity_prod_bc_2) #Stationary
plot(gross_electricity_prod_bc_2, type="l") #visual check


#Scale and stationarity test for Share_of_land_under_permanent_crops 
data <- data %>% mutate_at(c('Share_of_land_under_permanent_crops'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Share_of_land_under_permanent_crops) #independent variable is not stationary
ndiffs(data$Share_of_land_under_permanent_crops) #how many diffs should we take?
Share_of_land_under_permanent_crops_1 <- diff(data$Share_of_land_under_permanent_crops , differences = 1) #first differentiation
adf.test(Share_of_land_under_permanent_crops_1) #Stationary
plot(Share_of_land_under_permanent_crops_1 , type="l") #visual check

#KPss test
kpss.test(data$Share_of_land_under_permanent_crops) #not stationary
kpss.test(Share_of_land_under_permanent_crops_1) #Stationary

#Philips and Perron
pp.test(data$Share_of_land_under_permanent_crops) #not stationary
pp.test(Share_of_land_under_permanent_crops_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$Share_of_land_under_permanent_crops)
plot(data$Share_of_land_under_permanent_crops, type="l")
lambda_1
plot.ts(BoxCox(data$Share_of_land_under_permanent_crops, lambda = lambda_1))
Share_land_crops_bc<-BoxCox(data$Share_of_land_under_permanent_crops, lambda = lambda_1)
plot.ts(Share_land_crops_bc)
Share_land_crops_bc=scale(Share_land_crops_bc)
adf.test(Share_land_crops_bc)
ndiffs(Share_land_crops_bc)#how many diffs should we take?
Share_land_crops_bc_1 <- diff(Share_land_crops_bc, differences = 1) #Second differentiation
adf.test(Share_land_crops_bc_1) #Stationary
plot(Share_land_crops_bc_1, type="l") #visual check


#Scale and stationarity test for Area_harvested_Rice
data <- data %>% mutate_at(c('Area_harvested_Rice'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Area_harvested_Rice) #independent variable is not stationary
ndiffs(data$Area_harvested_Rice) #how many diffs should we take?
Area_harvested_Rice_1 <- diff(data$Area_harvested_Rice, differences = 1) #first differentiation
adf.test(Area_harvested_Rice_1) #Stationary 
plot(Area_harvested_Rice_1 , type="l") #visual check

#KPss test
kpss.test(data$Area_harvested_Rice) #Stationary

#Philips and Perron
pp.test(data$Area_harvested_Rice) #not stationary
pp.test(Area_harvested_Rice_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$Area_harvested_Rice)
plot(data$Area_harvested_Rice, type="l")
lambda_1
plot.ts(BoxCox(data$Area_harvested_Rice, lambda = lambda_1))
Area_harvested_rice_bc<-BoxCox(data$Area_harvested_Rice, lambda = lambda_1)
plot.ts(Area_harvested_rice_bc)
Area_harvested_rice_bc=scale(Area_harvested_rice_bc)
adf.test(Area_harvested_rice_bc)
ndiffs(Area_harvested_rice_bc)#how many diffs should we take
Area_harvested_rice_bc_1 <- diff(Area_harvested_rice_bc, differences = 1) #Second differentiation
adf.test(Area_harvested_rice_bc_1) #Stationary
plot(Area_harvested_rice_bc_1, type="l") #visual check


#Scale and stationarity test for Fertilizer_used_per_area_of_cropland
data <- data %>% mutate_at(c('Fertilizer_used_per_area_of_cropland'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Fertilizer_used_per_area_of_cropland) #independent variable is not stationary
ndiffs(data$Fertilizer_used_per_area_of_cropland) #how many diffs should we take?
Fertilizer_used_per_area_of_cropland_1 <- diff(data$Fertilizer_used_per_area_of_cropland, differences = 1) #first differentiation
adf.test(Fertilizer_used_per_area_of_cropland_1) #Still not stationary
Fertilizer_used_per_area_of_cropland_2 <- diff(Fertilizer_used_per_area_of_cropland_1 , differences = 1) #second differentiation
adf.test(Fertilizer_used_per_area_of_cropland_2) ##A little bit above
plot(Fertilizer_used_per_area_of_cropland_2 , type="l") #visual check

#KPss test
kpss.test(data$Fertilizer_used_per_area_of_cropland) #not Stationary
kpss.test(Fertilizer_used_per_area_of_cropland_1) #Stationary

#Philips and Perron
pp.test(data$Fertilizer_used_per_area_of_cropland) #not stationary
pp.test(Fertilizer_used_per_area_of_cropland_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$Fertilizer_used_per_area_of_cropland)
plot(data$Fertilizer_used_per_area_of_cropland, type="l")
lambda_1
plot.ts(BoxCox(data$Fertilizer_used_per_area_of_cropland, lambda = lambda_1))
Fertilizer_bc<-BoxCox(data$Fertilizer_used_per_area_of_cropland, lambda = lambda_1)
plot.ts(Fertilizer_bc)
Fertilizer_bc=scale(Fertilizer_bc)
adf.test(Fertilizer_bc)
ndiffs(Fertilizer_bc)#how many diffs should we take
Fertilizer_bc_1 <- diff(Fertilizer_bc, differences = 1) #First differentiation
adf.test(Fertilizer_bc_1) #Not Stationary
plot(Fertilizer_bc_1, type="l") #visual check
Fertilizer_bc_2 <- diff(Fertilizer_bc_1, differences = 1) #Second differentiation
adf.test(Fertilizer_bc_2) #Stationary
plot(Fertilizer_bc_2, type="l")


#Scale and stationarity test for Share_in_land_area_Forest_Land
data <- data %>% mutate_at(c('Share_in_land_area_Forest_Land'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Share_in_land_area_Forest_Land) #independent variable is not stationary
ndiffs(data$Share_in_land_area_Forest_Land) #how many diffs should we take?
Share_in_land_area_Forest_Land_2 <- diff(data$Share_in_land_area_Forest_Land, differences = 2) #first differentiation
adf.test(Share_in_land_area_Forest_Land_2) #A little bit above
plot(Share_in_land_area_Forest_Land_2 , type="l") #visual check

#KPss test
kpss.test(data$Share_in_land_area_Forest_Land) #not Stationary
kpss.test(Share_in_land_area_Forest_Land_2) #Stationary

#Philips and Perron
pp.test(data$Share_in_land_area_Forest_Land) #not stationary
pp.test(Share_in_land_area_Forest_Land_2) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$Share_in_land_area_Forest_Land)
plot(data$Share_in_land_area_Forest_Land, type="l")
lambda_1
plot.ts(BoxCox(data$Share_in_land_area_Forest_Land, lambda = lambda_1))
Forest_bc<-BoxCox(data$Share_in_land_area_Forest_Land, lambda = lambda_1)
plot.ts(Forest_bc)
Forest_bc=scale(Forest_bc)
adf.test(Forest_bc)
ndiffs(Forest_bc)#how many diffs should we take
Forest_bc_2 <- diff(Forest_bc, differences = 2) #Second differentiation
adf.test(Forest_bc_2) #Close to be stationart
plot(Forest_bc_2, type="l")


#Scale and stationarity test for Rail_tracks_KM
data <- data %>% mutate_at(c('Rail_tracks_KM'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Rail_tracks_KM) #independent variable is not stationary
ndiffs(data$Rail_tracks_KM) #how many diffs should we take?
Rail_tracks_KM_1 <- diff(data$Rail_tracks_KM , differences = 1) #first differentiation
adf.test(Rail_tracks_KM_1) #Still not stationary (And worse than before)
Rail_tracks_KM_2 <- diff(Rail_tracks_KM_1 , differences = 1) #second differentiation
adf.test(Rail_tracks_KM_2) ##Still not stationary
plot(Rail_tracks_KM_2 , type="l") #visual check

#KPss test
kpss.test(data$Rail_tracks_KM) #not Stationary
kpss.test(Rail_tracks_KM_1) #Stationary

#Philips and Perron
pp.test(data$Rail_tracks_KM) #not stationary
pp.test(Rail_tracks_KM_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$Rail_tracks_KM)
plot(data$Rail_tracks_KM, type="l")
lambda_1
plot.ts(BoxCox(data$Rail_tracks_KM, lambda = lambda_1))
Rail_bc<-BoxCox(data$Rail_tracks_KM, lambda = lambda_1)
plot.ts(Rail_bc)
Rail_bc=scale(Rail_bc)
adf.test(Rail_bc)#Not stationary
ndiffs(Rail_bc)#how many diffs should we take
Rail_bc_1 <- diff(Rail_bc, differences = 1) #First differentiation
adf.test(Rail_bc_1) #Not Stationary
plot(Fertilizer_bc_1, type="l") #visual check
Rail_bc_2 <- diff(Rail_bc_1, differences = 1) #Second differentiation
adf.test(Rail_bc_2) #Stationary
plot(Fertilizer_bc_2, type="l")


#Scale and stationarity test for Length_of_motorways
data <- data %>% mutate_at(c('Length_of_motorways'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Length_of_motorways) #independent variable is not stationary
ndiffs(data$Length_of_motorways) #how many diffs should we take?
Length_of_motorways_1 <- diff(data$Length_of_motorways , differences = 1) #first differentiation
adf.test(Length_of_motorways_1) #Still not stationary
Length_of_motorways_2 <- diff(Length_of_motorways_1 , differences = 1) #second differentiation
adf.test(Length_of_motorways_2) ##Still not stationary
plot(Length_of_motorways_2 , type="l") #visual check

#KPss test
kpss.test(data$Length_of_motorways) #not Stationary
kpss.test(Length_of_motorways_1) #Stationary

#Philips and Perron
pp.test(data$Length_of_motorways) #not stationary
pp.test(Length_of_motorways_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$Length_of_motorways)
plot(data$Length_of_motorways, type="l")
lambda_1
plot.ts(BoxCox(data$Length_of_motorways, lambda = lambda_1))
motorways_bc<-BoxCox(data$Length_of_motorways, lambda = lambda_1)
plot.ts(motorways_bc)
motorways_bc=scale(motorways_bc)
adf.test(motorways_bc)#Not stationary
ndiffs(motorways_bc)#how many diffs should we take
motorways_bc_1 <- diff(motorways_bc, differences = 1) #First differentiation
adf.test(motorways_bc_1) #Not Stationary
plot(motorways_bc_1, type="l") #visual check
motorways_bc_2 <- diff(motorways_bc_1, differences = 1) #Second differentiation
adf.test(motorways_bc_2) #Stationary
plot(motorways_bc_2, type="l")


#Scale and stationarity test for Number_of_motorcycle
data <- data %>% mutate_at(c('Number_of_motorcycle'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Number_of_motorcycle) #independent variable is not stationary
ndiffs(data$Number_of_motorcycle) #how many diffs should we take?
Number_of_motorcycle_1 <- diff(data$Number_of_motorcycle , differences = 1) #first differentiation
adf.test(Number_of_motorcycle_1) #Still not stationary
Number_of_motorcycle_2 <- diff(Number_of_motorcycle_1 , differences = 1) #second differentiation
adf.test(Number_of_motorcycle_2) ##Still not stationary
plot(Number_of_motorcycle_2 , type="l") #visual check

#KPss test
kpss.test(data$Number_of_motorcycle) #not Stationary
kpss.test(Number_of_motorcycle_1) #Stationary

#Philips and Perron
pp.test(data$Number_of_motorcycle) #not stationary
pp.test(Number_of_motorcycle_1) ##non stationary
pp.test(Number_of_motorcycle_2)# Stationary 


#BoxCox
lambda_1=BoxCox.lambda(data$Number_of_motorcycle)
plot(data$Number_of_motorcycle, type="l")
lambda_1
plot.ts(BoxCox(data$Number_of_motorcycle, lambda = lambda_1))
Number_mot_bc<-BoxCox(data$Number_of_motorcycle, lambda = lambda_1)
plot.ts(Number_mot_bc)
Number_mot_bc=scale(Number_mot_bc)
adf.test(Number_mot_bc)#Not stationary
ndiffs(Number_mot_bc)#how many diffs should we take
Number_mot_bc_1 <- diff(Number_mot_bc, differences = 1) #First differentiation
adf.test(Number_mot_bc_1) #Not Stationary
plot(Number_mot_bc_1, type="l") #visual check
Number_mot_bc_2 <- diff(Number_mot_bc_1, differences = 1) #Second differentiation
adf.test(Number_mot_bc_2) #Not_stationary
plot(Number_mot_bc_2, type="l")


#Scale and stationarity test for Total_freight_loaded_and_unloaded
data <- data %>% mutate_at(c('Total_freight_loaded_and_unloaded'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$Total_freight_loaded_and_unloaded) #independent variable is not stationary
ndiffs(data$Total_freight_loaded_and_unloaded) #how many diffs should we take?
Total_freight_loaded_and_unloaded_1 <- diff(data$Total_freight_loaded_and_unloaded , differences = 1) #first differentiation
adf.test(Total_freight_loaded_and_unloaded_1) #Still not stationary
Total_freight_loaded_and_unloaded_2 <- diff(Total_freight_loaded_and_unloaded_1 , differences = 1) #second differentiation
adf.test(Total_freight_loaded_and_unloaded_2) ##Still not stationary 0.08
plot(Total_freight_loaded_and_unloaded_2, type="l") #visual check


#KPss test
kpss.test(data$Total_freight_loaded_and_unloaded) #not Stationary
kpss.test(Total_freight_loaded_and_unloaded_1) #Stationary

#Philips and Perron
pp.test(data$Total_freight_loaded_and_unloaded) #not stationary
pp.test(Total_freight_loaded_and_unloaded_1) ##not Stationary
pp.test(Total_freight_loaded_and_unloaded_2)#not stat


#BoxCox
lambda_1=BoxCox.lambda(data$Total_freight_loaded_and_unloaded)
plot(data$Total_freight_loaded_and_unloaded, type="l")
lambda_1
plot.ts(BoxCox(data$Total_freight_loaded_and_unloaded, lambda = lambda_1))
Freight_bc<-BoxCox(data$Total_freight_loaded_and_unloaded, lambda = lambda_1)
plot.ts(Freight_bc)
Freight_bc=scale(Freight_bc)
adf.test(Freight_bc)#Not stationary
ndiffs(Freight_bc)#how many diffs should we take
Freight_bc_1 <- diff(Freight_bc, differences = 1) #First differentiation
adf.test(Freight_bc_1) #Not Stationary
plot(Freight_bc_1, type="l") #visual check
Freight_bc_2 <- diff(Freight_bc_1, differences = 1) #Second differentiation
adf.test(Freight_bc_2) #Stationary
plot(Freight_bc_2, type="l")


#Scale and stationarity test for livestock_heads
data <- data %>% mutate_at(c('livestock_heads'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$livestock_heads) #independent variable is not stationary
ndiffs(data$livestock_heads) #how many diffs should we take?
livestock_heads_1 <- diff(data$livestock_heads , differences = 1) #first differentiation
adf.test(livestock_heads_1) # not Stationary
plot(livestock_heads_1, type="l") #visual check
livestock_heads_2 <- diff(livestock_heads_1 , differences = 1) #first differentiation
adf.test(livestock_heads_2) #Stationary
plot(livestock_heads_2, type="l") #visual check

#KPss test
kpss.test(data$livestock_heads) #Stationary

#Philips and Perron
pp.test(data$livestock_heads) #not stationary
pp.test(livestock_heads_1) ##Stationary


#BoxCox
lambda_1=BoxCox.lambda(data$livestock_heads)
plot(data$livestock_heads, type="l")
lambda_1
plot.ts(BoxCox(data$livestock_heads, lambda = lambda_1))
livestock_bc<-BoxCox(data$livestock_heads, lambda = lambda_1)
plot.ts(livestock_bc)
livestock_bc=scale(livestock_bc)
adf.test(livestock_bc)#Not stationary
ndiffs(livestock_bc)#how many diffs should we take 0
livestock_bc_1 <- diff(livestock_bc, differences = 1) #First differentiation
adf.test(livestock_bc_1) #Not Stationary
plot(livestock_bc_1, type="l") #visual check
livestock_bc_2 <- diff(livestock_bc_1, differences = 1) #Second differentiation
adf.test(livestock_bc_2) #Close to be stationary 
plot(livestock_bc_2, type="l")


#Scale and stationarity test for res_capacity 
data <- data %>% mutate_at(c('res_capacity'), ~(scale(.) %>% as.vector))

#Test for stationarity - ADF test
#H0: time series is not stationary
adf.test(data$res_capacity ) #independent variable is not stationary
ndiffs(data$res_capacity ) #how many diffs should we take?
res_capacity_1  <- diff(data$res_capacity  , differences = 1) #first differentiation
adf.test(res_capacity_1 ) #Still not stationary
res_capacity_2 <- diff(res_capacity_1  , differences = 1) #second differentiation
adf.test(res_capacity_2 ) ##Still not stationary
plot(res_capacity_2 , type="l") #visual check

#KPss test
kpss.test(data$res_capacity) #not Stationary
kpss.test(res_capacity_1) #Stationary

#Philips and Perron
pp.test(data$res_capacity) #not stationary
pp.test(res_capacity_1) ##not stationary
pp.test(res_capacity_2) #stationary


#BoxCox
lambda_1=BoxCox.lambda(data$res_capacity)
plot(data$res_capacity, type="l")
lambda_1
plot.ts(BoxCox(data$res_capacity, lambda = lambda_1))
res_cap_bc<-BoxCox(data$res_capacity, lambda = lambda_1)
plot.ts(res_cap_bc)
res_cap_bc_bc=scale(res_cap_bc)
adf.test(res_cap_bc)#Not stationary
ndiffs(res_cap_bc)#how many diffs should we take
res_cap_bc_1 <- diff(res_cap_bc, differences = 1) #First differentiation
adf.test(res_cap_bc_1) #Not Stationary
plot(res_cap_bc_1, type="l") #visual check
res_cap_bc_2 <- diff(res_cap_bc_1, differences = 1) #Second differentiation
adf.test(res_cap_bc_2) #Stationary
plot(res_cap_bc_2, type="l")

#I(2) features datasets
bayes_1 <- cbind(net_greenhouse_pc_d2, industrial_production_2, energy_imp_dep_2)
bayes_2 <- cbind(net_greenhouse_pc_d2, naturalgas_imports_2, oil_imports_2)
bayes_3 <- cbind(net_greenhouse_pc_d2, total_energy_supply_2, gross_electricity_production_2)
bayes_4 <- cbind(net_greenhouse_pc_d2, Fertilizer_used_per_area_of_cropland_2, Share_in_land_area_Forest_Land_2)
write.csv(bayes_1, 'bayes_data_1.csv')
write.csv(bayes_2, 'bayes_data_2.csv')
write.csv(bayes_3, 'bayes_data_3.csv')
write.csv(bayes_4, 'bayes_data_4.csv')


#I(1) features datasets
bayes_5 <- cbind(net_greenhouse_pc_1, Share_of_land_under_permanent_crops_1, Area_harvested_Rice_1)
write.csv(bayes_5, 'bayes_data_5.csv')


bayes_6 <- cbind(net_greenhouse_pc_1, livestock_heads_1, Area_harvested_Rice_1)
write.csv(bayes_6, 'bayes_data_6.csv')

bayes_7 <- cbind(net_greenhouse_pc_1, res_capacity_1, total_energy_supply_1)
write.csv(bayes_7, 'bayes_data_7.csv')

bayes_8 <- cbind(net_greenhouse_pc_1, Length_of_motorways_1, Total_freight_loaded_and_unloaded_1)
write.csv(bayes_8, 'bayes_data_8.csv')




