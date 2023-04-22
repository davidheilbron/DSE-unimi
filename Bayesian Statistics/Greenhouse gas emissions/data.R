#Script for building the database for the Bayesian Analysis project

#Libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(xts)
library(dplyr)
library(data.table)

setwd("C:/Users/Administrator/OneDrive/Studies/M.Sc. Data Science & Economics/2nd Year/Bayesian Analysis/Exam")
files  <- list.files(recursive = T,
                     pattern = ".xlsx",
                     full.names = T)

files <- as_tibble(files)
files$value <- gsub("^.{0,2}", "", files$value)

air_emissions <- read_excel(paste0(files[1,1]), sheet = 'Sheet 1', range = "A10:AC62")
gas_imports <- read_excel(paste0(files[2,1]), sheet = 'Sheet 1', range = "A10:AO194")
gdp <- read_excel(paste0(files[3,1]), sheet = 'Sheet 1', range = "A10:AN63")
industrial_prod <- read_excel(paste0(files[4,1]))
oil_imports <- read_excel(paste0(files[5,1]), sheet = 'Sheet 1', range = "A9:AI229")
oil_stock <- read_excel(paste0(files[6,1]), sheet = 'Sheet 1', range = "A10:AK133")

#Air emissions
air_emissions <- air_emissions[-1,]
colnames(air_emissions)[1] <- "Time"
str(air_emissions)

#Gas imports
gas_imports <- gas_imports[-1,]
colnames(gas_imports)[1] <- "Time"


str(gas_imports)


#Create dataframe
data <- air_emissions %>% select(Time, Italy)
data <- data %>% rename("air.emissions" = Italy)
data <- data %>% mutate(country = "Italy")
