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

