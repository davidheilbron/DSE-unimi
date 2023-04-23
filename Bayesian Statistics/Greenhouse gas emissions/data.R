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


data <- read_excel("data.xlsx", sheet = 'Data')
