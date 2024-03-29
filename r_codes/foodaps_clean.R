################################################################################
##### Project: food inflation
##### Program: cex processing
##### Coding: Wenjie Zhan
################################################################################
# Clean the Environment
rm(list=ls())

# Loading packages
library(tidyverse)
library(tidycensus)
library(data.table)
library(dplyr)
library(ggplot2)
library(sandwich) # vcov
library(usmap) # us map with HI and AK
library(magrittr) # %$%
library(haven) # read_dta()
library(readr)

setwd("C:/Users/wenji/OneDrive/github_files/GitHub/food_inflation/")
################################################################################
##### 0. Data structure
################################################################################
# FoodAPS

################################################################################
##### 1. Import price data
################################################################################
### National CPI, by product
# Available area code: US city average; Size A; Size B/C


