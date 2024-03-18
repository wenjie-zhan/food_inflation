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
# product by month by census group
# months: 01/2010 - 12/2022
# census regions: Northeast, Midwest, West, South
# product groups: "nonfood", "fafh", "cereals and bakery", "meat", "eggs", "dairy", "fruits and vegetables", "alcoholic beverage", 
# "nonalcoholic beverage", "other food"


################################################################################
##### 1. Import CEX data: expenditure and demographic characteristics
################################################################################
### Import fmld and expd data
fmld_raw <- c()
expd_raw <- c()
fmld <- c()
expd <- c()
for (yr in 1:13) {
  dr_tmp <- paste("cex_data/diary_data/diary_data_raw/diary", (yr + 9), sep = "")
  fmld_raw[[yr]] <- list(NA)
  expd_raw[[yr]] <- list(NA)
  for (qt in 1:4) {
    fmld_raw[[yr]][[qt]] <- paste(dr_tmp, "/fmld", (yr + 9)*10 + qt, ".csv", sep = "") %>% read.csv() %>%
      mutate(quarter = qt, year = yr + 2009)
    names(fmld_raw[[yr]][[qt]]) <- fmld_raw[[yr]][[qt]] %>% names() %>% tolower()
    expd_raw[[yr]][[qt]] <- paste(dr_tmp, "/expd", (yr + 9)*10 + qt, ".csv", sep = "") %>% read.csv() %>%
      mutate(quarter = qt, year = yr + 2009)
    names(expd_raw[[yr]][[qt]]) <- expd_raw[[yr]][[qt]] %>% names() %>% tolower()
  }
  fmld[[yr]] <- rbindlist(fmld_raw[[yr]], fill = T)
  expd[[yr]] <- rbindlist(expd_raw[[yr]], fill = T)
}
rm(fmld_raw, expd_raw)
fmld_mg <- rbindlist(fmld, fill = T)
expd_mg <- rbindlist(expd, fill = T)

### Check null values
# 1. harmonize timing of expenditure in 2010 and 2011
expd_gp <- expd_mg %>%
  mutate(
    expnmo = expnmo %>% replace(is.na(.), substr(as.character(qredate), 3, 4)) %>% as.numeric(),
    expnyr = expnyr %>% replace(is.na(.), substr(as.character(qredate), 7, 10)) %>% as.numeric()
  ) %>%
  filter(!is.na(expnmo) & !is.na(ucc) & pub_flag == 2) %>% # There are hardly any nonfood uccs with pub_flag=1 (not included in published tables)
  select(newid, alloc, cost, ucc, quarter, year, expnmo, expnyr) %>%
  mutate(
    ucc_group = case_when(
      ucc %/% 10000 %in% c(1) ~ "Cereals",
      ucc %/% 10000 %in% c(2) ~ "Bakery products",
      ucc %/% 10000 %in% c(3) ~ "Beef",
      ucc %/% 10000 %in% c(4) ~ "Pork",
      ucc %/% 10000 %in% c(5) ~ "Lamb",
      ucc %/% 10000 %in% c(6) ~ "Poultry",
      ucc %/% 10000 %in% c(7) ~ "Fish",
      ucc %/% 10000 %in% c(8) ~ "Eggs",
      ucc %/% 10000 %in% c(9) ~ "Milk",
      ucc %/% 10000 %in% c(10) ~ "Dairy products",
      ucc %/% 10000 %in% c(11) ~ "Fresh fruits",
      ucc %/% 10000 %in% c(12) ~ "Fresh vegetables",
      ucc %/% 10000 %in% c(13) ~ "Processed fruits",
      ucc %/% 10000 %in% c(14) ~ "Processed vegetables",
      ucc %/% 10000 %in% c(15) ~ "Sugar",
      ucc %/% 10000 %in% c(16) ~ "Fat and Oil",
      ucc %/% 10000 %in% c(17) ~ "Nonalcoholic beverages",
      ucc %/% 10000 %in% c(18) ~ "Miscellaneous foods",
      ucc %/% 10000 %in% c(19) ~ "FAFH",
      ucc %/% 10000 %in% c(20) | ucc == 790420 ~ "Alcoholic beverages",
      TRUE ~ "Nonfood"
    )
  ) %>%
  mutate(
    product = case_when(
      ucc_group %in% c("Cereals", "Bakery products") ~ "Cereals and bakery",
      ucc_group %in% c("Beef", "Pork", "Lamb", "Poultry", "Fish") ~ "Meat",
      ucc_group %in% c("Eggs") ~ "Eggs",
      ucc_group %in% c("Milk", "Dairy products") ~ "Dairy",
      ucc_group %in% c("Fresh fruits", "Fresh vegetables", "Processed fruits", "Processed vegetables") ~ "Fruits and vegetables",
      ucc_group %in% c("Sugar", "Fat and Oil", "Miscellaneous foods") ~ "Other foods",
      ucc_group %in% c("Nonalcoholic beverages") ~ "Nonalcoholic beverages",
      ucc_group %in% c("Alcoholic beverages") ~ "Alcoholic beverages",
      ucc_group %in% c("FAFH") ~ "FAFH",
      ucc_group %in% c("Nonfood") ~ "Nonfood"
    )
  )

### Allocate the proper month to households
# This code assigns household to month based on the month the household purchased the most, if evenly split, then later month (by Abby)
expd_mo <- expd_gp %>%
  group_by(newid, expnmo, expnyr) %>%
  summarise(cost_tot = sum(cost, na.rm = T)) 

expd_max <- expd_mo %>%
  group_by(newid) %>%
  summarise(cost_max = max(cost_tot)) %>%
  merge(expd_mo, by = "newid", all.y = T) %>%
  filter(cost_tot == cost_max) %>%
  slice_max(expnmo, by = "newid") %>%
  rename(expnmo_alloc = expnmo, expnyr_alloc = expnyr)

expd_mo_alloc <- expd_gp %>%
  group_by(newid, product, expnmo, expnyr) %>%
  summarise(cost_mo = sum(cost, na.rm = T)) %>%
  merge(expd_max[, c("newid", "expnmo_alloc", "expnyr_alloc")], by = c("newid"), all.x = T) %>%
  select(-c(expnmo, expnyr)) %>%
  group_by(newid, product, expnmo_alloc, expnyr_alloc) %>%
  summarise(cost = sum(cost_mo, na.rm = T))

rm(expd_mo, expd_max)
  
### Calculate the expenditure share
# drop hh with expenditure share == 1
expd_hh <- expd_mo_alloc %>%
  group_by(newid) %>%
  summarise(cost_tot = sum(cost, na.rm = T)) %>%
  merge(expd_mo_alloc, by = "newid", all.y = T) %>%
  mutate(expn_share = cost/cost_tot) %>%
  filter(expn_share != 1)

### Merge HH characteristics and expenditure
# Harmonize fmld data set



cex_hh <- fmld_mg %>%
  select()

  
################################################################################
##### 2. Import price data
################################################################################
# Average price data, by census region
ap_dta <- read.delim("bls_price_data/ap.data.3.Food.txt") %>%
  mutate(unadj = ifelse(substr(series_id, 3, 3) == "U", 1, 0),
         area_code = substr(series_id, 4, 7),
         item_code = substr(series_id, 8, 13)
         ) %>%
  mutate(
    area = case_when(
      area_code == "0000" ~ "City Average",
      area_code == "0100" ~ "Northeast",
      area_code == "0200" ~ "Midwest",
      area_code == "0300" ~ "South",
      area_code == "0400" ~ "West"
    ),
    product = case_when(
      item_code ~ "",
      
      TRUE ~ "Nonfood"
    )
  ) 


# CPI data for FAH/FAFH





