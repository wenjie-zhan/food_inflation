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
library(systemfit) # estimate SUR

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
##### 1. Import price data
################################################################################
### National CPI, by product
# Available area code: US city average; Size A; Size B/C
cpi_natl <- read.delim("bls_price_data/cu.data.0.Current.txt") %>%
  mutate(unadj = ifelse(substr(series_id, 3, 3) == "U", 1, 0),
         monthly = ifelse(substr(series_id, 4, 4) == "R", 1, 0),
         area_code = substr(series_id, 5, 8),
         base82_84 = ifelse(substr(series_id, 9, 9) == "S", 1, 0),
         item_code = substr(series_id, 9, 16)
  ) %>%
  mutate(
    group = case_when(
      str_trim(item_code) == "SEFA" ~ "Cereals and cecreal products",
      str_trim(item_code) == "SEFB" ~ "Bakery products",
      str_trim(item_code) == "SEFC" ~ "Beef and veal",
      str_trim(item_code) == "SEFD" ~ "Pork",
      str_trim(item_code) == "SEFE" ~ "Other meats",
      str_trim(item_code) == "SEFF" ~ "Poultry",
      str_trim(item_code) == "SEFG" ~ "Fish and seafood",
      str_trim(item_code) == "SEFH" ~ "Eggs",
      str_trim(item_code) == "SEFJ" ~ "Dairy",
      str_trim(item_code) == "SEFK" ~ "Fresh fruits",
      str_trim(item_code) == "SEFL" ~ "Fresh vegetables",
      str_trim(item_code) == "SEFM" ~ "Processed fruits and vegetables",
      str_trim(item_code) == "SEFR" ~ "Sugar and sweets",
      str_trim(item_code) == "SEFS" ~ "Fat and oil",
      str_trim(item_code) == "SEFN" ~ "Juices and nonalchoholic drinks",
      str_trim(item_code) == "SEFP" ~ "Beverage materials including coffee and tea",
      str_trim(item_code) == "SEFT" ~ "Other foods",
      str_trim(item_code) == "SEFV" ~ "FAFH",
      str_trim(item_code) == "SEFW" ~ "Alcoholic beverages at home",
      str_trim(item_code) == "SEFX" ~ "Alcoholic beverages away from home",
      str_trim(item_code) == "SA0L1" ~ "Nonfood", # All items less food, does not exist
      str_trim(item_code) == "SAF111" ~ "Cereals and bakery products",
      str_trim(item_code) == "SAF1121" ~ "Meats",
      str_trim(item_code) == "SAF113" ~ "Fruits and vegetables",
      str_trim(item_code) == "SAF115" ~ "Other foods",
      str_trim(item_code) == "SAF114" ~ "Nonalchoholic beverages",
      str_trim(item_code) == "SAF116" ~ "Alcoholic beverages",
      TRUE ~ "Delete"
    )
  ) %>%
  filter( year %in% c(2010:2022) & !(period %in% c("M13", "S01", "S02", "S03")) 
         & group %in% c("Cereals and bakery products",
                        "Meats",
                        "Eggs",
                        "Dairy",
                        "Fruits and vegetables",
                        "Other foods",
                        "Nonalchoholic beverages",
                        "Alcoholic beverages",
                        "FAFH",
                        "Nonfood")
         & area_code == "0000" & unadj == 0) %>%
  mutate(month = gsub("M", "", period) %>% as.numeric(), year = year %>% as.numeric()) %>%
  rename(product = group)


################################################################################
##### 2. Import CEX data: expenditure and demographic characteristics
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
      ucc %/% 10000 %in% c(1) ~ "Cereals and cecreal products",
      ucc %/% 10000 %in% c(2) ~ "Bakery products",
      ucc %/% 10000 %in% c(3) ~ "Beef and veal", 
      ucc %/% 10000 %in% c(4) ~ "Pork", 
      ucc %/% 10000 %in% c(5) ~ "Other meats",
      ucc %/% 10000 %in% c(6) ~ "Poultry",
      ucc %/% 10000 %in% c(7) ~ "Fish and seafood", 
      ucc %/% 10000 %in% c(8) ~ "Eggs",
      ucc %/% 10000 %in% c(9:10) ~ "Dairy and related products",
      ucc %/% 10000 %in% c(11) ~ "Fresh fruits",
      ucc %/% 10000 %in% c(12) ~ "Fresh vegetables",
      ucc %/% 10000 %in% c(13:14) ~ "Processed fruits and vegetables",
      ucc %/% 10000 %in% c(15) ~ "Sugar and sweets",
      ucc %/% 10000 %in% c(16) ~ "Fat and oil",
      ucc %in% c(170110, 170210, 170510, 170530:170533) ~ "Juices and nonalchoholic drinks",
      ucc %in% c(170310, 170410, 170520) ~ "Beverage materials including coffee and tea",
      ucc %/% 10000 %in% c(18) ~ "Other foods",
      ucc %/% 10000 %in% c(19) ~ "FAFH",
      ucc %in% c(200511:200520) ~ "Alcoholic beverages at home",
      ucc %in% c(200521:200536, 740420) ~ "Alcoholic beverages away from home",
      TRUE ~ "Nonfood"
    )
  ) %>%
  mutate(
    product = case_when(
      ucc_group %in% c("Cereals and cecreal products", "Bakery products") ~ "Cereals and bakery products",
      ucc_group %in% c("Beef and veal", "Pork", "Other meats", "Poultry", "Fish and seafood") ~ "Meats",
      ucc_group %in% c("Eggs") ~ "Eggs",
      ucc_group %in% c("Dairy and related products") ~ "Dairy",
      ucc_group %in% c("Fresh fruits", "Fresh vegetables", "Processed fruits and vegetables") ~ "Fruits and vegetables",
      ucc_group %in% c("Sugar and sweets", "Fat and oil", "Other foods") ~ "Other foods",
      ucc_group %in% c("Juices and nonalchoholic drinks", "Beverage materials including coffee and tea") ~ "Nonalchoholic beverages",
      ucc_group %in% c("Alcoholic beverages at home", "Alcoholic beverages away from home") ~ "Alcoholic beverages",
      ucc_group %in% c("FAFH") ~ "FAFH",
      ucc_group %in% c("Nonfood") ~ "Nonfood"
    )
  )

### Allocate the proper month to households
# This code assigns household to month based on the month the household purchased the most, if evenly split, then later month (by Abby)
# We need to merge expd file to fmld file to determine the allocated month and year of expenditure because in fmld there are only start month and year of diary
# Calcluate total monthly cost
expd_mo <- expd_gp %>%
  group_by(newid, expnmo, expnyr) %>%
  summarise(cost_tot = sum(cost, na.rm = T)) 

# Only keep the monthly cost if it equals total max cost to get allocated month
expd_max <- expd_mo %>%
  group_by(newid) %>%
  summarise(cost_max = max(cost_tot)) %>%
  merge(expd_mo, by = "newid", all.y = T) %>%
  filter(cost_tot == cost_max) %>%
  slice_max(expnmo, by = "newid") %>%  # if costs are split even, take the later month.
  rename(expnmo_alloc = expnmo, expnyr_alloc = expnyr)

# Assign allocated month to cu-by-product
expd_mo_alloc <- expd_gp %>%
  group_by(newid, product) %>%
  summarise(cost = sum(cost, na.rm = T)) %>%
  merge(expd_max[, c("newid", "expnmo_alloc", "expnyr_alloc")], by = c("newid"), all.x = T) 
  
### Calculate the expenditure share
# drop hh with expenditure share == 1
expd_cu <- expd_mo_alloc %>%
  group_by(newid) %>%
  summarise(cost_tot = sum(cost, na.rm = T)) %>%
  merge(expd_mo_alloc, by = "newid", all.y = T) %>%
  mutate(expn_share = cost/cost_tot) %>%
  filter(expn_share != 1) %>%
  rename(month = expnmo_alloc, year = expnyr_alloc) %>%
  merge(cpi_natl[,c("month", "year", "product", "value")], by = c("month", "year", "product")) %>%
  rename(price = value)

# reshape expenditure data: long to wide
# cu_exp_share <- expd_cu %>%
#   select(newid, product, month, year, expn_share, price) %>%
#   pivot_wider(
#     names_from = "product",
#     values_from = "expn_share"
#   ) 
# %>%
#   mutate(across("Bakery products":"Alcoholic beverages at home", ~replace(., is.na(.), 0)))

### Merge HH characteristics and expenditure
# Harmonize fmld data set
cex_cu <- fmld_mg %>%
  select(newid, hhid, strtmnth, strtyear, finlwt21, wtrep01:wtrep44, 
         age_ref, age2, sex_ref, sex2, educ_ref, educa2, hisp_ref, hisp2, 
         ref_race, race2, marital1, hrsprwk1, hrsprwk2, wk_wrkd1, wk_wrkd2, 
         whynwrk1, whynwrk2, fam_size, fam_type, childage, fincbefx, fincbef1:fincbef5, 
         fincbefm, rec_fs, jfs_amt, jfs_amt1:jfs_amt5, jfs_amtm, othregx1:othregx5, othregxm, 
         alcbev, freemlx, bls_urbn, smsastat, division, region, state, occulis1, occulis2, foodtot:alcbev)

################################################################################
##### 3. Merge data sets
################################################################################


################################################################################
##### 4. EASI demand estimation
################################################################################
# set number of equations and prices and demographic characteristics and convergence criterion
neqminus1 <- 7
neq <- 8
nprice <- 9
ndem <- 5
npowers <- 5
conv_crit <- 0.000001

hixdata <- read_dta("replication_LP2009/hixdata.dta") %>%
  rename(
    s1 = sfoodh,
    s2 = sfoodr,
    s3 = srent,
    s4 = soper,
    s5 = sfurn,
    s6 = scloth,
    s7 = stranop,
    s8 = srecr,
    s9 = spers,
    p1 = pfoodh,
    p2 = pfoodr,
    p3 = prent,
    p4 = poper,
    p5 = pfurn,
    p6 = pcloth,
    p7 = ptranop,
    p8 = precr,
    p9 = ppers,
    z1 = age,
    z2 = hsex,
    z3 = carown,
    z4 = time,
    z5 = tran
  )

# normalised prices are what enter the demand system
# generate normalised prices, backup prices (they get deleted), and Ap
hix_dta <- hixdata %>%
  mutate(across(starts_with("p"), list(np = ~ . - nprice), .names = "n{col}")) %>%  # Create np[j]
  mutate(across(starts_with("np"), list(backup = ~ .), .names = "{col}_backup")) %>%  # Create np[j]_backup
  mutate(across(starts_with("p"), list(Ap = ~ 0), .names = "A{col}")) %>%
  select(-c(np9, np9_backup, Ap9)) %>%
  mutate(across(starts_with("s"), list(mean_s = ~ mean(., na.rm = T)), .names = "mean_{col}")) %>% # w^bar is the average budget shares across consumers in our sample
  mutate(across(starts_with('mean_s'), .names = 'mxp_{col}') * pick(starts_with('p')),
         across(starts_with('s'), .names = 'sxp_{col}') * pick(starts_with('p'))) %>%
  rowwise() %>%
  mutate(sum_mxp = sum(c_across("mxp_mean_s1":"mxp_mean_s9"), na.rm = T),
         sum_sxp = sum(c_across("sxp_s1":"sxp_s9"), na.rm = T)) %>%
  mutate(y_tilda = log_y - sum_mxp,
         y_stone = log_y - sum_sxp) %>%
  select(-c(mxp_mean_s1:mxp_mean_s9, sxp_s1:sxp_s9, sum_mxp, sum_sxp)) %>%
  mutate(pAp = 0)

# list of functions of (implicit) utility, y: fill them in, and add them to ylist below
# alternatively, fill ylist and yinstlist with the appropriate variables and instruments
# create polynomial terms for y
ylist <- c()
yinstlist <- c()
for (j in 1:npowers) {
  hix_dta[[paste0("y", j)]] <- hix_dta$y_stone^j
  hix_dta[[paste0("y", j, "_inst")]] <- hix_dta$y_tilda^j
  
  ylist <- c(ylist, paste0("y", j))
  yinstlist <- c(yinstlist, paste0("y", j, "_inst"))
}

# create equation system
eqlist <- c()
eq <- c("y1 ")
for (num in 2:npowers) {
  eq <- paste(eq, " + y", num, sep="")
}

for (num in 1:ndem) {
  eq <- paste(eq, " + z", num, sep="")
}

for (num in 1:neq) {
  eq <- paste(eq, " + np", num, sep="")
}

for (i in 1:neq){
  eqlist[[i]] <- paste("s", i, " ~ " , eq, sep = "") %>% formula()
}

# create list of constraints
# symmetry of price coefficients
const <- matrix(0,2,neq*(neq-1)/2)
k <- 0
for (i in 1:(neq-1)){
  for (j in ((i+1):neq)){
    k <- k+1
    const[1,k] <- paste("eq",i,"_np",j,"-","eq",j,"_np",i,"=0",sep="")
  }}
const <- t(const)
const <- const[,1]

# create list of instruments
instlist <- c()
inst <- c("y1_inst ")
for (num in 2:npowers) {
  inst <- paste(inst, " + y", num, "_inst", sep="")
}

for (num in 1:ndem) {
  inst <- paste(inst, " + z", num, sep="")
}

for (num in 1:neq) {
  inst <- paste(inst, " + np", num, sep="")
}

for (i in 1:neq){
  instlist[[i]] <- paste(" ~" , inst, sep = "") %>% formula()
}


hix_dta2 <- hix_dta %>%
  mutate(
    y = y_stone,
    y_backup = y_stone,
    y_old = y_stone,
    y_change = 0
  )

crit_test <- 1
iter <- 0

while (crit_test>conv_crit) {
  iter <- iter+1
  fit3sls <- systemfit(eqlist, "3SLS", inst = instlist, data = hix_dta2, restrict.matrix = const)
  params=fit3sls$coefficients
  
  hix_dta2$pAp <- 0
  hix_dta2$y_old <- hix_dta2$y
  
  pred <- predict(fit3sls,hix_dta2)
  for (i in 1:neq){
    hix_dta2[,paste("s",i, "hat",sep="")] <- pred[,i]
  }
  
  # make all np = 0, and predicted Values with p=0 and no interactions
  for (i in 1:neq){
    hix_dta2[,paste("np",i,sep="")] <- 0
  }
  pred2 <- predict(fit3sls,hix_dta2)
  for (i in 1:neq){
    hix_dta2[,paste("s",i, "hat_p0",sep="")] <- pred2[,i]
  }
  
  # recover np from np_backup
  for (i in 1:neq){
    hix_dta2[, paste("np",i,sep="")] <- hix_dta2[,paste("np",i, "_backup",sep="")]
    hix_dta2[, paste("Ap",i,sep="")] <- hix_dta2[, paste("s",i, "hat",sep="")] - hix_dta2[, paste("s",i, "hat_p0",sep="")] 
    hix_dta2[, "pAp"] <- hix_dta2[, "pAp"] + hix_dta2[, paste("np",i,sep="")]*hix_dta2[, paste("Ap",i,sep="")]
    hix_dta2 <- hix_dta2 %>% select(-c(paste("s",i, "hat",sep=""), paste("s",i, "hat_p0",sep="")))
  }
  
  hix_dta2$pAp <- round(hix_dta2$pAp*1000000 + 0.5)/1000000
  hix_dta2$y <- hix_dta2$y_stone + 0.5*hix_dta2$pAp
  
  for (i in 1:npowers){
    hix_dta2[,paste("y",i,sep="")] <- hix_dta2$y^i
  }
  
  hix_dta2$y_change = abs(hix_dta2$y - hix_dta2$y_old)
  
  crit_test <- max(hix_dta2$y_change)
}  

# now, create the instrument
hix_dta2$y_inst <- hix_dta2$y_tilda + 0.5*hix_dta2$pAp

for (i in 1:npowers){
  hix_dta2[,paste0("y", i, "_inst")] <- hix_dta2$y_inst^i
}

# run three stage least squares on the model with no py, pz or yz interactions, and then iterate to convergence
# note that the difference in predicted values between p and p=0 is Ap
# reset the functions of y
hix_dta2$y <- hix_dta2$y_stone
for (i in 1:npowers){
  hix_dta2[,paste0("y", i)] <- hix_dta2$y^i
}
hix_dta2$y_old <- hix_dta2$y_stone
hix_dta2$y_change <- 0
crit_test <- 0

while (crit_test>conv_crit) {
  iter <- iter+1
  fit3sls <- systemfit(eqlist, "3SLS", inst = instlist, data = hix_dta2, restrict.matrix = const)
  params=fit3sls$coefficients
  
  hix_dta2$pAp <- 0
  hix_dta2$y_old <- hix_dta2$y
  
  pred <- predict(fit3sls,hix_dta2)
  for (i in 1:neq){
    hix_dta2[,paste("s",i, "hat",sep="")] <- pred[,i]
  }
  
  # make all np = 0, and predicted Values with p=0 and no interactions
  for (i in 1:neq){
    hix_dta2[,paste("np",i,sep="")] <- 0
  }
  pred2 <- predict(fit3sls,hix_dta2)
  for (i in 1:neq){
    hix_dta2[,paste("s",i, "hat_p0",sep="")] <- pred2[,i]
  }
  
  # recover np from np_backup
  for (i in 1:neq){
    hix_dta2[, paste("np",i,sep="")] <- hix_dta2[,paste("np",i, "_backup",sep="")]
    hix_dta2[, paste("Ap",i,sep="")] <- hix_dta2[, paste("s",i, "hat",sep="")] - hix_dta2[, paste("s",i, "hat_p0",sep="")] 
    hix_dta2[, "pAp"] <- hix_dta2[, "pAp"] + hix_dta2[, paste("np",i,sep="")]*hix_dta2[, paste("Ap",i,sep="")]
    hix_dta2 <- hix_dta2 %>% select(-c(paste("s",i, "hat",sep=""), paste("s",i, "hat_p0",sep="")))
  }
  
  hix_dta2$pAp <- round(hix_dta2$pAp*1000000 + 0.5)/1000000
  hix_dta2$y <- hix_dta2$y_stone + 0.5*hix_dta2$pAp
  
  for (i in 1:npowers){
    hix_dta2[,paste("y",i,sep="")] <- hix_dta2$y^i
  }
  
  hix_dta2$y_change = abs(hix_dta2$y - hix_dta2$y_old)
  
  crit_test <- max(hix_dta2$y_change)
}  

# final estimate
fit3sls <- systemfit(eqlist, "3SLS", inst = instlist, data = hix_dta2, restrict.matrix = const)


