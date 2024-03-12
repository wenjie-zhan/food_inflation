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
library(plotrix) # std.error()
library(haven) # read_dta()
library(readr)

################################################################################
##### 1. Import CEX data
################################################################################
# Import fmld and expd data
fmld <- c()
expd <- c()
for (yr in 1:13) {
    dr_tmp <- paste("cex_data/diary_data/diary_data_raw/diary", (yr + 9), sep = "")
    fmld[[yr]] <- list(NA)
    expd[[yr]] <- list(NA)
  for (qt in 1:4) {
    fmld[[yr]][[qt]] <- paste(dr_tmp, "/fmld", (yr + 9)*10 + qt, ".csv", sep = "") %>% read.csv()
    names(fmld[[yr]][[qt]]) <- fmld[[yr]][[qt]] %>% names() %>% tolower()
    expd[[yr]][[qt]] <- paste(dr_tmp, "/expd", (yr + 9)*10 + qt, ".csv", sep = "") %>% read.csv()
    names(expd[[yr]][[qt]]) <- expd[[yr]][[qt]] %>% names() %>% tolower()
  }
}

# Select variables of interest
fmld_wd <- c()
expd_wd <- c()
for (yr in 1:13) {
  for (qt in 1:4) {
    fmld_wd[[yr]][[qt]] <- fmld[[yr]][[qt]] %>%
      select(newid,finlwt21,wtrep01:wtrep44,age_ref,age2,sex_ref,sex2,educ_ref,educa2,hisp_ref,
             hisp2,ref_race,race2,marital1,hrsprwk1,hrsprwk2,wk_wrkd1,wk_wrkd2,fam_size,fam_type,
             childage,fincbefx,fincbef1:fincbef5,fincbefm,rec_fs,jfs_amt,jfs_amt1:jfs_amt5,jfs_amtm,
             alcbev,bls_urbn,smsastat,division,region,state,occulis1,occulis2,foodtot:miscfood)
  }
}











