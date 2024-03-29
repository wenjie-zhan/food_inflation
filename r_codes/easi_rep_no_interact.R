################################################################################
##### Project: food inflation
##### Program: Replication of EASI made easier, no interaction
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
set.seed(123456)
################################################################################
##### Estimation
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