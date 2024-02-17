########################################################################
## Description: Developing operating models for WSKJ MSE...
##
## Maintainer: DatenKraft - SCRS/ICCAT (Tropical Tuna Species Group)
## Author: Bruno Mourato
## Adapted by:
## Created:
## Version: 01
##
## Contact: brunomourato@unifesp.br
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
##
## Feb 16 2024
##
## Adrian Hordyk edits
##
## Add Observed Historical Data to OMs
##
## Add observed index to Data object in OMs
## see note further down for more info on this
##
########################################################################

#-----------------------------------------
######@> Packages list...
#-----------------------------------------

library(openMSE)
library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(janitor)
library(readxl)
library(ggmse)


## ----- Import OMs from SS3 ------ ##
##
## Does same as the commented out code below, with a few changes indicated in
## comments

# OM specifications:
nsim <- 100
proyears <- 40
interval <- 3
Obs <- MSEtool::Precise_Unbiased
Imp <- MSEtool::Perfect_Imp

# 2021 and 2022 observed catch
obs_catches <- c(20048.21, 21377.24)

SS_file_path <- "G:/Shared drives/BM shared/1. Projects/TOF Advisory/WSKJ_MSE_Assistance/SS_files"

SS_dirs <- c("WSKJ_EstRec93_Qnt25_h6", "WSKJ_EstRec93_Qnt50_h6", "WSKJ_EstRec93_Qnt75_h6",
             "WSKJ_EstRec93_Qnt25_h7", "WSKJ_EstRec93_Qnt50_h7", "WSKJ_EstRec93_Qnt75_h7",
             "WSKJ_EstRec93_Qnt25_h8", "WSKJ_EstRec93_Qnt50_h8", "WSKJ_EstRec93_Qnt75_h8")


## Extract data from SS ##
data <- SS2Data(file.path(SS_file_path, SS_dirs[1]))
# add 2021/22 observed catch
data@Cat <- cbind(data@Cat, matrix(obs_catches, nrow=1, ncol=length(obs_catches)))
data@Year <- c(data@Year, 2021:2022)
data@CV_Cat <- array(0.2, dim=dim(data@Cat))
# 5 indices used in assessment
matplot(t(data@AddInd[1,,]), type='b')

apply(!apply(data@AddInd[1,,], 1, is.na), 2, sum)

## !!! ASSUMPTION !!! ##
# PS_West has longest time-series
# using that index for the index used in the MSE
# for the CMPs that use an index
#

data@Ind <- array(data@AddInd[1,1,], dim=c(1, length(data@AddInd[1,1,])))
data@CV_Ind <- array(data@CV_AddInd[1,1,], dim=dim(data@Ind))
# drop rest of indices, otherwise causes an issue
# !! Muliple indices can be included in OM but require custom MPs that are designed to use multiple indices !! #
data@AddInd <- array(NA, c(1,1,1))
data@AddIndType <- NA
data@AddIndV <- array(NA, c(1,1,1))
data@AddIunits <- NA

## ! loop below assumes observed data doesn't change between OMs ! ##
## Currently the case, but need to add this inside the loop if that changes ##

OM_list <- vector('list', length(SS_dirs)*3)

# import SS3 OMs
for (i in seq_along(SS_dirs)) {
  ssdir <- file.path(SS_file_path, SS_dirs[i])
  name <-  SS_dirs[i]
  om <- SS2OM(ssdir, nsim=nsim, proyears=proyears, interval = interval,
              Obs=Obs, Imp=Imp, Name=name)

  # Add catchability parameter q = 1
  # om@cpars$Find == fishing mortality
  # this was missing from the `SS2OM` code - fixed in dev version
  om@cpars$qs <- rep(1, om@nsim)

  # add data
  om@cpars$Data <- data

  OM_list[[i]] <- om
  rm(om)
}

## Add other OMs

# 10% higher than TAC
for (i in 10:18) {
  om_match <- i %% 9
  if (om_match==0) om_match <- 9
  om <- OM_list[[om_match]]
  om@TACFrac<-c(1.1,1.1)
  om@Name = paste0(om@Name,"_ImpErr_10%")
  OM_list[[i]] <- om
}

# 20% higher than TAC
for (i in 19:27) {
  om_match <- i %% 9
  if (om_match==0) om_match <- 9
  om <- OM_list[[om_match]]
  om@TACFrac <-c(1.2,1.2)
  om@Name = paste0(om@Name,"_ImpErr_20%")
  OM_list[[i]] <- om
}

saveRDS(OM_list, file = file.path('00_OMs', "SS_Operating_models_v01_fullgrid_16022024.rds"))



#
#
# ## WSKJ_EstRec93_Qnt25_h6
# SSdir1 <- "WSKJ_EstRec93_Qnt25_h6"
# SSdir1 <- "WSKJ_EstRec93_Qnt25_h6"
# OM1 <- SS2OM(SSdir1, nsim = 100,proyears = 40,reps = 100,
#              maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#              Obs = MSEtool::Precise_Unbiased,
#              Imp = MSEtool::Perfect_Imp,
#              gender = 1,silent = FALSE,
#              Name = "OM WSKJ_EstRec93_Qnt25_h6",
#              Source = "ICCAT 2022 SKJ assessment",
#              Author = "Bruno Mourato",
#              report = FALSE,
#              filename = "OM_WSKJ_EstRec93_Qnt25_h6",
#              dir = output.dir,
#              open_file = TRUE)
#
#
#
#
# #-----------------------------------------
# ######@> Set Working directory file where to store the results...
# #-----------------------------------------
# File<-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2"
# folder <- "Operating_models_v01"
# output.dir = file.path(File,folder)
# dir.create(output.dir,showWarnings = F)
# setwd(output.dir)
#
# ######@> Create directory...
#
# #-----------------------------------------
# ######@> Loading data...
# #-----------------------------------------
# SS_file_path <- "C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/"
#
# SS_file_path <- "G:/Shared drives/BM shared/1. Projects/TOF Advisory/WSKJ_MSE_Assistance/SS_files/"
#
# SS_dirs <- c("WSKJ_EstRec93_Qnt25_h6", "WSKJ_EstRec93_Qnt50_h6", "WSKJ_EstRec93_Qnt75_h6",
#              "WSKJ_EstRec93_Qnt25_h7", "WSKJ_EstRec93_Qnt50_h7", "WSKJ_EstRec93_Qnt75_h7",
#              "WSKJ_EstRec93_Qnt25_h8", "WSKJ_EstRec93_Qnt50_h8", "WSKJ_EstRec93_Qnt75_h8")
#
#
#
#
# ## WSKJ_EstRec93_Qnt25_h6
# SSdir1 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt25_h6"
# OM1 <- SS2OM(SSdir1, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt25_h6",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt25_h6",
#            dir = output.dir,
#            open_file = TRUE)
#
#
#
# OM1@Name = "OM WSKJ_EstRec93_Qnt25_h6"
# plot_SS2OM(OM1,
#            SSdir1,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt25_h6",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
#
# ## WSKJ_EstRec93_Qnt50_h6
# SSdir2 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt50_h6"
# OM2<-SS2OM(SSdir2, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt50_h6",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt50_h6",
#            dir = output.dir,
#            open_file = TRUE)
# OM2@Name = "OM WSKJ_EstRec93_Qnt50_h6"
# plot_SS2OM(OM2,
#            SSdir2,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt50_h6",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
# ## WSKJ_EstRec93_Qnt75_h6
# SSdir3 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt75_h6"
# OM3<-SS2OM(SSdir3, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt75_h6",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt75_h6",
#            dir = output.dir,
#            open_file = TRUE)
# OM3@Name = "OM WSKJ_EstRec93_Qnt75_h6"
# plot_SS2OM(OM3,
#            SSdir3,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt75_h6",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
# ## WSKJ_EstRec93_Qnt25_h7
# SSdir4 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt25_h7"
# OM4<-SS2OM(SSdir4, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt25_h7",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt25_h7",
#            dir = output.dir,
#            open_file = TRUE)
#
# OM4@Name = "OM WSKJ_EstRec93_Qnt25_h7"
# plot_SS2OM(OM4,
#            SSdir4,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt25_h7",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
#
# ## WSKJ_EstRec93_Qnt50_h7
# SSdir5 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt50_h7"
# OM5<-SS2OM(SSdir5, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt50_h7",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt50_h7",
#            dir = output.dir,
#            open_file = TRUE)
# OM5@Name = "OM WSKJ_EstRec93_Qnt50_h7"
# plot_SS2OM(OM5,
#            SSdir5,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt50_h7",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
# ## WSKJ_EstRec93_Qnt75_h7
# SSdir6 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt75_h7"
# OM6<-SS2OM(SSdir6, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt75_h7",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt75_h7",
#            dir = output.dir,
#            open_file = TRUE)
# OM6@Name = "OM WSKJ_EstRec93_Qnt75_h7"
#
# plot_SS2OM(OM6,
#            SSdir6,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt75_h7",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
#
# ## WSKJ_EstRec93_Qnt25_h8
# SSdir7 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt25_h8"
# OM7<-SS2OM(SSdir7, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt25_h8",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt25_h8",
#            dir = output.dir,
#            open_file = TRUE)
# OM7@Name = "OM WSKJ_EstRec93_Qnt25_h8"
# plot_SS2OM(OM7,
#            SSdir7,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt25_h8",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
#
# ## WSKJ_EstRec93_Qnt50_h8
# SSdir8 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt50_h8"
# OM8<-SS2OM(SSdir8, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt50_h8",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt50_h8",
#            dir = output.dir,
#            open_file = TRUE)
# OM8@Name = "OM WSKJ_EstRec93_Qnt50_h8"
# plot_SS2OM(OM8,
#            SSdir8,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt50_h8",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
# ## WSKJ_EstRec93_Qnt75_h8
# SSdir9 <-"C:/Users/moura/Dropbox/WSKJ_MSE_fase2/SS_files/WSKJ_EstRec93_Qnt75_h8"
# OM9<-SS2OM(SSdir9, nsim = 100,proyears = 40,reps = 100,
#            maxF = 3,seed = 1,interval = 3,pstar = 0.5,
#            Obs = MSEtool::Precise_Unbiased,
#            Imp = MSEtool::Perfect_Imp,
#            gender = 1,silent = FALSE,
#            Name = "OM WSKJ_EstRec93_Qnt75_h8",
#            Source = "ICCAT 2022 SKJ assessment",
#            Author = "Bruno Mourato",
#            report = FALSE,
#            filename = "OM_WSKJ_EstRec93_Qnt75_h8",
#            dir = output.dir,
#            open_file = TRUE)
# OM9@Name = "OM_WSKJ_EstRec93_Qnt75_h8"
# plot_SS2OM(OM9,
#            SSdir9,
#            gender = 1,
#            filename = "WSKJ_EstRec93_Qnt75_h8",
#            dir = output.dir,
#            open_file = TRUE,
#            silent = FALSE)
#
# ###Implementation error 10%
#
# OM10 <-OM1
# OM10@Name = paste0(OM1@Name,"_ImpErr_10%")
# OM10@TACSD<-c(0.1,0.1)
#
# OM11 <-OM2
# OM11@Name = paste0(OM2@Name,"_ImpErr_10%")
# OM11@TACSD<-c(0.1,0.1)
#
# OM12 <-OM3
# OM12@Name = paste0(OM3@Name,"_ImpErr_10%")
# OM12@TACSD<-c(0.1,0.1)
#
# OM13 <-OM4
# OM13@Name = paste0(OM4@Name,"_ImpErr_10%")
# OM13@TACSD<-c(0.1,0.1)
#
# OM14 <-OM5
# OM14@Name = paste0(OM5@Name,"_ImpErr_10%")
# OM14@TACSD<-c(0.1,0.1)
#
# OM15 <-OM6
# OM15@Name = paste0(OM6@Name,"_ImpErr_10%")
# OM15@TACSD<-c(0.1,0.1)
#
# OM16 <-OM7
# OM16@Name = paste0(OM7@Name,"_ImpErr_10%")
# OM16@TACSD<-c(0.1,0.1)
#
# OM17 <-OM8
# OM17@Name = paste0(OM8@Name,"_ImpErr_10%")
# OM17@TACSD<-c(0.1,0.1)
#
# OM18 <-OM9
# OM18@Name = paste0(OM9@Name,"_ImpErr_10%")
# OM18@TACSD<-c(0.1,0.1)
#
#
# ###Implementation error 20%
#
# OM19 <-OM1
# OM19@Name = paste0(OM1@Name,"_ImpErr_20%")
# OM19@TACSD<-c(0.2,0.2)
#
# OM20 <-OM2
# OM20@Name = paste0(OM2@Name,"_ImpErr_20%")
# OM20@TACSD<-c(0.2,0.2)
#
# OM21 <-OM3
# OM21@Name = paste0(OM3@Name,"_ImpErr_20%")
# OM21@TACSD<-c(0.1,0.1)
#
# OM22 <-OM4
# OM22@Name = paste0(OM4@Name,"_ImpErr_20%")
# OM22@TACSD<-c(0.2,0.2)
#
# OM23 <-OM5
# OM23@Name = paste0(OM5@Name,"_ImpErr_20%")
# OM23@TACSD<-c(0.2,0.2)
#
# OM24 <-OM6
# OM24@Name = paste0(OM6@Name,"_ImpErr_20%")
# OM24@TACSD<-c(0.2,0.2)
#
# OM25 <-OM7
# OM25@Name = paste0(OM7@Name,"_ImpErr_20%")
# OM25@TACSD<-c(0.2,0.2)
#
# OM26 <-OM8
# OM26@Name = paste0(OM8@Name,"_ImpErr_20%")
# OM26@TACSD<-c(0.2,0.2)
#
# OM27 <-OM9
# OM27@Name = paste0(OM9@Name,"_ImpErr_20%")
# OM27@TACSD<-c(0.2,0.2)
#
# ########################################################################################################
#
# OMs<-list()
# OMs[[1]]<-OM1
# OMs[[2]]<-OM2
# OMs[[3]]<-OM3
# OMs[[4]]<-OM4
# OMs[[5]]<-OM5
# OMs[[6]]<-OM6
# OMs[[7]]<-OM7
# OMs[[8]]<-OM8
# OMs[[9]]<-OM9
# OMs[[10]]<-OM10
# OMs[[11]]<-OM11
# OMs[[12]]<-OM12
# OMs[[13]]<-OM13
# OMs[[14]]<-OM14
# OMs[[15]]<-OM15
# OMs[[16]]<-OM16
# OMs[[17]]<-OM17
# OMs[[18]]<-OM18
# OMs[[19]]<-OM19
# OMs[[20]]<-OM20
# OMs[[21]]<-OM21
# OMs[[22]]<-OM22
# OMs[[23]]<-OM23
# OMs[[24]]<-OM24
# OMs[[25]]<-OM25
# OMs[[26]]<-OM26
# OMs[[27]]<-OM27
#
# saveRDS(OMs, file = paste0(output.dir,"/SS_Operating_models_v01_fullgrid_13082022.rds"))
#
#
#
#
#
