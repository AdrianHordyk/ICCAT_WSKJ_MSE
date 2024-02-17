########################################################################
## Description: Building Slick presentation...
##
## Maintainer: Datenkraft LTDA
## Author: Rodrigo Sant'Ana
## Created: dom abr 23 00:53:48 2023 (-0300)
## Version: 0.0.1
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

# Edits: Adrian Hordyk
# February 2024
# Updated for new Slick version
# Add to GitHub repo and changed directory references accordingly

########################################################################
######@> Loading R packages...

######@> Package list...
library(Slick)
library(MSEtool)
library(RColorBrewer)
library(viridis)

if (! packageVersion('Slick')>='1.0.15')
  stop('Update Slick!')

########################################################################
######@> Setup R...

######@> Path to MSE...
path00 <- paste0("02_MSEs")

######@> Path to PMs and MPs...
# path01 <- paste0("../../../../Finalizados/W-SKJ_MSE_2023_01/",
#                  "MSE_SKJ_2023_parte_III_ver01/R-Work/",
#                  "Back_to_old_MSE_ver00/")

######@> Custom R environment...
options(scipen = 16)
options(digits = 2)

######@> Listing MSE files...
MSE.files <- list.files(path00)

########################################################################
######@> Setup MSE - Slick...

######@> Preparing the design structure...
Design <- data.frame(
    OM = paste0("OM", sprintf("%03d", 1:27)),
    GrowthVector = rep(c("25th", "50th", "75th"), 9),
    Steepness = c(0.6, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 0.8,
                  0.6, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 0.8,
                  0.6, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 0.8),
    # SigmaR = rep(0.4, 27),
    Scenario = c(rep("Perfect TAC Imp.", 9),
                 rep("10% Overage TAC Imp.", 9),
                 rep("20% Overage TAC Imp.", 9)))

######@> Preparing to loading MSE dataset...
Design_Mat <- Design[, 2:ncol(Design)]
for(i in 1:ncol(Design_Mat)) {
    Design_Mat[, i] <- match(Design_Mat[, i], unique(Design_Mat[, i]))
}

######@> Building MSE list...
MSElist <- list()
for (i in 1:nrow(Design)) {
    fl <- MSE.files[i]
    MSElist[[i]] <- readRDS(file.path(path00, fl))
}


# Select MPs
MSElist02 <- list()
for(i in 1:27) {
    MSElist02[[i]] <- Sub(MSElist[[i]],
                          MPs = c("CC_30kt", "CC_40kt",
                                  "Iratio", "Islope1", "GB_slope",
                                  "SCA_100_40_SBMSY",
                                  "SP_100_40_SBMSY",
                                  "SPSS_100_40_SBMSY",
                                  "SP_03", "SP_04"))
}


######@> Management Procedures list...
## MPs <- MSElist[[1]]@MPs
## MP_Desc <- c("No Fishing - Reference MP",
##              "Effort fixed at the current level",
##              "Constant TAC - 15kt",
##              "Constant TAC - 20kt",
##              "Constant TAC - 25kt",
##              "Constant TAC - 30kt",
##              "Constant TAC - 35kt",
##              "Constant TAC - 40kt",
##              "Geromont and Butterworth Index slope Harvest Control Rule (DLMtool)",
##              "Mean Index Ratio (DLMtool)",
##              "Index Slope Tacking (DLMtool)",
##              "Statistical catch-at-age model with a 40-20 control rule based
## on depletion and minimum F at 10% of FMSY",
##              "Statistical catch-at-age model with an 100-40 control
## rule based on spawning biomass at MSY level and minimum F at 10% of FMSY",
##              "A surplus production model with a 40-20 control rule based on
## depletion and minimum F at 10% of FMSY",
##              "A surplus production model with an 80-40 control rule
## based on spawning biomass at MSY level and minimum F at 10% of FMSY")

MPs <- MSElist02[[1]]@MPs
MP_Desc <- c(## "No Fishing - Reference MP",
             ## "Effort fixed at the current level",
    ## "Constant TAC - 15kt",
    ## "Constant TAC - 20kt",
    ## "Constant TAC - 25kt",
    "Constant TAC - 30kt",
    ## "Constant TAC - 35kt",
    "Constant TAC - 40kt",
    "Geromont and Butterworth Index slope Harvest Control Rule (DLMtool)",
    "Mean Index Ratio (DLMtool)",
    "Index Slope Tacking (DLMtool)",
    ## "Statistical catch-at-age model with a 40-20 control rule based on depletion and minimum F at 10% of FMSY",
    "Statistical catch-at-age model with an 100-40 control
     rule based on spawning biomass at MSY level and minimum F at 10% of FMSY",
## "A surplus production model with a 40-20 control rule based on
## depletion and minimum F at 10% of FMSY",
    "A surplus production model with an 100-40 control rule
     based on spawning biomass at MSY level and minimum F at 10% of FMSY",
    "A state-space surplus production model with an 100-40 control rule
     based on spawning biomass at MSY level and minimum F at 10% of FMSY",
##     "A surplus production model with an 100-40 control rule based on
## spawning biomass at MSY level with associated maximum F at 80% and
## minimum F at 10% of FMSY with fixed TAC for the 1st management cycle (i.e.
## the first three years)",
##      "A state-space surplus production model with an 100-40 control rule
## based on spawning biomass at MSY level with associated maximum F at 80%
## and minimum F at 10% of FMSY with fixed TAC for the 1st management cycle
## (i.e. the first three years)",
"A surplus production model with an 100-40 control rule based on
spawning biomass at MSY level with associated maximum F at 80% and
minimum F at 10% of FMSY without fixed TAC for the 1st management cycle (i.e.
the first three years)",
"A state-spece surplus production model with an 100-40 control rule
based on spawning biomass at MSY level with associated maximum F at 80%
and minimum F at 10% of FMSY without fixed TAC for the 1st management cycle
(i.e. the first three years)"
## "A surplus production model with an 100-40 control rule based on
## spawning biomass at MSY level with associated maximum F at 80% and
## minimum F at 10% of FMSY without fixed TAC for the 1st management cycle (i.e.
## the first three years). For this CMP F was set three times larger",
## "A state-space surplus production model with an 100-40 control rule
## based on spawning biomass at MSY level with associated maximum F at 80%
## and minimum F at 10% of FMSY without fixed TAC for the 1st management cycle
## (i.e. the first three years). For this CMP F was set three times
## larger"
)

######@> Performance Metrics list...
source("script_prepare_PMs_ver00.R")
avail("PM")
PMs <- avail("PM")[c(1:3, 8:16)]

######@> Construct the SLICK object using the Make_SLICK function for
######@> MSEtool compatible MSEs...
fstYr <- 2021 # first projection year
obj <- Make_Slick(name =
                      "WSKJ MSE - 1st Intersessional Meeting of Panel 1 on Western Skipjack MSE, 20-21 Feb 2024",
                  MPs = MPs,
                  PMs = PMs,
                  Design = Design_Mat,
                  MP_Desc = MP_Desc,
                  SN = list(
                      Factor_Labels = c("GrowthVector",
                                        "Steepness",
                                        "Scenario"),
                      Labels = list(c("G=25th",
                                      "G=50th",
                                      "G=75th"),
                                    c("h=0.6",
                                      "h=0.7",
                                      "h=0.8"),
                                    c("Perfect TAC Imp.",
                                      "10% Overage TAC Imp.",
                                      "20% Overage TAC Imp.")
                                    ),
                      Codes =  list(c("25", "50", "75"),
                                    c("0.6", "0.7", "0.8"),
                                    c("Perfect Imp.",
                                      "10 %Overage TAC",
                                      "20 %Overage TAC")
                                    ),
                      Description =
                          list(paste("Growth Vector of",
                                     c("25th", "50th", "75th")),
                               paste("Beverton-Holt stock recruitment steepness (resilience) of",
                                     c(0.6, 0.7, 0.8)),
                               c("Perfect TAC Imp.",
                                 "10 %Overage TAC",
                                 "20% Overage TAC"))
                  ),
                  fstYr = fstYr,
                  MSElist = MSElist02
                )

######@> Last historical year...
obj$StateVar$TimeNow <- 2020 # last historical year in the OMs
nyears <- MSElist02[[1]]@nyears
proyears <- MSElist02[[1]]@proyears
obj$StateVar$Times <- c(rev(seq(fstYr-1, by=-1,length.out=nyears)),
                         seq(fstYr, by=1,length.out=proyears))

######@> Title...
obj$Text$Title <-
    "Western Atlantic Skipjack Tuna MSE - Communication and Demonstration"
obj$Text$Sub_title <-
    "A Summary of the Results to be Reported to the ICCAT Commission"

######@> Metadata...
obj$Misc$Author <- "1. Rodrigo Sant'Ana; 2. Bruno Leite Mourato"
obj$Misc$Contact <-""
obj$Misc$Date <- "February 20, 2024"
obj$Misc$Institution <-
    "1. Laboratório de Estudos Marinhos Aplicados, Escola Politécnica,
Universidade do Vale do Itajaí;
     \n2. Laboratório de Ciências da Pesca, Instituto do Mar, Universidade Federal de São Paulo."
obj$Misc$Fishery <- "Western Atlantic Skipjack Tuna"

######@> Intro Text...
obj$Text$Introduction[[1]] <- "The work on developing the Western Atlantic Skipjack MSE began in 2020 through a collaboration between Brazilian scientists and scientists who developed the openMSE (Open Source Software for Management Strategy Evaluation). This collaboration resulted in the presentation of document SCRS/2020/140 (Huynh et al., 2020) at the 2020 ICCAT Tropical Tunas Species Group meeting and included a first demonstration MSE framework with operating model (OM) conditioning for the Western Atlantic Skipjack. An updated MSE framework was developed in 2022 (SCRS/2022/097, Mourato et al., 2022) and presented at the 2022 Tropical Tuna MSE Technical Sub-group meeting. After the Atlantic skipjack stock assessment (2022), the W-SKJ MSE operating models were reconditioned using the Stock Synthesis model results. The analysis also included evaluating the relative performance of pre-selected management procedures across a set of performance metrics. In general, simulations presented during the 2022 Species Group Meeting shown good performance metrics across management procedures regarding the safety, status, yield, and stability of Western Atlantic skipjack tuna (SCRS/2022/180, Mourato et al., 2022b). Still in 2022, during the 27th Regular Meeting of the ICCAT Commission, the Commission adopted conceptual management objectives for W-SKJ (Res. 22-02), and the MSE work is now on track for ICCAT to adopt an MP in 2023, in accordance with the Commission’s MSE workplan. In this sense, the demonstration presenting in this tool were developed as result of the input from the defined conceptual management objectives provided by the ICCAT Commission and largely discussed in SCRS during the 2023 Intersessional Meeting of the Tropical Tunas Species Group (including MSE)."

obj$Text$Introduction[[2]] <-
    "This Slick data file includes a subset of the results that were presented to the ICCAT Second Intersessional meeting of Panel 1 (on Western Skipjack MSE)."

obj$Text$Introduction[[3]] <-
    "<strong>Note:</strong>These results were posted with permission from the International Commission for the Conservation of Atlantic Tunas for the purpose of demonstrating the features of Slick. The Western Atlantic Swordfish MSE process in still ongoing. The operating models, candidate management procedures, and performance metrics shown here as for demonstration purposes only and are subject to change as the MSE process contiunes. The results presented here do not necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area."


## Standardize Yield Metric
## All Performance Metrics in Slick must be scaled between 0 (worst) and
## 100 (best)
## The average yield PM in the SWO MSE must be re-scaled accordingly.
## Here they are calculated as a percent difference from the highest
## yield

## Deterministic
Yind <- grepl("AvC", obj$Perf$Det$Codes)
## Yind[c(2, 4, 6)] <- FALSE
obj$Perf$Det$Values[ , , Yind] <- obj$Perf$Det$Values[ , , Yind] /
    max(obj$Perf$Det$Values[ , , Yind]) * 100

## Stochastic
Yind <- grepl("AvC", obj$Perf$Stoch$Codes)
## Yind[c(2, 4, 6)] <- FALSE
obj$Perf$Stoch$Values[ , , , Yind] <-
    obj$Perf$Stoch$Values[ , , , Yind] /
    max(obj$Perf$Stoch$Values[ , , , Yind]) * 100

# obj$Perf$Det$Labels[Yind] <- c("Mean Short-Term Yield",
#                                "Mean Medium-Term Yield")
#
# obj$Perf$Stoch$Labels[Yind] <- c("Mean Short-Term Yield",
#                                  "Mean Medium-Term Yield")
obj$Perf$Det$Description[Yind] <-
    c("Mean Yield Relative to Highest Mean Yield (Years 11-30)",
      "Mean Yield Relative to Highest Mean Yield (Years 4-10)",
      "Mean Yield Relative to Highest Mean Yield (Years 1-3)")
obj$Perf$Stoch$Description[Yind] <-
    c("Mean Yield Relative to Highest Mean Yield (Years 11-30)",
      "Mean Yield Relative to Highest Mean Yield (Years 4-10)",
      "Mean Yield Relative to Highest Mean Yield (Years 1-3)")

######@> Changing colors...
## qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
## col_vector <- unlist(mapply(brewer.pal,
##                             qual_col_pals$maxcolors,
##                             rownames(qual_col_pals)))
## obj$Misc$Cols$MP <- sample(col_vector,  length(MPs))
col_vector <- mako(n = length(MPs), begin = 0.1, end = 0.9)

obj$Misc$Cols$MP <- col_vector

######@> Rename MPs...
nomesMPs <- c("CC_30kt", "CC_40kt", "Iratio", "Islope1", "GB_slope",
              "SCA01", "SP01", "SPSS01", "SP02", "SPSS02")
obj$MP$Codes <- nomesMPs
obj$MP$Labels <- nomesMPs


######@> Add Default OMs

obj$StateVar$Times


obj$OM$Defaults <- list()
obj$OM$Defaults[[1]] <- 1:3
obj$OM$Defaults[[2]] <- 1:3
obj$OM$Defaults[[3]] <- 1


######@> Exporting Slick File...
## dir.create("01_Slick_Files")
saveRDS(obj, "WSKJ.slick")

########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
