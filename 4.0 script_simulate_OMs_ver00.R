########################################################################
## Description: Simulating Operating Models based on the old OMs...
##
## Maintainer: Datenkraft - ICCAT/SCRS - TT MSE
## Author: Bruno Mourata & Rodrigo Sant'Ana
## Created: dom set 10 14:20:53 2023 (-0300)
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

########################################################################
######@> Loading R packages...

######@> Package list...
library(dplyr)
library(openMSE)
library(SWOMSE)
library(ggmse)

######@> ggplot theme...
extrafont::loadfonts(device = "postscript")
rgb01 <- "black"
rgb02 <- "black"
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
seta2 <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open",
                     ends = "both")
my_theme <- function(base_size = 18, base_family = "Helvetica") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_line(colour = rgb01),
              axis.line = element_line(colour = rgb01, linewidth = 0.2),
              axis.text = element_text(colour = rgb02, size = 14),
              axis.title = element_text(size = 18),
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_line(linetype = "solid",
                                        linewidth = 0.1,
                                        colour = "gray90"),
              plot.background = element_blank(),
              complete = TRUE)
}

########################################################################
######@> Setup MSE...

######@> Creating a folder to receive the hist objects...
if(!dir.exists("01_Hists"))
    dir.create("01_Hists")

######@> Creating a folder to receive the MSE objects...
if(!dir.exists("02_MSEs"))
    dir.create("02_MSEs")

######@> OM folder...
path01 <- "00_OMs/"

########################################################################
######@> Loading data...

######@> Operating models...
OMs <- readRDS(paste0(path01,
                      "SS_Operating_models_v01_fullgrid_13082022.rds"),
               refhook = NULL)

#####@> Setting names for OMs...
names(OMs) <- paste0("OM", sprintf("%03d", 1:27))

######@> Management procedures...
source("script_prepare_MPs_ver00.R")

#####@> Defining MPs...
MPs <- c("NFref", "curE", "CC_15kt", "CC_20kt",
         "CC_25kt", "CC_30kt", "CC_35kt", "CC_40kt",
         "GB_slope", "Iratio", "Islope1",
         "SCA_100_40_SBMSY", "SP_100_40_SBMSY", "SPSS_100_40_SBMSY",
         "SP_03", "SP_04", "SP_05", "SP_06")

########################################################################
######@> Simulating Historical Data...

######@> List of objects...
OM_Objects <- names(OMs)

######@> Looping to create historical data...
for(i in seq_along(OM_Objects)) {
    OM <- OMs[[i]]
    OM@interval <- 3
    Hist <- Simulate(OM, parallel = TRUE, silent = FALSE)
    nm <- paste0(OM_Objects[i], ".hist")
    saveRDS(Hist, file.path("01_Hists", nm))
}

########################################################################
######@> Simulating Projections Data...

######@> Loading history objects...
Hists <- sapply(dir("01_Hists", full.names = TRUE), readRDS)

#####@> Setting names for Hists...
names(Hists) <- paste0("OM", sprintf("%03d", 1:27))

######@> Looping to run the MSE for all histories data...
for(i in seq_along(Hists)) {
    Hist <- Hists[[i]]
    MSE <- Project(Hist, MPs = MPs, parallel = TRUE)
    MSE <- Sub(MSE, years = 1:33)
    nm <- paste0("MSE", sprintf("%03d", i), "_03.mse")
    saveRDS(MSE, file.path("02_MSEs/", nm))
}

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
