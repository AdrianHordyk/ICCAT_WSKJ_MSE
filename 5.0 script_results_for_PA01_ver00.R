########################################################################
## Description: Compiling the results to be presented in February
## meeting 2024...
##
## Maintainer: Datenkraft - SCRS/ICCAT (TT Species Group)
## Author: Bruno Mourato & Rodrigo Sant'Ana
## Created: seg fev  5 14:08:22 2024 (-0300)
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
library(openMSE)
library(SWOMSE)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggmse)
library(patchwork)
library(viridis)
library(data.table)
library(ggrepel)
library(tidyr)
library(ggridges)
library(extrafont)

########################################################################
######@> Setup MSE...

######@> Folder destination...
if(dir.exists("03_Results")) {
    print("Folder exists!!")
    folder <- "03_Results/"
} else {
    dir.create("03_Results")
    print("Folder will be created!!")
    folder <- "03_Results/"
}

######@> Path to MSEs...
path00 <- paste0("02_MSEs/")

######@> Custom R environment...
options(scipen = 16, digits = 2)

######@> Theme for this project...
font_import()
loadfonts(device = "postscript")
loadfonts(device = "pdf")

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
######@> Loading everything...

######@> Management Procedures...
source("script_prepare_MPs_ver00.R")

#####@> Defining All MPs...
MPs <- c("NFref", "curE", "CC_15kt", "CC_20kt",
         "CC_25kt", "CC_30kt", "CC_35kt", "CC_40kt",
         "GB_slope", "Iratio", "Islope1",
         "SCA_100_40_SBMSY", "SP_100_40_SBMSY", "SPSS_100_40_SBMSY",
         "SP_01", "SP_02", "SP_03", "SP_04", "SP_05", "SP_06")

######@> Defining selected MPs...
selMPs <- c("CC_30kt", "CC_40kt",
            "Iratio", "Islope1", "GB_slope",
            "SCA_100_40_SBMSY",
            "SPSS_100_40_SBMSY",
            "SP_100_40_SBMSY",
            "SP_03", "SP_04")

######@> Performance Metrics...
source("script_prepare_PMs_ver00.R")

#####@> List of PMs...
PMs <- avail('PM')[c(1:23)]

######@> MMSEs model results - No error implementation...
nomes <- paste0("MSE", sprintf("%03d", 1:9))
MSEs <- lapply(1:9, function(i) readRDS(paste0(path00, nomes[i],
                                               "_03.mse")))
names(MSEs) <- paste0("MSE", sprintf("%03d", 1:9))

######@> MMSEs model results - 10% error implementation...
nomes02 <- paste0("MSE", sprintf("%03d", 10:18))
MSEs02 <- lapply(1:9, function(i) readRDS(paste0(path00, nomes02[i],
                                                 "_03.mse")))
names(MSEs02) <- paste0("MSE", sprintf("%03d", 10:18))

######@> MMSEs model results - 20% error implementation...
nomes03 <- paste0("MSE", sprintf("%03d", 19:27))
MSEs03 <- lapply(1:9, function(i) readRDS(paste0(path00, nomes03[i],
                                                 "_03.mse")))
names(MSEs03) <- paste0("MSE", sprintf("%03d", 19:27))

######@>----------------------------------------------------------------
######@> No error implementation - OMs 1-9...

######@> Extracting PMs from all MMSE's objects...

#####@> Listing PMs functions...
PMlist <- list()
for(i in seq_along(PMs)) {
    PMlist[[i]] <- try(get(PMs[i]), silent = TRUE)
}

######@> Extracting values...
PMvalues <- list()
PMvalues2 <- list()
for(i in seq_along(MSEs)) {
    for(j in seq_along(PMs)) {
        pm <- PMlist[[j]](MSEs[[i]])
        om <- paste0("MSE", sprintf("%03d", i))
        mps <- pm@MPs
        val <- data.frame(pm@Prob)
        names(val) <- mps
        val <- val %>%
            mutate(sim = 1:100) %>%
            pivot_longer(names_to = "MP", values_to = "Values", 1:ncol(val))
        nom <- pm@Name
        cap <- pm@Caption
        pm <- PMs[j]
        PMvalues[[j]] <- val %>%
            mutate(OM = om, Name = nom, Caption = cap, PM = pm) %>%
            select(OM, Name, Caption, sim, MP, PM, Values) %>%
            as.data.frame()
    }
    out <- do.call(rbind.data.frame, PMvalues)
    PMvalues2[[i]] <- out
}
PM_output <- do.call(rbind.data.frame, PMvalues2)

#####@> Looking for NAs...
tab01_NA <- PM_output %>%
    filter(is.na(Values)) %>%
    group_by(OM, MP, PM) %>%
    summarise(N = n()) %>%
    as.data.frame()

#####@> Looking to the PMs - Estimating the average for each OMs...
tab01_exp <- PM_output %>%
    filter(MP %in% selMPs) %>%
    group_by(OM, MP, PM) %>%
    summarise(q50 = mean(Values, na.rm = TRUE)) %>%
    pivot_wider(names_from = "MP", values_from = "q50") %>%
    ## select(OM:SPSS_100_40_SBMSY, SP_100_40_SBMSY, SP_01:SP_06) %>%
    as.data.frame()

###@> Saving the output table...
tab01_exp_ref <- PM_output %>%
    filter(MP %in% selMPs)

####@> Exporting table...
write.table(tab01_exp, "03_Results/Table_PM-MP_OMs_001-009_ver00.csv",
            row.names = FALSE, sep = ",", dec = ".")

#####@> Looking to the PMs - Estimating the average between all OMs...
tab01 <- PM_output %>%
    group_by(MP, PM, Name, Caption) %>%
    summarise(q50 = mean(Values, na.rm = TRUE)) %>%
    mutate(MP2 = ifelse(MP == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MP == "SP_100_40_SBMSY", "SP01",
                 ifelse(MP == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MP == "SP_03", "SP02",
                 ifelse(MP == "SP_04", "SPSS02", MP)))))) %>%
    as.data.frame()

####@> Figure PGK ~ POF...
tmp <- tab01 %>%
    filter(PM %in% c("POF", "PGK_short")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status")

p00 <- ggplot(data = tmp, aes(x = PGK_short, y = POF, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 1-3",
         y = "Prob. Overfishing over years 1-30") +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    facet_wrap(~class) +
    my_theme() +
    theme(legend.position = "none")
p00

tmp <- tab01 %>%
    filter(PM %in% c("POF", "PGK_med")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status")

p01 <- ggplot(data = tmp, aes(x = PGK_med, y = POF, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 4-10",
         y = "Prob. Overfishing over years 1-30") +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    facet_wrap(~class) +
    my_theme() +
    theme(legend.position = "none")
p01

tmp <- tab01 %>%
    filter(PM %in% c("POF", "PGK_long")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status")

p02 <- ggplot(data = tmp, aes(x = PGK_long, y = POF, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 11-30",
         y = "Prob. Overfishing over years 1-30") +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    facet_wrap(~class) +
    my_theme() +
    theme(legend.position = "none")
p02

tmp <- tab01 %>%
    filter(PM %in% c("POF", "PGK")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status")

p03 <- ggplot(data = tmp, aes(x = PGK, y = POF, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 1-30",
         y = "Prob. Overfishing over years 1-30") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    facet_wrap(~class) +
    my_theme() +
    theme(legend.position = "none")
p03

ggsave("03_Results/Tradeplot_PGK_short-POF_ver00.png", plot = p00,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_PGK_med-POF_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_PGK_long-POF_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_PGK-POF_ver00.png", plot = p03,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

plot00 <- p00/p01/p02


####@> Figure LRP ~ VarC...
tmp <- tab01 %>%
    filter(PM %in% c("VarCmedium", "LRP_med")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class01 = "Safety", class02 = "Stability")

## ##@> Correcting SP_05...
## tmp$VarCmedium[tmp$VarCmedium > 1] <- 1

p00 <- ggplot(data = tmp, aes(x = VarCmedium, y = LRP_med, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Mean Variation in TAC (%) over years 4-10",
         y = "Prob. SB < 0.4 SBMSY over years 4-10") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = 0.1, linetype = "dashed", colour = "gray") +
    geom_vline(xintercept = 0.2, linetype = "dashed", colour = "gray") +
    facet_grid(class01~class02) +
    my_theme() +
    theme(legend.position = "none")
p00

tmp <- tab01 %>%
    filter(PM %in% c("VarClong", "LRP_long")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class01 = "Safety", class02 = "Stability")

p01 <- ggplot(data = tmp, aes(x = VarClong, y = LRP_long, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Mean Variation in TAC (%) over years 11-30",
         y = "Prob. SB < 0.4 SBMSY over years 11-30") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = 0.1, linetype = "dashed", colour = "gray") +
    geom_vline(xintercept = 0.2, linetype = "dashed", colour = "gray") +
    facet_grid(class01~class02) +
    my_theme() +
    theme(legend.position = "none")
p01

tmp <- tab01 %>%
    filter(PM %in% c("VarC", "LRP")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class01 = "Safety", class02 = "Stability")

p02 <- ggplot(data = tmp, aes(x = VarC, y = LRP, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Mean Variation in TAC (%) over years 1-30",
         y = "Prob. SB < 0.4 SBMSY over years 1-30") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = 0.1, linetype = "dashed", colour = "gray") +
    geom_vline(xintercept = 0.2, linetype = "dashed", colour = "gray") +
    facet_grid(class01~class02) +
    my_theme() +
    theme(legend.position = "none")
p02

ggsave("03_Results/Tradeplot_VarCmed-LRP_med_ver00.png", plot = p00,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_VarClong-LRP_long_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_VarC-LRP_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

plot01 <- p00/p01/p02
plot02 <- plot00 | plot01

ggsave("03_Results/Tradeplot_COMB_PA01_ver00.png", plot = plot02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 45, h = 45)

####@> Figure PGK ~ AvC...
tmp <- tab01 %>%
    filter(PM %in% c("AvC_short", "PGK_short")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status", class2 = "Yield")

p00 <- ggplot(data = tmp, aes(x = PGK_short, y = AvC_short, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 1-3",
         y = "Median Catch (t) over years 1-3") +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    scale_y_continuous(limits = c(0, 60000)) +
    scale_x_continuous(limits = c(0, 1)) +
    facet_grid(class2~class) +
    my_theme() +
    theme(legend.position = "none")
p00

tmp <- tab01 %>%
    filter(PM %in% c("AvC_med", "PGK_med")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status", class2 = "Yield")

p01 <- ggplot(data = tmp, aes(x = PGK_med, y = AvC_med, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 4-10",
         y = "Median Catch (t) over years 4-10") +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    scale_y_continuous(limits = c(0, 40000)) +
    scale_x_continuous(limits = c(0, 1)) +
    facet_grid(class2~class) +
    my_theme() +
    theme(legend.position = "none")
p01

tmp <- tab01 %>%
    filter(PM %in% c("AvC_long", "PGK_long")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    mutate(class = "Status", class2 = "Yield")

p02 <- ggplot(data = tmp, aes(x = PGK_long, y = AvC_long, fill = MP2)) +
    geom_point(size = 4, colour = "black", pch = 21) +
    geom_label_repel(aes(label = MP2), force = 40, family = "Helvetica",
                     size = 5) +
    labs(x = "Prob. Kobe green quadrante over years 11-30",
         y = "Median Catch (t) over years 11-30") +
    geom_vline(xintercept = 0.7, linetype = "dashed", colour = "gray") +
    scale_y_continuous(limits = c(0, 40000)) +
    scale_x_continuous(limits = c(0, 1)) +
    facet_grid(class2~class) +
    my_theme() +
    theme(legend.position = "none")
p02

ggsave("03_Results/Tradeplot_PGK_short-AvC_short_ver00.png", plot = p00,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_PGK_med-AvC_med_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Tradeplot_PGK_long-AvC_long_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

plot03 <- p00/p01/p02
plot04 <- plot03 | plot01

ggsave("03_Results/Tradeplot_COMB_PA01_ver00.png", plot = plot04,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 45, h = 45)

######@> Consolidating general probabilities for a heatmap table...
ProbTab <- tab01 %>%
    filter(PM %in% c("LRP", "LRP_long", "LRP_med", "LRP_short", "PGK",
                     "PGK_long", "PGK_med", "PGK_short", "PNOF", "POF",
                     "VarC","VarClong", "VarCmedium", "nLRP", "nLRP_long",
                     "nLRP_med", "nLRP_short", "rAvC_long", "rAvC_med",
                     "rAvC_short", "AvC_short", "AvC_med", "AvC_long")) %>%
    filter(MP %in% selMPs) %>%
    select(MP, MP2, PM, q50) %>%
    pivot_wider(names_from = "PM", values_from = "q50") %>%
    as.data.frame()

## ##@> Correcting SP_05...
## ProbTab$VarCmedium[ProbTab$VarCmedium > 1] <- 1

#####@> Exporting ProbTab - Combined all 9 MOMs...
write.table(ProbTab, file = "03_Results/Table_PMs_FULL_ver00.csv",
            row.names = FALSE, sep = ",", dec = ".")

######@> Heatmaps for ProbTab...

#####@> Figure for Yield...
tmp <- ProbTab %>%
    select(MP:AvC_short) %>%
    pivot_longer(names_to = "PM", values_to = "value", 3:5) %>%
    mutate(class = "Yield",
           breaks = cut(value,
                        breaks = quantile(value, seq(0, 1, 0.2)),
                        include.lowest = TRUE, right = FALSE),
           PM = factor(PM, levels = c("AvC_short",
                                      "AvC_med",
                                      "AvC_long")),
           MP = factor(MP, levels = selMPs),
           class2 = ifelse(value >= 20000, "Up", "Down")) ## Up average
## 4 years
p00 <- ggplot(data = tmp, aes(x = PM, y = MP2, fill = class2)) +
    geom_tile(colour = "black", alpha = 0) +
    geom_text(aes(label = round(value, 0)), size = 5,
              fontface = "bold") +
    scale_fill_brewer(palette = "Greys", direction = 1) +
    scale_x_discrete(labels = c(
                         "AvC_short" = bquote(AvC_short["[1-3 years]"]),
                         "AvC_med" = bquote(AvC_med["[4-10 years]"]),
                         "AvC_long" = bquote(AvC_long["[11-30 years]"]))) +
    facet_wrap(~class) +
    labs(x = "Median Catch (t) over period",
         y = "Candidate Management Procedures") +
    my_theme() +
    theme(legend.position = "none")
p00

#####@> Figure for Status...
tmp <- ProbTab %>%
    select(MP:MP2, PGK:PGK_short) %>%
    pivot_longer(names_to = "PM", values_to = "value", 3:6) %>%
    mutate(class = "Status",
           breaks = cut(value,
                        breaks = quantile(value, seq(0, 1, 0.2)),
                        include.lowest = TRUE, right = FALSE),
           PM = factor(PM, levels = c("PGK_short",
                                      "PGK_med",
                                      "PGK_long",
                                      "PGK")),
           MP = factor(MP, levels = selMPs),
           class2 = ifelse(value >= 0.7, "Up", "Down"))
p01 <- ggplot(data = tmp, aes(x = PM, y = MP2, fill = class2)) +
    geom_tile(colour = "black", alpha = 0.8) +
    geom_text(aes(label = round(value, 3)), size = 5,
              fontface = "bold") +
    scale_fill_brewer(palette = "Greys", direction = 1) +
    scale_x_discrete(labels =
                         c("PGK_short" =
                               bquote(PGK_short["[1-3 years]"]),
                           "PGK_med" = bquote(PGK_med["[4-10 years]"]),
                           "PGK_long" =
                               bquote(PGK_long["[11-30 years]"]),
                           "PGK" = bquote(PGK["[1-30 years]"]))) +
    facet_wrap(~class) +
    labs(x = "Probability of being in the Kobe green quadrant",
         y = "Candidate Management Procedures") +
    my_theme() +
    theme(legend.position = "none")
p01

tmp <- ProbTab %>%
    select(MP, MP2, PNOF:POF) %>%
    pivot_longer(names_to = "PM", values_to = "value", 3:4) %>%
    mutate(class = "Status",
           breaks = cut(value,
                        breaks = quantile(value, seq(0, 1, 0.2)),
                        include.lowest = TRUE, right = FALSE),
           PM = factor(PM, levels = c("PNOF",
                                      "POF")),
           MP = factor(MP, levels = selMPs),
           class2 = ifelse(value >= 0.7, "Up", "Down"))
p01b <- ggplot(data = tmp, aes(x = PM, y = MP2)) +
    geom_tile(colour = "black", fill = "white", alpha = 0.1) +
    geom_text(aes(label = round(value, 3)), size = 5,
              fontface = "bold") +
    ## scale_fill_brewer(palette = "Greys", direction = 1) +
    facet_wrap(~class) +
    labs(x = "Performance Metrics",
         y = "Candidate Management Procedures") +
    my_theme() +
    theme(legend.position = "none")
p01b

#####@> Figure for Safety...
tmp <- ProbTab %>%
    select(MP, MP2, LRP:LRP_short) %>%
    pivot_longer(names_to = "PM", values_to = "value", 3:6) %>%
    mutate(class = "Safety",
           breaks = cut(value,
                        breaks = quantile(value, seq(0, 1, 0.2)),
                        include.lowest = TRUE, right = FALSE),
           PM = factor(PM, levels = c("LRP_short",
                                      "LRP_med",
                                      "LRP_long",
                                      "LRP")),
           MP = factor(MP, levels = selMPs),
           class2 = ifelse(value <= 0.1, "Up", "Down"))
p02 <- ggplot(data = tmp, aes(x = PM, y = MP2, fill = class2)) +
    geom_tile(colour = "black", alpha = 0.8) +
    geom_text(aes(label = round(value, 3)), size = 5,
              fontface = "bold") +
    scale_fill_brewer(palette = "Greys", direction = 1) +
    scale_x_discrete(labels = c("LRP_short" =
                                    bquote(LRP_short["[1-3 years]"]),
                                "LRP_med" = bquote(LRP_med["[4-10 years]"]),
                                "LRP_long" =
                                    bquote(LRP_long["[11-30 years]"]),
                                "LRP" = bquote(LRP["[1-30 years]"]))) +
    facet_wrap(~class) +
    labs(x = "Probability of breaching the limit reference point",
         y = "Candidate Management Procedures") +
    my_theme() +
    theme(legend.position = "none")
p02

#####@> Figure for Stability...
tmp <- ProbTab %>%
    select(MP, MP2, VarC:VarCmedium) %>%
    pivot_longer(names_to = "PM", values_to = "value", 3:5) %>%
    mutate(class = "Stability",
           breaks = cut(value,
                        breaks = quantile(value, seq(0, 1, 0.2)),
                        include.lowest = TRUE, right = FALSE),
           PM = factor(PM, levels = c("VarCmedium",
                                      "VarClong",
                                      "VarC")),
           MP = factor(MP, levels = selMPs),
           class2 = ifelse(value <= 0.2, "Up", "Down"))
p03 <- ggplot(data = tmp, aes(x = PM, y = MP2, fill = class2)) +
    geom_tile(colour = "black", alpha = 0.8) +
    geom_text(aes(label = round(value, 3)), size = 5,
              fontface = "bold") +
    scale_fill_brewer(palette = "Greys", direction = 1) +
    scale_x_discrete(labels =
                         c("VarCmedium" = bquote(VarC_med["[4-10 years]"]),
                           "VarClong" =
                               bquote(VarC_long["[11-30 years]"]),
                           "VarC" = bquote(VarC["[1-30 years]"]))) +
    facet_wrap(~class) +
    labs(x = "Mean variation in TAC (%) between management cycles",
         y = "Candidate Management Procedures") +
    my_theme() +
    theme(legend.position = "none")
p03

ggsave("03_Results/Table_PM-MP_Yield_ver00.png", plot = p00,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Table_PM-MP_Status_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Table_PM-MP_Status_B_ver00.png", plot = p01b,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Table_PM-MP_Safety_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

ggsave("03_Results/Table_PM-MP_Stability_ver00.png", plot = p03,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 25, h = 25)

######@> Extracting References Points from MSE objects...

#####@> Extracting SB_SBMSY values...
tmp01 <- list()
for(i in seq_along(MSEs)) {
    om <- names(MSEs)[i]
    output <- MSEs[[i]]@SB_SBMSY %>%
        structure(
            dimnames = list(
                Sim = 1:MSEs[[i]]@nsim,
                MPs = MSEs[[i]]@MPs,
                Year = 2020 + 1:MSEs[[i]]@proyears
            )
        ) %>%
        reshape2::melt() %>%
        mutate(OM = om)
    tmp01[[i]] <- output
}
SB_SBMSY_output <- do.call(rbind.data.frame, tmp01)

####@> Looking to the RP - Estimating the average between all MOMs...
tab02 <- SB_SBMSY_output %>%
    mutate(MPs = as.character(MPs)) %>%
    filter(MPs %in% selMPs) %>%
    mutate(MP2 = ifelse(MPs == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MPs == "SP_100_40_SBMSY", "SP01",
                 ifelse(MPs == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MPs == "SP_03", "SP02",
                 ifelse(MPs == "SP_04", "SPSS02", MPs)))))) %>%
    group_by(MPs, MP2, Year) %>%
    summarise(SB_SBMSY = mean(value, na.rm = TRUE),
              SB_SBMSY_Lwr = quantile(value, probs = 0.025,
                                      na.rm = TRUE),
              SB_SBMSY_Upr = quantile(value, probs = 0.975,
                                      na.rm = TRUE),
              SB_SBMSY_Lwr2 = quantile(value, probs = 0.1,
                                      na.rm = TRUE),
              SB_SBMSY_Upr2 = quantile(value, probs = 0.9,
                                      na.rm = TRUE)) %>%
    as.data.frame()

###@> Figure Reference Points...
p01 <- ggplot(data =
                  filter(tab02,
                         MPs %in% c("CC_30kt", "CC_40kt"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = SB_SBMSY_Lwr,
                    ymax = SB_SBMSY_Upr),
                alpha = 0.5, fill = "#95B634") +
    geom_ribbon(aes(x = Year,
                    ymin = SB_SBMSY_Lwr2,
                    ymax = SB_SBMSY_Upr2),
                alpha = 0.2, fill = "#006750") +
    geom_line(aes(x = Year, y = SB_SBMSY), linewidth = 1,
              colour = "#006750") +
    geom_hline(yintercept = 1, linetype = "solid", colour = "white",
               linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray10",
               linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 4, 1),
                       limits = c(0, 4)) +
    scale_x_continuous(breaks = seq(2025, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2025, 2052)) +
    facet_grid(MPs ~ ., scales = "free") +
    labs(x = "Year", y = expression(B/B[MSY])) +
    my_theme() +
    theme(strip.placement = "outside")
p01

p02 <- ggplot(data =
                  filter(tab02,
                         MPs %in% c("Iratio", "Islope1", "GB_slope"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = SB_SBMSY_Lwr,
                    ymax = SB_SBMSY_Upr),
                alpha = 0.5, fill = "#86BEDA") +
    geom_ribbon(aes(x = Year,
                    ymin = SB_SBMSY_Lwr2,
                    ymax = SB_SBMSY_Upr2),
                alpha = 0.2, fill = "#005690") +
    geom_line(aes(x = Year, y = SB_SBMSY), linewidth = 1,
              colour = "#005690") +
    geom_hline(yintercept = 1, linetype = "solid", colour = "white",
               linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray10",
               linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5, 1),
                       limits = c(0, 6)) +
    scale_x_continuous(breaks = seq(2025, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2025, 2052)) +
    facet_grid(MPs ~ ., scales = "free") +
    labs(x = "Year", y = expression(B/B[MSY])) +
    my_theme() +
    theme(strip.placement = "outside")
p02

p03 <- ggplot(data =
                  filter(tab02,
                         MPs %in% c("SCA_100_40_SBMSY",
                                    "SP_100_40_SBMSY",
                                    "SPSS_100_40_SBMSY",
                                    "SP_03", "SP_04",
                                    "SP_05", "SP_06"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = SB_SBMSY_Lwr,
                    ymax = SB_SBMSY_Upr),
                alpha = 0.5, fill = "#FA8072") +
    geom_ribbon(aes(x = Year,
                    ymin = SB_SBMSY_Lwr2,
                    ymax = SB_SBMSY_Upr2),
                alpha = 0.4, fill = "#FA8072") +
    geom_line(aes(x = Year, y = SB_SBMSY), linewidth = 1,
              colour = "#FF4e4e") +
    geom_hline(yintercept = 1, linetype = "solid", colour = "white",
               linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray10",
               linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, 1),
                       limits = c(0, 4)) +
    scale_x_continuous(breaks = seq(2025, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2025, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = expression(B/B[MSY])) +
    my_theme() +
    theme(strip.placement = "outside")
p03

ggsave("03_Results/Trajectory_SB_SBMSY_CC_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_SB_SBMSY_Index_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_SB_SBMSY_Model_ver00.png", plot = p03,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

#####@> Extracting F_FMSY values...
tmp02 <- list()
for(i in seq_along(MSEs)) {
    om <- names(MSEs)[i]
    output <- MSEs[[i]]@F_FMSY %>%
        structure(
            dimnames = list(
                Sim = 1:MSEs[[i]]@nsim,
                MPs = MSEs[[i]]@MPs,
                Year = 2020 + 1:MSEs[[i]]@proyears
            )
        ) %>%
        reshape2::melt() %>%
        mutate(OM = om) %>%
        as.data.frame()
    tmp02[[i]] <- output
}
F_FMSY_output <- do.call(rbind.data.frame, tmp02)

####@> Looking to the RP - Estimating the average between all MOMs...
tab03 <- F_FMSY_output %>%
    mutate(MPs = as.character(MPs)) %>%
    filter(MPs %in% selMPs) %>%
    mutate(MP2 = ifelse(MPs == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MPs == "SP_100_40_SBMSY", "SP01",
                 ifelse(MPs == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MPs == "SP_03", "SP02",
                 ifelse(MPs == "SP_04", "SPSS02", MPs)))))) %>%
    group_by(MPs, MP2, Year) %>%
    summarise(F_FMSY = mean(value, na.rm = TRUE),
              F_FMSY_Lwr = quantile(value, probs = 0.025,
                                    na.rm = TRUE),
              F_FMSY_Upr = quantile(value, probs = 0.975,
                                    na.rm = TRUE),
              F_FMSY_Lwr2 = quantile(value, probs = 0.1,
                                       na.rm = TRUE),
              F_FMSY_Upr2 = quantile(value, probs = 0.9,
                                       na.rm = TRUE)) %>%
    as.data.frame()

###@> Figure Reference Points...
p01 <- ggplot(data =
                  filter(tab03,
                         MPs %in% c("CC_30kt", "CC_40kt"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = F_FMSY_Lwr,
                    ymax = F_FMSY_Upr),
                alpha = 0.5, fill = "#95B634") +
    geom_ribbon(aes(x = Year,
                    ymin = F_FMSY_Lwr2,
                    ymax = F_FMSY_Upr2),
                alpha = 0.2, fill = "#006750") +
    geom_line(aes(x = Year, y = F_FMSY), linewidth = 1,
              colour = "#006750") +
    geom_hline(yintercept = 1, linetype = "solid", colour = "white",
               linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray10",
               linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 2),
                       limits = c(0, 8)) +
    scale_x_continuous(breaks = seq(2025, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2025, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = expression(F/F[MSY])) +
    my_theme() +
    theme(strip.placement = "outside")
p01

p02 <- ggplot(data =
                  filter(tab03,
                         MPs %in% c("Iratio", "Islope1", "GB_slope"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = F_FMSY_Lwr,
                    ymax = F_FMSY_Upr),
                alpha = 0.5, fill = "#86BEDA") +
    geom_ribbon(aes(x = Year,
                    ymin = F_FMSY_Lwr2,
                    ymax = F_FMSY_Upr2),
                alpha = 0.2, fill = "#005690") +
    geom_line(aes(x = Year, y = F_FMSY), linewidth = 1,
              colour = "#005690") +
    geom_hline(yintercept = 1, linetype = "solid", colour = "white",
               linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray10",
               linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, 1),
                       limits = c(0, 4)) +
    scale_x_continuous(breaks = seq(2025, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2025, 2052), ylim = c(0, 3)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = expression(F/F[MSY])) +
    my_theme() +
    theme(strip.placement = "outside")
p02

p03 <- ggplot(data =
                  filter(tab03,
                         MPs %in% c("SCA_100_40_SBMSY",
                                    "SP_100_40_SBMSY",
                                    "SPSS_100_40_SBMSY",
                                    "SP_03", "SP_04"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = F_FMSY_Lwr,
                    ymax = F_FMSY_Upr),
                alpha = 0.5, fill = "#FA8072") +
    geom_ribbon(aes(x = Year,
                    ymin = F_FMSY_Lwr2,
                    ymax = F_FMSY_Upr2),
                alpha = 0.4, fill = "#FA8072") +
    geom_line(aes(x = Year, y = F_FMSY), linewidth = 1,
              colour = "#FF4e4e") +
    geom_hline(yintercept = 1, linetype = "solid", colour = "white",
               linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "gray10",
               linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 5, 1),
                       limits = c(0, 5)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = expression(F/F[MSY])) +
    my_theme() +
    theme(strip.placement = "outside")
p03

ggsave("03_Results/Trajectory_F_FMSY_CC_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_F_FMSY_Index_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_F_FMSY_Model_ver00.png", plot = p03,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

#####@> Extracting Catches values...
tmp03 <- list()
for(i in seq_along(MSEs)) {
    om <- names(MSEs)[i]
    output <- MSEs[[i]]@Catch %>%
        structure(
            dimnames = list(
                Sim = 1:MSEs[[i]]@nsim,
                MPs = MSEs[[i]]@MPs,
                Year = 2020 + 1:MSEs[[i]]@proyears
            )
        ) %>%
        reshape2::melt() %>%
        mutate(MOM = om) %>%
        as.data.frame()
    tmp03[[i]] <- output
}
Catch_output <- do.call(rbind.data.frame, tmp03)

####@> Looking to the RP - Estimating the average between all MOMs...
tab04 <- Catch_output %>%
    mutate(MPs = as.character(MPs)) %>%
    filter(MPs %in% selMPs) %>%
    mutate(MP2 = ifelse(MPs == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MPs == "SP_100_40_SBMSY", "SP01",
                 ifelse(MPs == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MPs == "SP_03", "SP02",
                 ifelse(MPs == "SP_04", "SPSS02", MPs)))))) %>%
    group_by(MPs, MP2, Year) %>%
    summarise(Catch = mean(value, na.rm = TRUE),
              Catch_Lwr = quantile(value, probs = 0.025,
                                   na.rm = TRUE),
              Catch_Upr = quantile(value, probs = 0.975,
                                   na.rm = TRUE),
              Catch_Lwr2 = quantile(value, probs = 0.1,
                                   na.rm = TRUE),
              Catch_Upr2 = quantile(value, probs = 0.9,
                                   na.rm = TRUE)) %>%
    as.data.frame()

###@> Figure Reference Points...
p01 <- ggplot(data =
                  filter(tab04,
                         MPs %in% c("CC_30kt", "CC_40kt"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = Catch_Lwr,
                    ymax = Catch_Upr),
                alpha = 0.5, fill = "#95B634") +
    geom_ribbon(aes(x = Year,
                    ymin = Catch_Lwr2,
                    ymax = Catch_Upr2),
                alpha = 0.2, fill = "#006750") +
    geom_line(aes(x = Year, y = Catch), linewidth = 1,
              colour = "#006750") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 45000, 10000),
                       limits = c(0, 45000)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = "Catch (t)") +
    my_theme() +
    theme(strip.placement = "outside")
p01

p02 <- ggplot(data =
                  filter(tab04,
                         MPs %in% c("Iratio", "Islope1", "GB_slope"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = Catch_Lwr,
                    ymax = Catch_Upr),
                alpha = 0.5, fill = "#86BEDA") +
    geom_ribbon(aes(x = Year,
                    ymin = Catch_Lwr2,
                    ymax = Catch_Upr2),
                alpha = 0.2, fill = "#005690") +
    geom_line(aes(x = Year, y = Catch), linewidth = 1,
              colour = "#005690") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 65000, 10000),
                       limits = c(0, 65000)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = "Catch (t)") +
    my_theme() +
    theme(strip.placement = "outside")
p02

p03 <- ggplot(data =
                  filter(tab04,
                         MPs %in% c("SCA_100_40_SBMSY",
                                    "SP_100_40_SBMSY",
                                    "SPSS_100_40_SBMSY",
                                    "SP_03", "SP_04"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = Catch_Lwr,
                    ymax = Catch_Upr),
                alpha = 0.5, fill = "#FA8072") +
    geom_ribbon(aes(x = Year,
                    ymin = Catch_Lwr2,
                    ymax = Catch_Upr2),
                alpha = 0.4, fill = "#FA8072") +
    geom_line(aes(x = Year, y = Catch), linewidth = 1,
              colour = "#FF4e4e") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 160000, 40000),
                       limits = c(0, 120000)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = "Catch (t)") +
    my_theme() +
    theme(strip.placement = "outside")
p03

ggsave("03_Results/Trajectory_Catch_CC_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_Catch_Index_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_Catch_Model_ver00.png", plot = p03,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

#####@> Extracting TAC values...
tmp04 <- list()
for(i in seq_along(MSEs)) {
    om <- names(MSEs)[i]
    output <- MSEs[[i]]@TAC %>%
        structure(
            dimnames = list(
                Sim = 1:MSEs[[i]]@nsim,
                MPs = MSEs[[i]]@MPs,
                Year = 2020 + 1:MSEs[[i]]@proyears
            )
        ) %>%
        reshape2::melt() %>%
        mutate(MOM = om) %>%
        as.data.frame()
    tmp04[[i]] <- output
}
TAC_output <- do.call(rbind.data.frame, tmp04)

#####@> Looking for NAs...
tab05_NA <- TAC_output %>%
    filter(is.na(value)) %>%
    group_by(Sim, MOM, MPs) %>%
    summarise(N = n()) %>%
    as.data.frame()

####@> Looking to the RP - Estimating the average between all MOMs...
tab05 <- TAC_output %>%
    mutate(MPs = as.character(MPs)) %>%
    filter(MPs %in% selMPs) %>%
    mutate(MP2 = ifelse(MPs == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MPs == "SP_100_40_SBMSY", "SP01",
                 ifelse(MPs == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MPs == "SP_03", "SP02",
                 ifelse(MPs == "SP_04", "SPSS02", MPs)))))) %>%
    group_by(MPs, MP2, Year) %>%
    summarise(TAC = median(value, na.rm = TRUE),
              TAC_Lwr = quantile(value, probs = 0.025,
                                 na.rm = TRUE),
              TAC_Upr = quantile(value, probs = 0.975,
                                 na.rm = TRUE),
              TAC_Lwr2 = quantile(value, probs = 0.1,
                                 na.rm = TRUE),
              TAC_Upr2 = quantile(value, probs = 0.9,
                                 na.rm = TRUE)) %>%
    as.data.frame()

###@> Figure Reference Points...
p01 <- ggplot(data =
                  filter(tab05,
                         MPs %in% c("CC_30kt", "CC_40kt"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = TAC_Lwr,
                    ymax = TAC_Upr),
                alpha = 0.5, fill = "#95B634") +
    geom_ribbon(aes(x = Year,
                    ymin = TAC_Lwr2,
                    ymax = TAC_Upr2),
                alpha = 0.2, fill = "#006750") +
    geom_line(aes(x = Year, y = TAC), linewidth = 1,
              colour = "#006750") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 45000, 10000),
                       limits = c(0, 45000)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MPs ~ ., scales = "free") +
    labs(x = "Year", y = "TAC (t)") +
    my_theme() +
    theme(strip.placement = "outside")
p01

p02 <- ggplot(data =
                  filter(tab05,
                         MPs %in% c("Iratio", "Islope1", "GB_slope"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = TAC_Lwr,
                    ymax = TAC_Upr),
                alpha = 0.5, fill = "#86BEDA") +
    geom_ribbon(aes(x = Year,
                    ymin = TAC_Lwr2,
                    ymax = TAC_Upr2),
                alpha = 0.2, fill = "#005690") +
    geom_line(aes(x = Year, y = TAC), linewidth = 1,
              colour = "#005690") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 65000, 10000),
                       limits = c(0, 65000)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MPs ~ ., scales = "free") +
    labs(x = "Year", y = "TAC (t)") +
    my_theme() +
    theme(strip.placement = "outside")
p02

p03 <- ggplot(data =
                  filter(tab05,
                         MPs %in% c("SCA_100_40_SBMSY",
                                    "SP_100_40_SBMSY",
                                    "SPSS_100_40_SBMSY",
                                    "SP_03", "SP_04"),
                         Year %in% 2020:2053)) +
    geom_ribbon(aes(x = Year,
                    ymin = TAC_Lwr,
                    ymax = TAC_Upr),
                alpha = 0.5, fill = "#FA8072") +
    geom_ribbon(aes(x = Year,
                    ymin = TAC_Lwr2,
                    ymax = TAC_Upr2),
                alpha = 0.4, fill = "#FA8072") +
    geom_line(aes(x = Year, y = TAC), linewidth = 1,
              colour = "#FF4e4e") +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 200000, 50000),
                       limits = c(0, 150000)) +
    scale_x_continuous(breaks = seq(2022, 2053, 2),
                       expand = c(0.01, 0)) +
    coord_cartesian(xlim = c(2023, 2052)) +
    facet_grid(MP2 ~ ., scales = "free") +
    labs(x = "Year", y = "TAC (t)") +
    my_theme() +
    theme(strip.placement = "outside")
p03

ggsave("03_Results/Trajectory_TAC_CC_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_TAC_Index_ver00.png", plot = p02,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

ggsave("03_Results/Trajectory_TAC_Model_ver00.png", plot = p03,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

######@> Violin plot...
tmp <- PM_output %>%
    filter(MP %in% selMPs) %>%
    mutate(MP2 = ifelse(MP == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MP == "SP_100_40_SBMSY", "SP01",
                 ifelse(MP == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MP == "SP_03", "SP02",
                 ifelse(MP == "SP_04", "SPSS02", MP)))))) %>%
    filter(PM %in% c("VarCmedium", "VarC", "VarClong")) %>%
    group_by(sim, MP, MP2, PM) %>%
    summarise(Values = median(Values, na.rm = TRUE)) %>%
    mutate(class = "Reference case [OMs 1-9]")

p00 <- ggplot(data = tmp, aes(x = MP2, y = Values * 100, fill = MP2)) +
    geom_jitter(pch = 21, size = 4, alpha = 0.4) +
    geom_boxplot(outlier.alpha = 0) +
    ## geom_violin() +
    facet_wrap(~class) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = "Candidate Management Procedure",
         y = "Absolute change in TAC (%)") +
    my_theme() +
    theme(legend.position = "none")
p00

ggsave("03_Results/Absolute_Change_TAC_ver00.png", plot = p00,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 40, h = 35)

######@> Creating the Kobe plots (by Year and general)...

#####@> Merging SB/SBMSY and F/FMSY...
df00 <- SB_SBMSY_output %>%
    left_join(F_FMSY_output, by = c("Sim", "MPs", "Year", "OM")) %>%
    mutate(MPs = as.character(MPs)) %>%
    filter(MPs %in% selMPs) %>%
    mutate(MP2 = ifelse(MPs == "SCA_100_40_SBMSY", "SCA01",
                 ifelse(MPs == "SP_100_40_SBMSY", "SP01",
                 ifelse(MPs == "SPSS_100_40_SBMSY", "SPSS01",
                 ifelse(MPs == "SP_03", "SP02",
                 ifelse(MPs == "SP_04", "SPSS02", MPs)))))) %>%
    rename("SB_SBMSY" = value.x, "F_FMSY" = value.y)

#####@> Estimating Kobe values for the last year...
tmp <- df00 %>%
    filter(Year == 2053) %>%
    group_by(Sim, MP2) %>%
    summarise(SB_SBMSY = mean(SB_SBMSY, na.rm = TRUE),
              F_FMSY = mean(F_FMSY, na.rm = TRUE)) %>%
    mutate(OF = ifelse(F_FMSY <= 1, FALSE, TRUE),
           OFD = ifelse(SB_SBMSY <= 1, TRUE, FALSE))

#####@> Percentage of cases...
valdf <- tmp %>%
    group_by(MP2) %>%
    summarise(BL = sum(OF == FALSE & OFD == TRUE)/100 * 100,
              BR = sum(OF == FALSE & OFD == FALSE)/100 * 100,
              TL = sum(OF == TRUE & OFD == TRUE)/100 * 100,
              TR = sum(OF == TRUE & OFD == FALSE)/100 * 100)
valdf <- valdf %>% tidyr::pivot_longer(., cols = 2:5)
valdf$x <- -Inf
valdf$y <- -Inf
valdf$y[valdf$name == "TL"] <- Inf
valdf$y[valdf$name == "TR"] <- Inf
valdf$x[valdf$name == "BR"] <- Inf
valdf$x[valdf$name == "TR"] <- Inf
valdf$value <- round(valdf$value, 2)
valdf$value <- paste0(valdf$value, "%")
valdf$hjustvar <- -2
valdf$vjustvar <- -2
valdf$hjustvar[valdf$name == "TL"] <- -1
valdf$hjustvar[valdf$name == "TR"] <- 2
valdf$hjustvar[valdf$name == "BL"] <- -1
valdf$hjustvar[valdf$name == "BR"] <- 2
valdf$vjustvar[valdf$name == "TL"] <- 2
valdf$vjustvar[valdf$name == "TR"] <- 2
valdf$vjustvar[valdf$name == "BL"] <- -2
valdf$vjustvar[valdf$name == "BR"] <- -2

#####@> Colors to figure...
kobe_df <- bind_rows(data.frame(x = c(0, 0, 1, 1),
                                y = c(0, 1, 1, 0),
                                fill = "bl"),
                     data.frame(x = c(1, 1, 4, 4),
                                y = c(0, 1, 1, 0),
                                fill = "br"),
                     data.frame(x = c(0, 0, 1, 1),
                                y = c(1, 4, 4, 1),
                                fill = "tl"),
                     data.frame(x = c(1, 1, 4, 4),
                                y = c(1, 4, 4, 1),
                                fill = "tr"))
kobe_df$alpha <- 0.3

#####@> Figure dots...
p00 <- ggplot() +
    geom_polygon(data = kobe_df,
                 aes(x = x, y = y, fill = fill, alpha = alpha)) +
    scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                 "#FDBD56")) +
    geom_point(data = tmp,
               aes(x = SB_SBMSY, y = F_FMSY), colour = "gray30",
               alpha = 0.5, size = 4) +
    geom_text(data = valdf, fontface = "bold", size = 6,
              aes(x = x, y = y, label = value,
                  hjust = hjustvar, vjust = vjustvar),
              colour = "gray10") +
    expand_limits(x = c(0, 4), y = c(0, 4)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 4)) +
    geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
    geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
    labs(x = expression(B/B[MSY]), y = expression(F/F[MSY])) +
    facet_wrap(~MP2) +
    my_theme() +
    theme(legend.position = "none")
p00

ggsave("03_Results/Kobe_plot_Last_Year_2053_ver00.png", plot = p00,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 35, h = 35)

######@> Estimating Kobe values per year...
tmp <- df00 %>%
    group_by(Sim, Year, MP2) %>%
    summarise(SB_SBMSY = mean(SB_SBMSY, na.rm = TRUE),
              F_FMSY = mean(F_FMSY, na.rm = TRUE)) %>%
    mutate(OF = ifelse(F_FMSY <= 1, FALSE, TRUE),
           OFD = ifelse(SB_SBMSY <= 1, TRUE, FALSE))

#####@> Proportions by year...
valdf <- tmp %>%
    group_by(Year, MP2) %>%
    summarise(Yellow = sum(OF == FALSE & OFD == TRUE)/100 * 100,
              Green = sum(OF == FALSE & OFD == FALSE)/100 * 100,
              Red = sum(OF == TRUE & OFD == TRUE)/100 * 100,
              Orange = sum(OF == TRUE & OFD == FALSE)/100 * 100) %>%
    pivot_longer(names_to = "Cond", values_to = "Perc", 3:6) %>%
    mutate(Cond = factor(Cond, levels = c("Green", "Yellow", "Orange",
                                          "Red")))

p01 <- ggplot() +
    geom_area(data = filter(valdf, Year %in% 2021:2053),
              aes(x = Year, y = Perc, fill = Cond),
              stat = "identity", colour = "black") +
    facet_wrap(~MP2, ncol = 2) +
    scale_fill_manual(values = c("#67C18B", "#F8DC7A", "#FDBD56",
                                 "#D8775D")) +
    expand_limits(x = c(2021, 2053), y = c(0, 100)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2022, 2053, 3)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = c(2023, 2053)) +
    labs(x = "Year", y = "%") +
    my_theme() +
    theme(legend.position = "none")
p01

ggsave("03_Results/Kobe_plot_by_year_Area_ver00.png", plot = p01,
       device = "png", dpi = 600, bg = "white", unit = "cm",
       w = 30, h = 30)

######@>----------------------------------------------------------------
######@> 10% error implementation - OMs 10-18...

######@> Extracting PMs from all MMSE's objects...

#####@> Listing PMs functions...
PMlist <- list()
for(i in seq_along(PMs)) {
    PMlist[[i]] <- try(get(PMs[i]), silent = TRUE)
}

######@> Extracting values...
PMvalues <- list()
PMvalues2 <- list()
for(i in seq_along(MSEs02)) {
    for(j in seq_along(PMs)) {
        pm <- PMlist[[j]](MSEs02[[i]])
        om <- names(MSEs02[i])
        mps <- pm@MPs
        val <- data.frame(pm@Prob)
        names(val) <- mps
        val <- val %>%
            mutate(sim = 1:100) %>%
            pivot_longer(names_to = "MP", values_to = "Values", 1:20)
        nom <- pm@Name
        cap <- pm@Caption
        pm <- PMs[j]
        PMvalues[[j]] <- val %>%
            mutate(OM = om, Name = nom, Caption = cap, PM = pm) %>%
            select(OM, Name, Caption, sim, MP, PM, Values) %>%
            as.data.frame()
    }
    out <- do.call(rbind.data.frame, PMvalues)
    PMvalues2[[i]] <- out
}
PM_output <- do.call(rbind.data.frame, PMvalues2)

#####@> Looking for NAs...
tab01_NA <- PM_output %>%
    filter(is.na(Values)) %>%
    group_by(OM, MP, PM) %>%
    summarise(N = n()) %>%
    as.data.frame()

#####@> Looking to the PMs - Estimating the average for each OMs...
tab01_exp <- PM_output %>%
    filter(MP %in% selMPs) %>%
    group_by(OM, MP, PM) %>%
    summarise(q50 = mean(Values, na.rm = TRUE)) %>%
    pivot_wider(names_from = "MP", values_from = "q50") %>%
    ## select(OM:SPSS_100_40_SBMSY, SP_100_40_SBMSY, SP_01:SP_06) %>%
    as.data.frame()

###@> Saving the output table...
tab01_exp_10 <- PM_output %>%
    filter(MP %in% selMPs)

####@> Exporting table...
write.table(tab01_exp, "03_Results/Table_PM-MP_OMs_10-18_ver00.csv",
            row.names = FALSE, sep = ",", dec = ".")

######@>----------------------------------------------------------------
######@> 20% error implementation - OMs 19-27...

######@> Extracting PMs from all MMSE's objects...

#####@> Listing PMs functions...
PMlist <- list()
for(i in seq_along(PMs)) {
    PMlist[[i]] <- try(get(PMs[i]), silent = TRUE)
}

######@> Extracting values...
PMvalues <- list()
PMvalues2 <- list()
for(i in seq_along(MSEs03)) {
    for(j in seq_along(PMs)) {
        pm <- PMlist[[j]](MSEs03[[i]])
        om <- names(MSEs03[i])
        mps <- pm@MPs
        val <- data.frame(pm@Prob)
        names(val) <- mps
        val <- val %>%
            mutate(sim = 1:100) %>%
            pivot_longer(names_to = "MP", values_to = "Values", 1:20)
        nom <- pm@Name
        cap <- pm@Caption
        pm <- PMs[j]
        PMvalues[[j]] <- val %>%
            mutate(OM = om, Name = nom, Caption = cap, PM = pm) %>%
            select(OM, Name, Caption, sim, MP, PM, Values) %>%
            as.data.frame()
    }
    out <- do.call(rbind.data.frame, PMvalues)
    PMvalues2[[i]] <- out
}
PM_output <- do.call(rbind.data.frame, PMvalues2)

#####@> Looking for NAs...
tab01_NA <- PM_output %>%
    filter(is.na(Values)) %>%
    group_by(OM, MP, PM) %>%
    summarise(N = n()) %>%
    as.data.frame()

#####@> Looking to the PMs - Estimating the average for each OMs...
tab01_exp <- PM_output %>%
    filter(MP %in% selMPs) %>%
    group_by(OM, MP, PM) %>%
    summarise(q50 = mean(Values, na.rm = TRUE)) %>%
    pivot_wider(names_from = "MP", values_from = "q50") %>%
    ## select(OM:SPSS_100_40_SBMSY, SP_100_40_SBMSY, SP_01:SP_06) %>%
    as.data.frame()

###@> Saving the output table...
tab01_exp_20 <- PM_output %>%
    filter(MP %in% selMPs)

####@> Exporting table...
write.table(tab01_exp, "03_Results/Table_PM-MP_OMs_19-27_ver00.csv",
            row.names = FALSE, sep = ",", dec = ".")

######@>----------------------------------------------------------------
######@> Preparing comparisons between Scenarios (Perfect, 10%, 20%)...

######@> Including references for each scenario...
tab01_exp_ref$scenario <- "Perfect TAC Implementation"
tab01_exp_10$scenario <- "10% TAC Error Implementation"
tab01_exp_20$scenario <- "20% TAC Error Implementation"

######@> Combining datasets...
output <- gtools::smartbind(tab01_exp_ref, tab01_exp_10, tab01_exp_20)

######@> Boxplot...
p00 <- ggplot(data = filter(output, PM %in% c("AvC_long", "AvC_med",
                                              "AvC_short")),
              aes(x = PM, y = Values, fill = scenario)) +
    geom_boxplot(outlier.shape = 21, outlier.fill = "gray10",
                 outlier.colour = "gray10", outlier.alpha = 0.05,
                 outlier.size = 0.4, varwidth = FALSE) +
    ## geom_violin() +
    facet_wrap(~MP, scales = "free", ncol = 3) +
    labs(x = "Performance Metrics", y = "Mediam Catch (t)", fill = "") +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    theme(legend.position = c(0.8, 0.1))
p00

p01 <- ggplot(data = filter(output, PM %in% c("LRP_long", "LRP_med",
                                              "LRP_short", "LRP")),
              aes(x = PM, y = Values, fill = scenario)) +
    geom_boxplot(outlier.shape = 21, outlier.fill = "gray10",
                 outlier.colour = "gray10", outlier.alpha = 0.05,
                 outlier.size = 0.4, varwidth = FALSE) +
    facet_wrap(~MP, scales = "free", ncol = 3) +
    labs(x = "Performance Metrics", y = "Prob. SB < 0.4SBMSY",
         fill = "") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    theme(legend.position = c(0.8, 0.1))
p01

p02 <- ggplot(data = filter(output, PM %in% c("PGK_long", "PGK_med",
                                              "PGK_short", "PGK")),
              aes(x = PM, y = Values, fill = scenario)) +
    geom_boxplot(outlier.shape = 21, outlier.fill = "gray10",
                 outlier.colour = "gray10", outlier.alpha = 0.05,
                 outlier.size = 0.4, varwidth = FALSE) +
    facet_wrap(~MP, scales = "free", ncol = 3) +
    labs(x = "Performance Metrics", y = "Prob. Kobe Green Quadrant", fill = "") +
    my_theme() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    theme(legend.position = c(0.8, 0.1))
p02

p03 <- ggplot(data = filter(output, PM %in% c("VarClong", "VarCmedium",
                                              "VarC")),
              aes(x = PM, y = Values, fill = scenario)) +
    geom_boxplot(outlier.shape = 21, outlier.fill = "gray10",
                 outlier.colour = "gray10", outlier.alpha = 0.05,
                 outlier.size = 0.4, varwidth = FALSE) +
    facet_wrap(~MP, scales = "free", ncol = 3) +
    labs(x = "Performance Metrics", y = "Mean Variation in TAC (%)",
         fill = "") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    theme(legend.position = c(0.8, 0.1))
p03

ggsave("03_Results/Boxplot_AvC_ver00.png", plot = p00, device = "png",
       units = "cm", width = 48, height = 33, dpi = 600,
       bg = "white")

ggsave("03_Results/Boxplot_LRP_ver00.png", plot = p01, device = "png",
       units = "cm", width = 48, height = 33, dpi = 600,
       bg = "white")

ggsave("03_Results/Boxplot_PGK_ver00.png", plot = p02, device = "png",
       units = "cm", width = 48, height = 33, dpi = 600,
       bg = "white")

ggsave("03_Results/Boxplot_VarC_ver00.svg", plot = p03, device = "png",
       units = "cm", width = 48, height = 33, dpi = 600,
       bg = "white")

######@> Ridges...
p00 <- ggplot(data = filter(output, PM %in% c("AvC_long", "AvC_med",
                                              "AvC_short")),
              aes(x = Values, y = PM, fill = scenario, colour = scenario)) +
    stat_density_ridges(quantile_lines = TRUE, alpha = 0.5) +
    facet_wrap(~MP, scales = "free_x", ncol = 5) +
    labs(y = "Performance Metrics", x = "Mediam Catch (t)", fill = "",
         colour = "") +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    scale_colour_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    theme(legend.position = "bottom")
p00

p01 <- ggplot(data = filter(output, PM %in% c("LRP_long", "LRP_med",
                                              "LRP_short", "LRP")),
              aes(x = Values, y = PM, fill = scenario, colour = scenario)) +
    stat_density_ridges(quantile_lines = TRUE, alpha = 0.5) +
    facet_wrap(~MP, scales = "free_x", ncol = 5) +
    labs(y = "Performance Metrics", x = "Prob. SB < 0.4SBMSY",
         fill = "", colour = "") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    scale_colour_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    theme(legend.position = "bottom")
p01

p02 <- ggplot(data = filter(output, PM %in% c("PGK_long", "PGK_med",
                                              "PGK_short", "PGK")),
              aes(x = Values, y = PM, fill = scenario, colour = scenario)) +
    stat_density_ridges(quantile_lines = TRUE, alpha = 0.5) +
    facet_wrap(~MP, scales = "free_x", ncol = 5) +
    labs(y = "Performance Metrics", x = "Prob. Kobe Green Quadrant",
         fill = "", colour = "") +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    scale_colour_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    scale_x_continuous(limits = c(0, 1)) +
    theme(legend.position = "none")
p02

p03 <- ggplot(data = filter(output, PM %in% c("VarClong", "VarCmedium",
                                              "VarC")),
              aes(x = Values, y = PM, fill = scenario, colour = scenario)) +
    stat_density_ridges(quantile_lines = TRUE, alpha = 0.5) +
    facet_wrap(~MP, scales = "free_x", ncol = 5) +
    labs(y = "Performance Metrics", x = "Mean Variation in TAC (%)",
         fill = "", colour = "") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_fill_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    scale_colour_manual(values = gg_color_hue(10)[c(6, 8, 10)]) +
    my_theme() +
    theme(legend.position = "none")
p03

## ggsave("Ridges_AvC_ver00.svg", plot = p00, device = "svg",
##        units = "cm", width = 48, height = 33, dpi = 300,
##        bg = "white")

## ggsave("Ridges_LRP_ver00.svg", plot = p01, device = "svg",
##        units = "cm", width = 48, height = 33, dpi = 300,
##        bg = "white")

## ggsave("Ridges_PGK_ver00.svg", plot = p02, device = "svg",
##        units = "cm", width = 48, height = 33, dpi = 300,
##        bg = "white")

## ggsave("Ridges_VarC_ver00.svg", plot = p03, device = "svg",
##        units = "cm", width = 48, height = 33, dpi = 300,
##        bg = "white")

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
