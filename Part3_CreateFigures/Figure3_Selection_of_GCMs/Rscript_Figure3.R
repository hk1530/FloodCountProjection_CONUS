rm(list=ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(gridExtra)
library(ggh4x) # For modifying text in a specific strip

setwd("C:/Users/hk1530/Desktop/MyPath/Works/11_Papers/15_FutureProjection_FloodFrequency_CONUS/02_Codeworks/Part3_CreateFigures/Figure3_Selection_of_GCMs/")

names.model = c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CAMS-CSM1-0", "CAS-ESM2-0",
                "CESM2", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR",
                "CNRM-ESM2-1", "EC-Earth3", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FGOALS-f3-L", "FGOALS-g3",
                "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "GISS-E2-2-G", "IITM-ESM", "INM-CM4-8",
                "INM-CM5-0", "IPSL-CM6A-LR", "KACE-1-0-G", "MCM-UA-1-0", "MIROC6", "MPI-ESM1-2-HR",
                "MPI-ESM1-2-LR", "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL")
n.models = length(names.model)

EventsPerYear <- c(1,2,3)
n.cases <- length(EventsPerYear)

sig.lev = 0.05

######################
# load the final selection of GCMs
######################
df.GCM.fin.t <- read.csv("Results03_N_GCMs_suitable_bySubregions.csv")
df.GCM.fin <- df.GCM.fin.t %>% filter(NPOT.per.yr == 1 & Threshold == 75, N.subregions == 7)
# finally selected GCMs
sel.mod.fin = unique(df.GCM.fin$Model.name)

######################
# load data.frame for ggplot
######################
df.gg.text.t <- read.csv("df_for_ggplot.csv")

##########
# ggplot. bar plot
##########
names.subr <- c("Northwest","Southwest","GreatPlainsNorth","GreatPlainsSouth","Midwest","Northeast","Southeast")
labels.subr <- c("Northwest","Southwest","Great Plains of the North","Great Plains of the South","Midwest","Northeast","Southeast")
n.subr <- length(names.subr)

df.gg.text.t$NPOT.per.yr <- factor(df.gg.text.t$NPOT.per.yr, levels = c(1,2,3), labels = c("1 event / year","2 events / year","3 events / year"))
df.gg.text.t$Subregion <- factor(df.gg.text.t$Subregion, levels = names.subr, labels = labels.subr)

gg <- ggplot()
gg <- gg + geom_bar(data = df.gg.text.t, aes(x=Subregion, y=Match.per, fill=NPOT.per.yr), stat = "identity", position = "dodge", linewidth=0.5, color="black")

gg <- gg + scale_fill_manual(NULL, values=c("lightblue3","royalblue","darkblue"))
gg <- gg + labs(x="Subregion", y="Percentage of sites where the GCM is valid (%)")

gg <- gg + theme_bw()
gg <- gg + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1, unit="cm"),
                 panel.grid.major.x = element_blank(),
                 legend.title = element_blank(),
                 legend.margin = margin(5, 5, 5, 5),
                 legend.spacing.x = unit(1.5, "mm"),
                 legend.spacing.y = unit(0.5, "mm"),
                 legend.key.size = unit(2, 'mm'),
                 legend.text = element_text(size=8),
                 legend.position = c(0.01, 0.945),
                 legend.justification = c('left','top'),
                 legend.background = element_rect(fill='white', linetype="solid", 
                                                  colour ="black", linewidth = 0.3),
                 strip.text = element_text(size = 10),
                 axis.title = element_text(size = 12),
                 axis.text = element_text(size = 9),
                 axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1))

gg <- gg + geom_hline(yintercept=75, colour='red', linewidth=0.6, linetype='longdash')

# match desired color to the selected GCMs
col.GCM.names <- ifelse(names.model %in% sel.mod.fin, "red", "black")
col.GCM.names_scale <- setNames(col.GCM.names, names.model)
bold.GCM.names <- ifelse(names.model %in% sel.mod.fin, 'bold', 'plain')
bold.GCM.names_scale <- setNames(bold.GCM.names, names.model)
# set strip background color
strip_text <- strip_themed(
  text_x = elem_list_text(colour = col.GCM.names_scale, face = bold.GCM.names_scale)
)

gg <- gg + facet_wrap2(. ~ Model, ncol = 6, strip = strip_text)


png("Figure3.png", width=10, height=9, unit='in', res=300)
print(gg)
dev.off()
