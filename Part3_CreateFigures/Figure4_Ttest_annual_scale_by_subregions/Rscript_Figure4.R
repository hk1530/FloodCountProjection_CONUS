rm(list=ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(scales)
library(sf)
library(patchwork) # for inset plot
library(ggh4x)

setwd("C:/Users/hk1530/Desktop/MyPath/Works/11_Papers/15_FutureProjection_FloodFrequency_CONUS/02_Codeworks/Part3_CreateFigures/Figure4_Ttest_annual_scale_by_subregions/")

scens = c("ssp126","ssp245","ssp370","ssp585")
n.scens = length(scens)

EventsPerYear <- c(1,2,3)
n.cases <- length(EventsPerYear)

seasons <- c("SON","DJF","MAM","JJA","Annual")
n.seasons <- length(seasons)

names.subr <- c("CONUS","Northwest","Southwest","GreatPlainsNorth","GreatPlainsSouth","Midwest","Northeast","Southeast")
n.subr <- length(names.subr)

# read stationo index
station.index.t <- read.csv("../Data01_StationIndex_4632sites.csv", colClasses = c("Site.name" = "character"))

# read model validity results
dat.model.val.t <- read.csv(file = "Results01_Model_availability.csv", header = T, colClasses = c("Site.name"='character'))


iepy=2
epy <- paste0(EventsPerYear[iepy],"EventsPerYear")

# load station index
dat.model.val <- dat.model.val.t %>% filter(NPOT.per.yr == iepy & SON != "error") # "error" due to not enough sample size
names.site <- dat.model.val$Site.name
n.sites <- length(names.site)

station.index <- station.index.t %>% filter(Site.name %in% names.site)
if(any(station.index$Site.name != names.site)) print(paste0("Check station index - epy:",iepy))


############
# Plot. scatter plot
############
#################################
# 1. create dataframe for ggplot
#################################
df.s1 <- read.csv("ref_period_1985-2014/Output01_ShiftTest_MW_SiteRatio_by_regions_ssp126.csv")
df.s2 <- read.csv("ref_period_1985-2014/Output01_ShiftTest_MW_SiteRatio_by_regions_ssp245.csv")
df.s3 <- read.csv("ref_period_1985-2014/Output01_ShiftTest_MW_SiteRatio_by_regions_ssp370.csv")
df.s5 <- read.csv("ref_period_1985-2014/Output01_ShiftTest_MW_SiteRatio_by_regions_ssp585.csv")

df.fin <- rbind(df.s1 %>% filter(Year.st == 2041) %>% add_column(.before = "Year.st", Scenario = "ssp126") %>% add_column(.before = "Year.st", Or.scen = 1) %>% add_column(.before = "Year.st", Period = "Mid"),
                df.s2 %>% filter(Year.st == 2041) %>% add_column(.before = "Year.st", Scenario = "ssp245") %>% add_column(.before = "Year.st", Or.scen = 2) %>% add_column(.before = "Year.st", Period = "Mid"),
                df.s3 %>% filter(Year.st == 2041) %>% add_column(.before = "Year.st", Scenario = "ssp370") %>% add_column(.before = "Year.st", Or.scen = 3) %>% add_column(.before = "Year.st", Period = "Mid"),
                df.s5 %>% filter(Year.st == 2041) %>% add_column(.before = "Year.st", Scenario = "ssp585") %>% add_column(.before = "Year.st", Or.scen = 4) %>% add_column(.before = "Year.st", Period = "Mid"),
                df.s1 %>% filter(Year.st == 2071) %>% add_column(.before = "Year.st", Scenario = "ssp126") %>% add_column(.before = "Year.st", Or.scen = 1) %>% add_column(.before = "Year.st", Period = "End"),
                df.s2 %>% filter(Year.st == 2071) %>% add_column(.before = "Year.st", Scenario = "ssp245") %>% add_column(.before = "Year.st", Or.scen = 2) %>% add_column(.before = "Year.st", Period = "End"),
                df.s3 %>% filter(Year.st == 2071) %>% add_column(.before = "Year.st", Scenario = "ssp370") %>% add_column(.before = "Year.st", Or.scen = 3) %>% add_column(.before = "Year.st", Period = "End"),
                df.s5 %>% filter(Year.st == 2071) %>% add_column(.before = "Year.st", Scenario = "ssp585") %>% add_column(.before = "Year.st", Or.scen = 4) %>% add_column(.before = "Year.st", Period = "End"))

# calculate the difference between the end and the mid-21st century...
names.shift <- c("Increasing","No","Decreasing")

df.diff.t <- NULL
for(iscen in 1:n.scens){
  for(isubr in 1:n.subr){
    for(ishift in 1:3){
      #iscen=1; isubr=1; ishift=1
      df1 <- df.fin %>% filter(Scenario == scens[iscen] & Subregion == names.subr[isubr] & Shift == names.shift[ishift])
      df.diff <- df1[1,]
      
      values.diff <- df1 %>% filter(Period == "End") %>% dplyr::select("Ratio.Sites","Ratio.Sites.LL","Ratio.Sites.UL") - df1 %>% filter(Period == "Mid") %>% dplyr::select("Ratio.Sites","Ratio.Sites.LL","Ratio.Sites.UL")
      df.diff$Period <- "Diff"
      df.diff$Ratio.Sites <- values.diff$Ratio.Sites
      
      # Standard error for each category
      se_diff <- sqrt(df1$Ratio.Sites[1]/100 * (1 - df1$Ratio.Sites[1]/100) / df1$N.total[1] + df1$Ratio.Sites[2]/100 * (1 - df1$Ratio.Sites[2]/100) / df1$N.total[2])
      # 95% CI (z = 1.96)
      df.diff$Ratio.Sites.LL <- df.diff$Ratio.Sites - 1.96 * se_diff
      df.diff$Ratio.Sites.UL <- df.diff$Ratio.Sites + 1.96 * se_diff
      
      df.diff.t <- rbind(df.diff.t, df.diff)
    }
  }
}

#################################
# 2. Plot
#################################
# set colour palette
palette.blind <- unname(palette.colors(palette = "Okabe-Ito")) # Okabe & Ito (2008): https://jfly.uni-koeln.de/color/

usmap <- ggplot2::borders("state", colour="gray50", fill="white")
#####
# (1) create inset US subregion map
#####
#Northwest
sub1 <- c("washington", "oregon", "idaho") 
#Southwest
sub2 <- c("california", "nevada", "utah", "arizona", "new mexico", "colorado")
#GreatPlainsNorth
sub3 <- c("montana", "wyoming", "north dakota", "south dakota", "nebraska")
#GreatPlainsSouth
sub4 <- c("kansas", "oklahoma", "texas")
#Midwest
sub5 <- c("minnesota", "iowa", "missouri", "wisconsin", "illinois", "michigan", "indiana", "ohio")
#Northeast
sub6 <- c("maine", "new hampshire", "vermont", "massachusetts", "rhode island", "connecticut", "new york", "pennsylvania", "new jersey", "delaware", "maryland", "district of columbia", "west virginia")
#Southeast
sub7 <- c("arkansas", "louisiana", "kentucky", "tennessee", "mississippi", "alabama", "virginia", "north carolina", "south carolina", "georgia", "florida")

sub1.poly <- ggplot2::borders("state", regions=sub1, colour="black", fill=palette.blind[2], linewidth=0.5)
sub2.poly <- ggplot2::borders("state", regions=sub2, colour="black", fill=palette.blind[3], linewidth=0.5)
sub3.poly <- ggplot2::borders("state", regions=sub3, colour="black", fill=palette.blind[4], linewidth=0.5)
sub4.poly <- ggplot2::borders("state", regions=sub4, colour="black", fill=palette.blind[5], linewidth=0.5)
sub5.poly <- ggplot2::borders("state", regions=sub5, colour="black", fill=palette.blind[6], linewidth=0.5)
sub6.poly <- ggplot2::borders("state", regions=sub6, colour="black", fill=palette.blind[7], linewidth=0.5)
sub7.poly <- ggplot2::borders("state", regions=sub7, colour="black", fill=palette.blind[8], linewidth=0.5)

gg <- ggplot()
gg <- gg + usmap
gg <- gg + sub1.poly + sub2.poly + sub3.poly + sub4.poly + sub5.poly + sub6.poly + sub7.poly
gg <- gg + coord_map('lambert', lat0=30, lat1=65, xlim=c(-117, -77), ylim=c(25, 51))
gg <- gg + theme_bw()
gg <- gg + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1, unit="cm"),
                 plot.background = element_rect(fill='transparent',
                                                linetype="solid", colour ="transparent", linewidth=0.3),
                 legend.position = "none",
                 legend.text = element_text(size=15),
                 legend.key = element_blank(),
                 axis.text = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank(),
                 panel.background = element_rect(fill='gray90')
)
gg.subregions <- gg


#####
# (2) create main plot
#####
df.gg <- rbind(df.fin %>% dplyr::select("Scenario","Or.scen","Period","Subregion","Shift","Ratio.Sites","Ratio.Sites.LL","Ratio.Sites.UL"),
               df.diff.t %>% dplyr::select("Scenario","Or.scen","Period","Subregion","Shift","Ratio.Sites","Ratio.Sites.LL","Ratio.Sites.UL"))

#changes to factors for ordering
## (1) Subregion
labels.subr <- c("Contiguous United States","Northwest","Southwest","Great Plains of the North","Great Plains of the South","Midwest","Northeast","Southeast")
df.gg$Subregion <- factor(df.gg$Subregion, levels = names.subr, labels = labels.subr)
### add the number of sites
n.sites.subr.t <- c(nrow(station.index), length(which(station.index$Subregion==names.subr[2])),
                    length(which(station.index$Subregion==names.subr[3])), length(which(station.index$Subregion==names.subr[4])),
                    length(which(station.index$Subregion==names.subr[5])), length(which(station.index$Subregion==names.subr[6])),
                    length(which(station.index$Subregion==names.subr[7])), length(which(station.index$Subregion==names.subr[8])))
levels(df.gg$Subregion) <- paste0(labels.subr," (",n.sites.subr.t,")")

## (2) Scenario
labels.scens <- c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")
df.gg$Scenario <- factor(df.gg$Scenario, levels = scens, labels = labels.scens)
## (3) etc.
df.gg$Period <- factor(df.gg$Period, levels = c("Mid","End","Diff"), labels = c("Mid-21st century", "End of the 21st century","Difference (End-Mid)"))
df.gg$Shift <- factor(df.gg$Shift, levels = c("Increasing","No","Decreasing"), labels=c("More frequent flood events","No change","Less frequent flood events"))


gg <- ggplot(data = df.gg)
gg <- gg + geom_hline(yintercept=0, colour='black', linewidth=0.6, linetype='longdash')
gg <- gg + geom_point(aes(x=Or.scen, y=Ratio.Sites, color=Subregion), size=1.2, position=position_dodge(width=0.65))
gg <- gg + geom_errorbar(aes(x=Or.scen, ymin=Ratio.Sites.LL, ymax=Ratio.Sites.UL, color=Subregion), width=0.7, linewidth=0.5, show.legend = F, position = position_dodge(width=0.65))

gg <- gg + scale_color_manual(values=palette.blind[1:8])
gg <- gg + scale_x_continuous(name="Scenario", limits=c(0.65,4.35), breaks=c(1:4), labels = labels.scens)

gg <- gg + theme_bw()
gg <- gg + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1, unit="cm"),
                 legend.margin = margin(t=0.3, r=1.5, b=1, l=1.5, unit="mm"),
                 legend.title = element_blank(),
                 legend.text = element_text(size=7),
                 legend.position = c(0.685, 0.99),
                 legend.justification = c("left","top"),
                 legend.background = element_rect(fill='white', linetype="solid", 
                                                  colour ="black", linewidth=0.1),
                 legend.key = element_rect(fill='transparent'),
                 legend.key.size = unit(0.1, 'mm'),
                 legend.text.align = 0,
                 legend.spacing.x = unit(1.5, "mm"),
                 legend.spacing.y = unit(1.0, "mm"),
                 strip.text = element_text(size = 12),
                 axis.title = element_text(size = 11),
                 axis.text = element_text(size = 9),
                 axis.text.x = element_text(angle=45, vjust = 1, hjust = 1),
                 panel.spacing.y = unit(0.7, "lines"),
                 panel.grid.major = element_line(colour="gray80", linewidth=0.1, linetype="solid"),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank()
                 
)
gg <- gg + guides(color = guide_legend(byrow = TRUE)) # should add it to space between legend items...
gg <- gg + facet_grid(Period ~ Shift, scales = "free_y")
gg <- gg + facetted_pos_scales(y = list(Period == "Mid-21st century" ~ scale_y_continuous(name="Percentage of streamgages (%)", limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = c(0, 0)),
                                        Period == "End of the 21st century" ~ scale_y_continuous(name="Percentage of streamgages (%)", limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = c(0, 0)),
                                        Period == "Difference (End-Mid)" ~ scale_y_continuous(name="Percentage of streamgages (%)", limits = c(-20, 20), breaks = seq(-20, 20, by = 5), expand = c(0, 0)))
)

gg <- gg + inset_element(gg.subregions, left=0.34, bottom=0.86, right=0.53, top=1.00)

png("Figure4.png", width=9, height=10, unit='in', res=300)
print(gg)
dev.off()
