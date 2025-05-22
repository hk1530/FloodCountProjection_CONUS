rm(list=ls(all=TRUE))

library(data.table)
library(tidyverse)
library(ggplot2)
library(scales)
library(sf)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)

setwd("C:/Users/hk1530/Desktop/MyPath/Works/11_Papers/15_FutureProjection_FloodFrequency_CONUS/02_Codeworks/Part3_CreateFigures/Figures5-6_Seasonal_contribution/")

scens = c("ssp126","ssp245","ssp370","ssp585")
n.scens = length(scens)

EventsPerYear <- c(1,2,3)
n.cases <- length(EventsPerYear)

seasons <- c("SON","DJF","MAM","JJA","Annual")
n.seasons <- length(seasons)

# read stationo index
station.index.t <- read.csv("../Data01_StationIndex_4632sites.csv", colClasses = c("Site.name" = "character"))

# read model validity results
dat.model.val.t <- read.csv(file = "Results01_Model_availability.csv", header = T, colClasses = c("Site.name"='character'))

names.GagesII <- c("All sites", "Reference sites", "Non-reference sites")

#################################
# 1. create dataframe for ggplot
#################################
# (1) contribution ratio
df.fin.t <- NULL
iepy=2
epy <- paste0(EventsPerYear[iepy],"EventsPerYear")

for(iscen in 1:n.scens){
  scen <- scens[iscen]
  
  res.test <- read.csv(paste0("Results01_Changes_N/Output01_Difference_Seasonality_",scen,"_",epy,".csv"),
                       colClasses = c("Site.name"='character'))
  
  for(isea in 1:n.seasons){
    res.test1 <- res.test %>% filter(Season == seasons[isea])
    
    for(iref in 1:3){
      if(iref == 1) station.index.r1 <- station.index.t
      if(iref == 2) station.index.r1 <- station.index.t %>% filter(GagesII.NHD == "Ref")
      if(iref == 3) station.index.r1 <- station.index.t %>% filter(GagesII.NHD == "Non-ref") 
      res.test2 <- res.test1 %>% filter(Site.name %in% station.index.r1$Site.name)
      n.sites <- nrow(res.test2)
      
      df1 <- merge(res.test2, station.index.r1, by = "Site.name")
      df2 <- df1 %>% add_column(.before="Site.name", GagesII=names.GagesII[iref]) %>%
        add_column(.before="GagesII", Scenario=scen) %>%
        add_column(.before="Scenario", Events.per.year = iepy)
      df.fin.t <- rbind(df.fin.t, df2)          
    }# end reference loop
  }# end season loop
}# end scenario loop

# (2) N of flood events for historical period
df.N.t <- read.csv("df_for_ggplot_ratio_historical.csv")

#################################
# 2. Plot: US map
#################################
# load worldmap
library(rnaturalearth)
worldmap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
usmap <- rnaturalearth::ne_states(iso_a2='US', returnclass = "sf")
oceanmap <- rnaturalearth::ne_download(scale = "medium", type="ocean", category = "physical", returnclass = "sf")
lakemap <- rnaturalearth::ne_download(scale = "medium", type = 'lakes', category = 'physical', returnclass = "sf")

#Northwest
names.subr1 <- c("Washington", "Oregon", "Idaho") 
#Southwest
names.subr2 <- c("California", "Nevada", "Utah", "Arizona", "New Mexico", "Colorado")
#GreatPlainsNorth
names.subr3 <- c("Montana", "Wyoming", "North Dakota", "South Dakota", "Nebraska")
#GreatPlainsSouth
names.subr4 <- c("Kansas", "Oklahoma", "Texas")
#Midwest
names.subr5 <- c("Minnesota", "Iowa", "Missouri", "Wisconsin", "Illinois", "Michigan", "Indiana", "Ohio")
#Northeast
names.subr6 <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New York", "Pennsylvania", "New Jersey", "Delaware", "Maryland", "District of Columbia", "West Virginia")
#Southeast
names.subr7 <- c("Arkansas", "Louisiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida")

usmap.subr1 <- usmap %>% filter(name %in% names.subr1) %>% st_union()
usmap.subr2 <- usmap %>% filter(name %in% names.subr2) %>% st_union()
usmap.subr3 <- usmap %>% filter(name %in% names.subr3) %>% st_union()
usmap.subr4 <- usmap %>% filter(name %in% names.subr4) %>% st_union()
usmap.subr5 <- usmap %>% filter(name %in% names.subr5) %>% st_union()
usmap.subr6 <- usmap %>% filter(name %in% names.subr6) %>% st_union()
usmap.subr7 <- usmap %>% filter(name %in% names.subr7) %>% st_union()

rf <- colorRampPalette(c("blue4","blue","lightblue","white","white","orange","red","red4"))
rf.hist <- colorRampPalette(c("white","lightblue","blue","blue4"))

#########
#plot #1. Difference of contribution ratio based on N
#########
iepy=2
epy <- paste0(EventsPerYear[iepy],"EventsPerYear")

########
# subplot #1. Ratio: historical period
########
df1 <- df.N.t %>% filter(Events.per.year == iepy)

#changes to factors for ordering
df1$Events.per.year <- factor(df1$Events.per.year, levels = c(1,2,3), labels = c("1 event/year","2 events/year", "3 events/year"))
df1$Season <- factor(df1$Season, levels = seasons[1:4], labels = c("Fall","Winter","Spring","Summer"))
df1$Scenario <- factor(df1$Scenario, levels = "historical", labels = c("Historical"))

df2 <- df1 %>% dplyr::select("Scenario","Season","Site.name","Lon","Lat","Ratio.N")

#re-order
df.gg <- rbind(df2 %>% filter(abs(Ratio.N) <= 0.1),
               df2 %>% filter(abs(Ratio.N) > 0.1 & abs(Ratio.N) <= 0.2),
               df2 %>% filter(abs(Ratio.N) > 0.2 & abs(Ratio.N) <= 0.3),
               df2 %>% filter(abs(Ratio.N) > 0.3))
df.gg.sf <- st_as_sf(df.gg, coords = c("Lon","Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


gg.map <- ggplot()
gg.map <- gg.map + geom_sf(data=oceanmap, colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
gg.map <- gg.map + geom_sf(data=worldmap, colour='gray60', fill='gray90', linewidth=0.3)
#gg.map <- gg.map + geom_sf(data=worldmap %>% filter(name == "United States"), colour='gray30', fill='gray90', linewidth=0.3)
gg.map <- gg.map + geom_sf(data=usmap, colour='gray30', fill='gray90', linewidth=0.3)

## add the lakes within the CONUS
gg.map <- gg.map + geom_sf(data=lakemap %>% filter(name_en %in% c("Michigan","Superior","Ontario","Huron","Erie","Ontario")),
                           colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'

## add points
gg.map <- gg.map + geom_sf(data = df.gg.sf, aes(colour=Ratio.N), size=0.3, shape=16)
gg.map <- gg.map + scale_colour_stepsn(limits = c(0,1), colours=rf.hist(10),
                                       space ="Lab", name="Contribution ratio",
                                       breaks=seq(0,1,0.1),labels=seq(0,1,0.1),
                                       na.value=NA)
## add the boundary line of 7 subregions
gg.map <- gg.map + geom_sf(data=usmap.subr1, colour='black', fill='transparent', linewidth=0.5)
gg.map <- gg.map + geom_sf(data=usmap.subr2, colour='black', fill='transparent', linewidth=0.5)
gg.map <- gg.map + geom_sf(data=usmap.subr3, colour='black', fill='transparent', linewidth=0.5)
gg.map <- gg.map + geom_sf(data=usmap.subr4, colour='black', fill='transparent', linewidth=0.5)
gg.map <- gg.map + geom_sf(data=usmap.subr5, colour='black', fill='transparent', linewidth=0.5)
gg.map <- gg.map + geom_sf(data=usmap.subr6, colour='black', fill='transparent', linewidth=0.5)
gg.map <- gg.map + geom_sf(data=usmap.subr7, colour='black', fill='transparent', linewidth=0.5)

gg.map <- gg.map + theme_bw()
gg.map <- gg.map + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.05, unit="cm"),
                         legend.position = "bottom",
                         legend.title = element_text(size=12),
                         legend.text = element_text(size=10),
                         legend.background = element_rect(fill='white'),
                         legend.box.background = element_rect(colour = "transparent"),
                         legend.key = element_rect(fill='transparent'),
                         legend.margin = margin(t=0, r=0.1, b=0.1, l=0.1, unit="cm"),
                         legend.text.align = 0.5,
                         axis.text = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.ticks = element_blank(),
                         strip.text = element_text(size = 14)
)

gg.map <- gg.map + facet_grid(Scenario ~ Season, switch = "y")
gg.map <- gg.map + guides(colour = guide_colorbar(barwidth = 35, barheight = 1,
                                                  title.position = "top", title.hjust=0.5,
                                                  ticks.colour = 'black', ticks.linewidth = 0.7, order = 1)
)

gg.map <- gg.map + coord_sf(crs = st_crs("ESRI:102004"), xlim=c(-2445641.59, 2375286.04), ylim=c(-1626003.26, 1492610.28), expand=F)
gg.map.hist <- gg.map

########
# subplot #2. Ratio difference: future period
########
df1 <- df.fin.t %>% filter(Events.per.year == iepy & GagesII == "All sites" & Season != "Annual")

#changes to factors for ordering
df1$Events.per.year <- factor(df1$Events.per.year, levels = c(1,2,3), labels = c("1 event/year","2 events/year", "3 events/year"))
df1$Season <- factor(df1$Season, levels = seasons[1:4], labels = c("Fall","Winter","Spring","Summer"))
df1$Scenario <- factor(df1$Scenario, levels = scens, labels = c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"))

for(ip in 1:2){
  if(ip == 1) df2 <- df1 %>% dplyr::select("Scenario","Season","Site.name","Lon","Lat","Diff.ratio.N.mid")
  if(ip == 2) df2 <- df1 %>% dplyr::select("Scenario","Season","Site.name","Lon","Lat","Diff.ratio.N.end")
  colnames(df2) <- c("Scenario","Season","Site.name","Lon","Lat","Diff.ratio")
  
  #re-order
  df.gg <- rbind(df2 %>% filter(abs(Diff.ratio) <= 0.1),
                 df2 %>% filter(abs(Diff.ratio) > 0.1 & abs(Diff.ratio) <= 0.2),
                 df2 %>% filter(abs(Diff.ratio) > 0.2 & abs(Diff.ratio) <= 0.3),
                 df2 %>% filter(abs(Diff.ratio) > 0.3))  
  df.gg.sf <- st_as_sf(df.gg, coords = c("Lon","Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  gg.map <- ggplot()
  gg.map <- gg.map + geom_sf(data=oceanmap, colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  gg.map <- gg.map + geom_sf(data=worldmap, colour='gray60', fill='gray90', linewidth=0.3)
  gg.map <- gg.map + geom_sf(data=usmap, colour='gray30', fill='gray90', linewidth=0.3)
  
  ## add the lakes within the CONUS
  gg.map <- gg.map + geom_sf(data=lakemap %>% filter(name_en %in% c("Michigan","Superior","Ontario","Huron","Erie","Ontario")),
                             colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  
  ## add points
  gg.map <- gg.map + geom_sf(data = df.gg.sf, aes(colour=Diff.ratio), size=0.3, shape=16)
  gg.map <- gg.map + scale_colour_stepsn(limits = c(-1,1), colours=rev(c(rf(20))),
                                         space ="Lab", name="Difference in contribution ratio (future-hist)",
                                         breaks=seq(-1,1,0.1),labels=seq(-1,1,0.1),
                                         na.value=NA)
  ## add the boundary line of 7 subregions
  gg.map <- gg.map + geom_sf(data=usmap.subr1, colour='black', fill='transparent', linewidth=0.5)
  gg.map <- gg.map + geom_sf(data=usmap.subr2, colour='black', fill='transparent', linewidth=0.5)
  gg.map <- gg.map + geom_sf(data=usmap.subr3, colour='black', fill='transparent', linewidth=0.5)
  gg.map <- gg.map + geom_sf(data=usmap.subr4, colour='black', fill='transparent', linewidth=0.5)
  gg.map <- gg.map + geom_sf(data=usmap.subr5, colour='black', fill='transparent', linewidth=0.5)
  gg.map <- gg.map + geom_sf(data=usmap.subr6, colour='black', fill='transparent', linewidth=0.5)
  gg.map <- gg.map + geom_sf(data=usmap.subr7, colour='black', fill='transparent', linewidth=0.5)
  
  gg.map <- gg.map + theme_bw()
  gg.map <- gg.map + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.05, unit="cm"),
                           legend.position = "bottom",
                           legend.title = element_text(size=12),
                           legend.text = element_text(size=10),
                           legend.background = element_rect(fill='white'),
                           legend.box.background = element_rect(colour = "transparent"),
                           legend.key = element_rect(fill='transparent'),
                           legend.margin = margin(t=0, r=0.1, b=0.1, l=0.1, unit="cm"),
                           legend.text.align = 0.5,
                           axis.text = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.ticks = element_blank(),
                           strip.text = element_text(size = 14)
  )
  
  gg.map <- gg.map + facet_grid(Scenario ~ Season, switch = "y")
  gg.map <- gg.map + guides(colour = guide_colorbar(barwidth = 35, barheight = 1,
                                                    title.position = "top", title.hjust=0.5,
                                                    ticks.colour = 'black', ticks.linewidth = 0.7, order = 1)
  )
  
  gg.map <- gg.map + coord_sf(crs = st_crs("ESRI:102004"), xlim=c(-2445641.59, 2375286.04), ylim=c(-1626003.26, 1492610.28), expand=F)
  gg.map.future <- gg.map
  
  if(ip==2){
    gg.final <- ggarrange(gg.map.hist, gg.map.future, nrow=2, heights = c(0.7,2),
                          labels = c("a","b"), font.label = list(size = 20), hjust = -0.6, vjust = 1.2)
    png("Figure5.png", width=12, height=12, unit='in', res=300)
    print(gg.final)
    dev.off()  
  }
  if(ip==1){
    png("Figure6.png", width=12, height=8.5, unit='in', res=300)
    print(gg.map.future)
    dev.off()  
  } 
  
}# end period loop
