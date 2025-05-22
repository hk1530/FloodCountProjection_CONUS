rm(list=ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(scales)
library(sf)

setwd("C:/Users/hk1530/Desktop/MyPath/Works/11_Papers/15_FutureProjection_FloodFrequency_CONUS/02_Codeworks/Part3_CreateFigures/Figures2_S2-S3_Covariate_selection_map/")

seasons <- c("SON","DJF","MAM","JJA")
labels.season <- c("Fall","Winter","Spring","Summer")
n.seasons <- length(seasons)

EventsPerYear <- c(1,2,3)
n.cases <- length(EventsPerYear)

# read stationo index
station.index.t <- read.csv("../Data01_StationIndex_4632sites.csv", colClasses = c("Site.name" = "character"))

# read model validity results
dat.model.val.t <- read.csv(file = "Results01_Model_availability.csv", header = T, colClasses = c("Site.name"='character'))

###########################
# load covariate selection results and identify the sign of coefficients
###########################
df.fin <- NULL
names.cov <- c("Mu_prec.conc", "Mu_prec.lag", "Mu_temp.conc", "Mu_temp.lag")
n.cov = length(names.cov)
for(iepy in 1:n.cases){
  epy <- paste0(EventsPerYear[iepy],"EventsPerYear")
  
  for(isea in 1:n.seasons){
    season <- seasons[isea]
    
    var.select <- c("Site.name",season)
    dat.model.val <- dat.model.val.t %>% filter(NPOT.per.yr == iepy) %>% dplyr::select(tidyselect::all_of(var.select))
    
    pos <- which(dat.model.val[,2] != "error")
    dat.model.val <- dat.model.val[pos,]
    
    names.site <- dat.model.val$Site.name
    n.sites <- length(names.site)
    
    station.index <- station.index.t %>% filter(Site.name %in% names.site)
    if(any(station.index$Site.name != names.site)) print(paste0("Check Model availability data: ",epy))
    
    df.cov <- read.csv(paste0("03_SeasonalModeling/",epy,"/Results02_SBC_final/Parameters_SBCselect_",season,".csv"),
                       colClasses = c(rep('character',2),rep('numeric',10)))
    df.cov <- df.cov %>% filter(Site_ID %in% names.site)
    
    if( length(which(station.index$Site.name != df.cov$Site_ID)) != 0 ) print(paste0(epy,' / ',season))
    
    # identify the coefficient sign
    for(icov in 1:n.cov){
      #icov=1
      df.new <- station.index[,-c(7)] %>% add_column(NPOT.per.yr=iepy, Season=labels.season[isea]) %>% add_column(Cov.name = names.cov[icov], Cov.value = NA, Cov.sign = NA)
      
      pos.col <- which(colnames(df.cov) == names.cov[icov])
      df.new$Cov.value <- df.cov[,pos.col]
      
      pos1 <- which(df.new$Cov.value > 0)
      pos2 <- which(df.new$Cov.value < 0 & df.new$Cov.value != -99)
      pos3 <- which(df.new$Cov.value == -99)
      df.new$Cov.sign[pos1] <- "pos"
      df.new$Cov.sign[pos2] <- "neg"
      df.new$Cov.sign[pos3] <- "zero"
      
      pos4 <- which(dat.model.val[,2] == "nyr.Nevents.over.zero<5")
      df.new$Cov.sign[pos4] <- "Less than 5 years"
      
      # re-order the results for ggplot
      pos.less.5 <- which(df.new$Cov.sign == "Less than 5 years")
      pos.zero <- which(df.new$Cov.sign == "zero")
      pos.dec <- which(df.new$Cov.sign == "neg")
      pos.inc <- which(df.new$Cov.sign == "pos")
      
      if( length(unique(c(pos.less.5, pos.zero, pos.dec, pos.inc))) != n.sites ) print(paste0(epy,' / ',season, ' / ',names.cov[icov]))
      
      df.fin <- rbind(df.fin, df.new[pos.less.5,], df.new[pos.zero,], df.new[pos.dec,], df.new[pos.inc,])
    }# emd cov loop
  }# end season loop
}# end epy loop


###########################
# ggplot
###########################
# load worldmap
library(rnaturalearth)
worldmap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
usmap <- rnaturalearth::ne_states(iso_a2='US', returnclass = "sf")
oceanmap <- rnaturalearth::ne_download(scale = "medium", type="ocean", category = "physical", returnclass = "sf")
lakemap <- rnaturalearth::ne_download(scale = "medium", type = 'lakes', category = 'physical', returnclass = "sf")

labels.cov <- c("Concurrent precipitation", "Preceding precipitation", "Concurrent temperature", "Preceding temperature")

# (1) Mu parameter
ncov = 4
for(iepy in 1:n.cases){
  df.gg <- df.fin %>% filter(NPOT.per.yr == iepy)
  df.gg$Cov.name <- factor(df.gg$Cov.name, levels = names.cov, labels = labels.cov)
  df.gg$Season <- factor(df.gg$Season, levels=labels.season)
  
  df.map <- st_as_sf(df.gg, coords = c("Lon","Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  gg.map <- ggplot()
  gg.map <- gg.map + geom_sf(data=oceanmap, colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  gg.map <- gg.map + geom_sf(data=worldmap, colour='gray60', fill='gray90', linewidth=0.3)
  gg.map <- gg.map + geom_sf(data=usmap, colour='gray30', fill='white', linewidth=0.3)
  
  ## add the lakes within the CONUS
  gg.map <- gg.map + geom_sf(data=lakemap %>% filter(name_en %in% c("Michigan","Superior","Ontario","Huron","Erie","Ontario")),
                             colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  
  ## add the sites (N < 5)
  gg.map <- gg.map + geom_sf(data=df.map %>% filter(Cov.sign == "Less than 5 years"), aes(colour=Cov.sign), size=0.8, shape=4, alpha=0.4, stroke=0.5)
  gg.map <- gg.map + scale_colour_manual(name=NULL, values=c('gray20'), labels=c('N < 5'))
  ## add the rest sites
  df.map1 <- df.map %>% filter(Cov.sign != "Less than 5 years")
  df.map1$Cov.sign <- factor(df.map1$Cov.sign, levels = c("pos","neg","zero"))
  gg.map <- gg.map + geom_sf(data=df.map1, aes(fill=Cov.sign), size=1.3, shape=21, colour='transparent')
  gg.map <- gg.map + scale_fill_manual(name=" ",values=c("blue3", "red3", "gray50"),
                                       labels=c(paste0("Positive"),
                                                paste0("Negative"),
                                                paste0("Not selected")), drop=FALSE)
  gg.map <- gg.map + coord_sf(crs = st_crs("ESRI:102004"), xlim=c(-2445641.59, 2375286.04), ylim=c(-1626003.26, 1492610.28), expand=F)
  gg.map <- gg.map + facet_grid(Season ~ Cov.name, switch="y")
  
  gg.map <- gg.map + theme_bw()
  gg.map <- gg.map + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1, unit="cm"),
                           legend.position = c(0.01,0.84),
                           legend.justification = c("left","top"),
                           legend.text = element_text(size=17),
                           legend.background = element_rect(fill='transparent'),
                           legend.key = element_rect(fill='transparent'),
                           axis.text = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.ticks = element_blank(),
                           strip.text = element_text(size = 20),
                           strip.text.y = element_text(angle=180)
  )
  gg.map <- gg.map + guides(fill = guide_legend(override.aes = list(size = 3) ),
                            colour = "none") # adjust legend dots size
  
  if(iepy == 1) png("FigureS2.png",width=21,height=14,unit='in',res=300)
  if(iepy == 2) png("Figure2.png",width=21,height=14,unit='in',res=300)
  if(iepy == 3) png("FigureS3.png",width=21,height=14,unit='in',res=300)
  print(gg.map)
  dev.off()
}
