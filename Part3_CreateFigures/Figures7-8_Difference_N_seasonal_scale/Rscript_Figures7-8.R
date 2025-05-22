rm(list=ls(all=TRUE))

library(data.table)
library(tidyverse)
library(ggplot2)
library(scales)
library(sf)
library(RColorBrewer)

setwd("C:/Users/hk1530/Desktop/MyPath/Works/11_Papers/15_FutureProjection_FloodFrequency_CONUS/02_Codeworks/Part3_CreateFigures/Figures7-8_Difference_N_seasonal_scale/")

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
df.fin.t <- NULL
iepy=2
epy <- paste0(EventsPerYear[iepy],"EventsPerYear")

for(iscen in 1:n.scens){
  #iscen=1
  scen <- scens[iscen]
  
  res.test <- read.csv(paste0("Results01_Changes_N/Output01_Difference_Seasonality_",scen,"_",epy,".csv"),
                       colClasses = c("Site.name"='character'))
  
  for(isea in 1:n.seasons){
    #isea=3
    res.test1 <- res.test %>% filter(Season == seasons[isea])
    
    for(iref in 1:3){
      #iref=1
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

rf <- colorRampPalette(c("blue4","blue","lightblue","white","orange","red","red4"))


#########
# Figure
#########
iepy=2
epy <- paste0(EventsPerYear[iepy],"EventsPerYear")

df1 <- df.fin.t %>% filter(GagesII == "All sites", Season != "Annual", Events.per.year == iepy)

#changes to factors for ordering
df1$Events.per.year <- factor(df1$Events.per.year, levels = c(1,2,3), labels = c("1 event/year","2 events/year", "3 events/year"))
df1$Season <- factor(df1$Season, levels = seasons[1:4], labels = c("Fall","Winter","Spring","Summer"))
df1$Scenario <- factor(df1$Scenario, levels = scens, labels = c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"))

for(ip in 1:2){
  #ip=2
  if(ip == 1) df2 <- df1 %>% dplyr::select("Scenario","Season","Site.name","Lon","Lat","Diff.count.per.year.mid")
  if(ip == 2) df2 <- df1 %>% dplyr::select("Scenario","Season","Site.name","Lon","Lat","Diff.count.per.year.end")
  colnames(df2) <- c("Scenario","Season","Site.name","Lon","Lat","Diff.count.per.year")
  
  df2 <- df2 %>% add_column("Diff.count.total" = round(df2$Diff.count.per.year * 30,0))
  
  labels.count <- c("-30>",
                    paste(seq(-30,-6,6),"~",seq(-25,-1,6)),
                    "0",
                    paste(seq(1,25,6),"~",seq(6,30,6)),
                    ">30"
  )
  df3 <- df2 %>% add_column(Diff.count.total.gg = NA)
  df3$Diff.count.total.gg[which(df3$Diff.count.total < -30)] <- labels.count[1]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(-30:-25))] <- labels.count[2]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(-24:-19))] <- labels.count[3]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(-18:-13))] <- labels.count[4]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(-12:-7))] <- labels.count[5]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(-6:-1))] <- labels.count[6]
  df3$Diff.count.total.gg[which(df3$Diff.count.total == 0)] <- labels.count[7]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(1:6))] <- labels.count[8]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(7:12))] <- labels.count[9]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(13:18))] <- labels.count[10]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(19:24))] <- labels.count[11]
  df3$Diff.count.total.gg[which(df3$Diff.count.total %in% c(25:30))] <- labels.count[12]
  df3$Diff.count.total.gg[which(df3$Diff.count.total > 30)] <- labels.count[13]
  df3$Diff.count.total.gg <- factor(df3$Diff.count.total.gg, levels = labels.count)
  
  #re-order
  df.gg <- rbind(df3 %>% filter(Diff.count.total.gg == labels.count[7]),
                 df3 %>% filter(Diff.count.total.gg %in% labels.count[c(6,8)]),
                 df3 %>% filter(Diff.count.total.gg %in% labels.count[c(5,9)]),
                 df3 %>% filter(Diff.count.total.gg %in% labels.count[-c(5:9)])
  )
  df.gg.sf <- st_as_sf(df.gg, coords = c("Lon","Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  gg.map <- ggplot()
  gg.map <- gg.map + geom_sf(data=oceanmap, colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  gg.map <- gg.map + geom_sf(data=worldmap, colour='gray60', fill='gray90', linewidth=0.3)
  gg.map <- gg.map + geom_sf(data=usmap, colour='gray30', fill='gray90', linewidth=0.3)
  
  ## add the lakes within the CONUS
  gg.map <- gg.map + geom_sf(data=lakemap %>% filter(name_en %in% c("Michigan","Superior","Ontario","Huron","Erie","Ontario")),
                             colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  
  ## add points
  gg.map <- gg.map + geom_sf(data = df.gg.sf, aes(colour=Diff.count.total.gg), size=0.3, shape=16)
  gg.map <- gg.map + scale_colour_manual(values=rev(c(rf(length(labels.count)))),
                                         name="Difference in the number of flood events during the 30 years (future-hist)",
                                         drop=F)
  
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
  gg.map <- gg.map + guides( colour = guide_legend(title.position = "top", title.hjust=0.5, nrow=1,
                                                   override.aes = list(size = 2.5)) )
  
  gg.map <- gg.map + coord_sf(crs = st_crs("ESRI:102004"), xlim=c(-2445641.59, 2375286.04), ylim=c(-1626003.26, 1492610.28), expand=F)
  
  if(ip==2) png("Figure7.png", width=12, height=8.5, unit='in', res=300)
  if(ip==1) png("Figure8.png", width=12, height=8.5, unit='in', res=300)
  print(gg.map)
  dev.off()
}# end period loop
