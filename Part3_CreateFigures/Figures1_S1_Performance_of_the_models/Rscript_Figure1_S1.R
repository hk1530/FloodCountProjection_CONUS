rm(list=ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(scales)
library(sf)
library(viridis)
library(gridExtra)
library(ggpubr)

setwd("C:/Users/hk1530/Desktop/MyPath/Works/11_Papers/15_FutureProjection_FloodFrequency_CONUS/02_Codeworks/Part3_CreateFigures/Figures1_S1_Performance_of_the_models/")

# load station index
station.index.t <- read.csv("../Data01_StationIndex_4632sites.csv", colClasses = c("Site.name" = "character"))
n.site <- nrow(station.index.t)

# load worldmap
library(rnaturalearth)
worldmap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
usmap <- rnaturalearth::ne_states(iso_a2='US', returnclass = "sf")
oceanmap <- rnaturalearth::ne_download(scale = "medium", type="ocean", category = "physical", returnclass = "sf")
lakemap <- rnaturalearth::ne_download(scale = "medium", type = 'lakes', category = 'physical', returnclass = "sf")

seasons <- c("SON","DJF","MAM","JJA","Annual")
season.label <- c("Fall","Winter","Spring","Summer","Annual total")
nseason <- length(seasons)

EventsPerYear <- c(1,2,3)
n.cases <- length(EventsPerYear)
epy.label <- paste0(EventsPerYear," events per year")

# load corr.coef results
data.cor <- read.csv("Results01_CorCoef_Spearman_whole.csv", colClasses = c("Site.name" = "character"))
data.cor.CV <- read.csv("Results02_CorCoef_Spearman_whole_LOOCV.csv", colClasses = c("Site.name" = "character"))

#############################
# Plotting correlation maps (Four seasons + annual total)
#############################
# create data.frame for ggplot
df.gg <- data.cor %>% add_column(corr_spearman.CV = data.cor.CV$corr_spearman)
df.gg <- merge(x = df.gg, y = station.index.t[,c(1:3)], by.x = "Site.name", all.x = TRUE)
df.gg$Season <- factor(df.gg$Season, levels=seasons, labels = season.label)
df.gg$NPOT.per.yr <- factor(df.gg$NPOT.per.yr, levels=EventsPerYear, labels = epy.label)

col.pal <- rev(viridis_pal()(100))

###################### 
# Figure 1: 2 events per year
######################
## (1) correlation map
iepy=2
df.gg.sub1 <- df.gg %>% filter(NPOT.per.yr == epy.label[iepy])

gg.map.t <- list()
for(isea in 1:nseason){
  #isea=1
  df.gg.sub2 <- df.gg.sub1 %>% filter(Season == season.label[isea])
  df.map <- st_as_sf(df.gg.sub2, coords = c("Lon","Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  gg.map <- ggplot()
  gg.map <- gg.map + geom_sf(data=oceanmap, colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  gg.map <- gg.map + geom_sf(data=worldmap, colour='gray60', fill='gray90', linewidth=0.3)
  gg.map <- gg.map + geom_sf(data=usmap, colour='gray30', fill='white', linewidth=0.3)
  
  ## add the lakes within the CONUS
  gg.map <- gg.map + geom_sf(data=lakemap %>% filter(name_en %in% c("Michigan","Superior","Ontario","Huron","Erie","Ontario")),
                             colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
  
  gg.map <- gg.map + geom_sf(data=df.map %>% filter(Memo == "nyr.Nevents.over.zero<5"), aes(colour=NPOT.per.yr), size=0.8, shape=4, alpha=0.4, stroke=0.5)
  gg.map <- gg.map + scale_colour_manual(name=NULL, values=c('gray20'), labels=c('Model is not fitted'))
  
  gg.map <- gg.map + geom_sf(data=df.map %>% filter(Memo != "nyr.Nevents.over.zero<5"), aes(fill=corr_spearman), size=1.3, shape=21, stroke=0.1)
  gg.map <- gg.map + scale_fill_stepsn(limits = c(0,1), colours=col.pal,
                                       space ="Lab", name="Correlation coeffficient",
                                       breaks=c(0,0.3,0.5,0.7,1),labels=c(0,0.3,0.5,0.7,1),
                                       oob=squish, na.value=NA)
  gg.map <- gg.map + theme_bw()
  gg.map <- gg.map + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1, unit="cm"),
                           legend.position = "none",
                           axis.text = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.ticks = element_blank(),
                           strip.text = element_text(size = 18)
  )
  gg.map <- gg.map + facet_grid(. ~ Season)
  # convert to NAD 1983 Lambert contiguous USA #https://epsg.io/102004
  gg.map <- gg.map + coord_sf(crs = st_crs("ESRI:102004"), xlim=c(-2445641.59, 2375286.04), ylim=c(-1626003.26, 1492610.28), expand=F)
  
  gg.map.t[[isea]] <- gg.map
}

## (2) correlation boxplot
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
df.box <- rbind(data.frame(Season = df.gg.sub1$Season, Corr.coef = df.gg.sub1$corr_spearman, Case = "Model"),
                data.frame(Season = df.gg.sub1$Season, Corr.coef = df.gg.sub1$corr_spearman.CV, Case = "CV"))
df.box$Season <- factor(df.box$Season, levels=season.label)
df.box$Case <- factor(df.box$Case, levels=c("Model","CV"))

gg.box <- ggplot(df.box, aes(x=Season, y=Corr.coef, fill=Case, pattern=Case))
gg.box <- gg.box + stat_summary(fun.data = f, geom="boxplot", width=0.25, position = "dodge", colour = 'black')
gg.box <- gg.box + scale_fill_manual(name=NULL, values = c('gray','magenta'))

gg.box <- gg.box + scale_y_continuous(name="Correlation coefficient", breaks=seq(0,1,by=0.1))

gg.box <- gg.box + theme_bw()
gg.box <- gg.box + theme(plot.margin = margin(t=0.2, r=0.1, b=0.9, l=0.3, unit="cm"),
                         legend.position = c(0.990,0.010),
                         legend.justification = c("right","bottom"),
                         legend.background = element_rect(fill='white', colour = 'black', linewidth = 0.3),
                         legend.text = element_text(size=11),
                         legend.margin = margin(t=0, r=2, b=1, l=2, unit="mm"),
                         axis.title = element_text(size=13),
                         axis.title.x = element_blank(),
                         axis.text = element_text(size = 11),
                         axis.text.x = element_text(angle = 45,hjust=1, vjust=1))
gg.box <- gg.box + coord_cartesian(ylim = c(0,0.8))

## (3) get legend from corr map
df.map <- df.gg %>% filter(Season==season.label[isea])
gg.map <- ggplot(data=df.map)
gg.map <- gg.map + geom_point(aes(x=Lon, y=Lat, fill=corr_spearman), size=1.3, shape=21, stroke=0.3)
gg.map <- gg.map + scale_fill_stepsn(limits = c(0,1), colours=col.pal,
                                     space ="Lab", name="Correlation coeffficient",
                                     breaks=seq(0,1,0.1),labels=seq(0,1,0.1),
                                     oob=squish, na.value=NA)
gg.map <- gg.map + coord_map('lambert', lat0=30, lat1=65, xlim=c(-119, -78), ylim=c(25, 51))
gg.map <- gg.map + theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit="cm"),
                         legend.margin = margin(t=-0.5, r=0, b=0, l=0.5, unit="cm"),
                         legend.title=element_text(size=13),
                         legend.position = "bottom",
                         legend.justification='left',
                         legend.text = element_text(size=11),
                         axis.text = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.ticks = element_blank(),
                         strip.text = element_text(size = 20, face = "bold")
)
gg.map <- gg.map + guides(fill = guide_colorbar(barwidth = 18, barheight = 0.6,
                                                title.position = "top", title.hjust=0.5,
                                                ticks.colour = 'black', ticks.linewidth = 1.5))
gg.legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend <- gg.legend(gg.map)
dev.off()
  
png("Figure1.png", width=8, height=10, unit='in', res=400)
grid.arrange(gg.map.t[[1]], gg.map.t[[2]], 
             gg.map.t[[3]], gg.map.t[[4]],
             gg.map.t[[5]], gg.box,
             mylegend,
             ncol=2, nrow=4, widths=c(1,1), heights=c(3,3,3,1),
             layout_matrix = rbind(c(1,2), c(3,4), c(5,6), c(7,6))
)
dev.off()



###################### 
# Figure S1: 1 and 3 events per year
######################
## (1) correlation map
col.pal <- rev(viridis_pal()(100))

gg.t <- list()
for(iepy in c(1,3)){
  df.gg.sub1 <- df.gg %>% filter(NPOT.per.yr == epy.label[iepy])
  
  gg.map.t <- list()
  for(isea in 1:nseason){
    df.gg.sub2 <- df.gg.sub1 %>% filter(Season == season.label[isea])
    df.map <- st_as_sf(df.gg.sub2, coords = c("Lon","Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    gg.map <- ggplot()
    gg.map <- gg.map + geom_sf(data=oceanmap, colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
    gg.map <- gg.map + geom_sf(data=worldmap, colour='gray60', fill='gray90', linewidth=0.3)
    gg.map <- gg.map + geom_sf(data=usmap, colour='gray30', fill='white', linewidth=0.3)
    
    ## add the lakes within the CONUS
    gg.map <- gg.map + geom_sf(data=lakemap %>% filter(name_en %in% c("Michigan","Superior","Ontario","Huron","Erie","Ontario")),
                               colour='gray60', fill='lightblue2', linewidth=0.4, alpha=0.7)# 'azure'
    
    gg.map <- gg.map + geom_sf(data=df.map %>% filter(Memo == "nyr.Nevents.over.zero<5"), aes(colour=NPOT.per.yr), size=0.8, shape=4, alpha=0.4, stroke=0.5)
    gg.map <- gg.map + scale_colour_manual(name=NULL, values=c('gray20'), labels=c('Model is not fitted'))
    
    gg.map <- gg.map + geom_sf(data=df.map %>% filter(Memo != "nyr.Nevents.over.zero<5"), aes(fill=corr_spearman), size=1.3, shape=21, stroke=0.1)
    gg.map <- gg.map + scale_fill_stepsn(limits = c(0,1), colours=col.pal,
                                         space ="Lab", name="Correlation coeffficient",
                                         breaks=c(0,0.3,0.5,0.7,1),labels=c(0,0.3,0.5,0.7,1),
                                         oob=squish, na.value=NA)
    gg.map <- gg.map + theme_bw()
    gg.map <- gg.map + theme(plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1, unit="cm"),
                             legend.position = "none",
                             axis.text = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             axis.ticks = element_blank(),
                             strip.text = element_text(size = 18)
    )
    gg.map <- gg.map + facet_grid(. ~ Season)
    # convert to NAD 1983 Lambert contiguous USA #https://epsg.io/102004
    gg.map <- gg.map + coord_sf(crs = st_crs("ESRI:102004"), xlim=c(-2445641.59, 2375286.04), ylim=c(-1626003.26, 1492610.28), expand=F)
    
    gg.map.t[[isea]] <- gg.map
  }
  
  ## (2) correlation boxplot
  f <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }
  
  df.box <- rbind(data.frame(Season = df.gg.sub1$Season, Corr.coef = df.gg.sub1$corr_spearman, Case = "Model"),
                  data.frame(Season = df.gg.sub1$Season, Corr.coef = df.gg.sub1$corr_spearman.CV, Case = "CV"))
  df.box$Season <- factor(df.box$Season, levels=season.label)
  df.box$Case <- factor(df.box$Case, levels=c("Model","CV"))
  
  gg.box <- ggplot(df.box, aes(x=Season, y=Corr.coef, fill=Case, pattern=Case))
  gg.box <- gg.box + stat_summary(fun.data = f, geom="boxplot", width=0.25, position = "dodge", colour = 'black')
  gg.box <- gg.box + scale_fill_manual(name=NULL, values = c('gray','magenta'))
  
  gg.box <- gg.box + scale_y_continuous(name="Correlation coefficient", breaks=seq(0,1,by=0.1))
  
  gg.box <- gg.box + theme_bw()
  gg.box <- gg.box + theme(plot.margin = margin(t=0.2, r=0.1, b=0.9, l=0.3, unit="cm"),
                           legend.position = c(0.990,0.010),
                           legend.justification = c("right","bottom"),
                           legend.background = element_rect(fill='white', colour = 'black', linewidth = 0.3),
                           legend.text = element_text(size=11),
                           legend.margin = margin(t=0, r=2, b=1, l=2, unit="mm"),
                           axis.title = element_text(size=13),
                           axis.title.x = element_blank(),
                           axis.text = element_text(size = 11),
                           axis.text.x = element_text(angle = 45,hjust=1, vjust=1))
  gg.box <- gg.box + coord_cartesian(ylim = c(0,0.85))
  
  ## (3) get legend from corr map
  df.map <- df.gg %>% filter(Season==season.label[isea])
  gg.map <- ggplot(data=df.map)
  gg.map <- gg.map + geom_point(aes(x=Lon, y=Lat, fill=corr_spearman), size=1.3, shape=21, stroke=0.3)
  gg.map <- gg.map + scale_fill_stepsn(limits = c(0,1), colours=col.pal,
                                       space ="Lab", name="Correlation coeffficient",
                                       breaks=seq(0,1,0.1),labels=seq(0,1,0.1),
                                       oob=squish, na.value=NA)
  gg.map <- gg.map + coord_map('lambert', lat0=30, lat1=65, xlim=c(-119, -78), ylim=c(25, 51))
  gg.map <- gg.map + theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit="cm"),
                           legend.margin = margin(t=-0.5, r=0, b=0, l=0.5, unit="cm"),
                           legend.title=element_text(size=13),
                           legend.position = "bottom",
                           legend.justification='left',
                           legend.text = element_text(size=11),
                           axis.text = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.ticks = element_blank(),
                           strip.text = element_text(size = 20, face = "bold")
  )
  gg.map <- gg.map + guides(fill = guide_colorbar(barwidth = 18, barheight = 0.6,
                                                  title.position = "top", title.hjust=0.5,
                                                  ticks.colour = 'black', ticks.linewidth = 1.5))
  gg.legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  mylegend <- gg.legend(gg.map)
  dev.off()
  
  gg.t[[iepy]] <- grid.arrange(gg.map.t[[1]], gg.map.t[[2]], 
                               gg.map.t[[3]], gg.map.t[[4]],
                               gg.map.t[[5]], gg.box,
                               mylegend,
                               ncol=2, nrow=4, widths=c(1,1), heights=c(3,3,3,1),
                               layout_matrix = rbind(c(1,2), c(3,4), c(5,6), c(7,6))
  )
}

# Add blank space using a blank plot
gg.blank <- ggplot() + theme_void()

gg.final <- ggarrange(gg.blank, gg.t[[1]], gg.blank, gg.t[[3]], labels = c("a","","b",""),
                      ncol=4, font.label = list(size = 20), hjust = -0.8, widths = c(0.04,1, 0.04, 1))

png("FigureS1.png", width=16.5, height=10, unit='in', res=400)
print(gg.final)
dev.off()

