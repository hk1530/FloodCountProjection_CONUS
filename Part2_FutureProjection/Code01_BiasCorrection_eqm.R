# This script for empirical quantile mapping bias correction
rm(list=ls(all=TRUE)) ; cat('\014') ; gc()

library(tidyverse)

path = "/Part2_FutureProjection/"
source(paste0(path,"Required_Functions.R")) # Source code for empirical quantile mapping


site.name = "11342000"

model.name = "ACCESS-CM2"
scen.name = "ssp585"

yr.ref <- c(1948:2014) # Reference period for bias-correction
yr.future <- c(2015:2100) # Future period of GCM outputs
yr.whole <- c(1948:2100) # Whole period of GCM outputs


# Read Input01. Raw GCM output (Basin averaged monthly precipitation and temperature)
df.GCM.t <- read.csv(paste0(path,"Input01_Climate_Variables_",model.name,"_",scen.name,"_Monthly_",site.name,".csv"), header = T)

# Read Input02. PRISM observations (Basin averaged monthly precipitation and temperature)
df.obs.t <- read.csv(paste0(path,"Input02_Climate_Variables_PRISM_Monthly_",site.name,".csv"), header = T)

####################
# 1. perform bias-correction at monthly scale
####################
res.bc.monthly.t <- NULL
for(variable in c("pr","tas")){
  #variable = "pr"
  df.GCM <- df.GCM.t %>% filter(Variable == variable)
  if(variable == "pr") df.obs <- df.obs.t %>% filter(Variable == "PPT")
  if(variable == "tas") df.obs <- df.obs.t %>% filter(Variable == "TMEAN")
  
  for(imn in 1:12){
    #imn=6
    df.o <- df.obs %>% filter(Month == imn & Year %in% yr.ref)
    
    df.m.w <- df.GCM %>% filter(Month == imn & Year %in% yr.whole)
    df.m.h <- df.GCM %>% filter(Month == imn & Year %in% yr.ref)
    df.m.f <- df.GCM %>% filter(Month == imn & Year %in% yr.future)
    
    # run EQM
    o    <- df.o$Value
    p    <- df.m.h$Value
    s    <- df.m.w$Value     #s: modeled data for the whole period
    if(variable == "pr") s.bc <- eqm(o, p, s, precip=TRUE, pr.threshold = 0.1, n.quantiles=NULL, extrapolation = "constant")
    if(variable == "tas") s.bc <- eqm(o, p, s, precip=FALSE, n.quantiles=NULL, extrapolation = "constant")
    
    # If NA produced, replace NA value with zero.
    if( any(is.na(s.bc)) ) s.bc[which(is.na(s.bc))] <- 0
    
    #if the corrected precipitation value < 0, make them 0
    if( variable == "pr" & any(s.bc < 0) ) s.bc[which(s.bc < 0)] <- 0
    
    df.bc <- data.frame(Year = yr.whole, Month = imn, Value.mod.raw = s, Value.mod.bc = round(s.bc,3), Variable = variable)
    
    res.bc.monthly.t <- rbind(res.bc.monthly.t, df.bc)
  }# end monthly loop
}# end variable loop

####################
# 2. calculate seasonal average
####################
n.yr <- length(yr.whole)

res.bc.seasonal.t <- NULL
for(variable in c("pr","tas")){
  #variable = "pr"
  res.bc.monthly <- res.bc.monthly.t %>% filter(Variable == variable)
  
  for(iyr in 1:n.yr){
    #iyr=1
    yr <- yr.whole[iyr]
    
    # DJF
    pos.DJF <- c( which(res.bc.monthly$Year==(yr-1) & res.bc.monthly$Month ==12), which(res.bc.monthly$Year==yr & res.bc.monthly$Month %in% c(1,2)) )
    df.DJF <- res.bc.monthly[pos.DJF,]
    if( nrow(df.DJF) == 3){
      mean.DJF <- mean(df.DJF$Value.mod.bc, na.rm = T)  
    }else{
      mean.DJF <- NA
    }
    
    # MAM
    pos.MAM <- which(res.bc.monthly$Year==yr & res.bc.monthly$Month %in% c(3,4,5))
    df.MAM <- res.bc.monthly[pos.MAM,]
    if( nrow(df.MAM) == 3){
      mean.MAM <- mean(df.MAM$Value.mod.bc, na.rm = T)  
    }else{
      mean.MAM <- NA
    }
    # JJA
    pos.JJA <- which(res.bc.monthly$Year==yr & res.bc.monthly$Month %in% c(6,7,8))
    df.JJA <- res.bc.monthly[pos.JJA,]
    if( nrow(df.JJA) == 3){
      mean.JJA <- mean(df.JJA$Value.mod.bc, na.rm = T)  
    }else{
      mean.JJA <- NA
    }
    # SON
    pos.SON <- which(res.bc.monthly$Year==yr & res.bc.monthly$Month %in% c(9,10,11))
    df.SON <- res.bc.monthly[pos.SON,]
    if( nrow(df.SON) == 3){
      mean.SON <- mean(df.SON$Value.mod.bc, na.rm = T)  
    }else{
      mean.SON <- NA
    }
    
    res.bc.seasonal <- data.frame(Year = yr, DJF = round(mean.DJF,3), MAM = round(mean.MAM,3),
                                  JJA = round(mean.JJA,3), SON = round(mean.SON,3), Variable = variable)
    res.bc.seasonal.t <- rbind(res.bc.seasonal.t, res.bc.seasonal)
  }# end year loop
}# end variable loop

write.csv(res.bc.seasonal.t, paste0(path,"Output01_Climate_Predictors_",model.name,"_",scen.name,"_eqm_11342000.csv"), row.names = F)
