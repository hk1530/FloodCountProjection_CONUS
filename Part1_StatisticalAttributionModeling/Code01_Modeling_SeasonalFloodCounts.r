rm(list=ls(all=TRUE))

library(tidyverse)
library(gamlss)
library(lubridate)

path = "/Part1_StatisticalAttributionModeling/"

years.ref <- c(1949:2022)

names.season = c("SON","DJF","MAM","JJA")
n.season <- length(names.season)

site.name = "11342000"

# Read Input01. Flood count series
df.count.t <- read.csv(paste0(path,"Input01_FloodCounts_USGS_",site.name,".csv"), header = T)
df.count <- df.count.t %>% filter(NPOT.per.yr == 2) # POT sampling based on the threshold yielding two events per year on average

# Read Input02. Basin- and season-averaged climate predictors
df.pred <- read.csv(paste0(path,"Input02_Climate_Predictors_PRISM_",site.name,".csv"), header = T)
df.ppt <- df.pred %>% filter(Variable == "PPT")
df.tmean <- df.pred %>% filter(Variable == "TMEAN")


res.count.t <- NULL
df.coef.t <- NULL
for(isea in 1:n.season){
  #isea=1
  season <- names.season[isea]
  if(season == "DJF") season.lag <- "SON"
  if(season == "MAM") season.lag <- "DJF"
  if(season == "JJA") season.lag <- "MAM"
  if(season == "SON") season.lag <- "JJA"
  
  res.count <- data.frame(Water.year = years.ref)
  
  df.count.sea <- df.count %>% dplyr::select( tidyselect::all_of(c("Year",season)) )
  years.water <- intersect(years.ref, df.count.sea$Year) # Possible years to model the number of flood events
  
  ####################
  # 1. create a data.frame for statistcal attribution modeling
  ####################
  df.input <- data.frame(Year = years.water)
  
  # (1.1) add predictand
  predictand <- df.count.sea %>% filter(Year %in% years.water)
  df.input <- df.input %>% add_column(Y = predictand[,2])
  
  # When POT flood events are less than a five-year record, we do not fit models and assume that no flooding occurs.
  n.yr.flood <- length(which(df.input$Y != 0))
  if(n.yr.flood < 5){
    # 3.1. mu parameters
    df.res.mu <- data.frame(Water.year = df.input$Year, Mu = 0)
    res.count <- merge(res.count, df.res.mu, by = "Water.year", all.x =TRUE)
    
    # 3.2. flood counts
    df.res.count <- data.frame(Water.year = df.input$Year, Count.model = 0, Count.obs = df.input$Y)
    res.count <- merge(res.count, df.res.count, by = "Water.year", all.x =TRUE)
    
    res.count <- res.count %>% add_column(Season = season)
    res.count.t <- rbind(res.count.t, res.count)
    
    # 3.3. coefficients
    df.coef = matrix(-99,1,5) 
    colnames(df.coef) = c( paste0("Mu_",c("intercept",cov.conc,cov.lag)) )
    df.coef <- data.frame(Site_ID = site.name, df.coef)
    df.coef <- df.coef %>% add_column(Season = season)
    df.coef.t <- rbind(df.coef.t, df.coef)
    
    next
  }
  
  # (1.2) add climate predictors
  # Here water year (SON-JJA) is applied (because 'df.pred' is not based on water year).
  if(season == "SON"){
    ## Concurrent predictors
    var.select <- c("Year",season)
    df.input <- df.input %>% add_column(prec.conc = NA) %>% add_column(temp.conc = NA)
    pr.con <- df.ppt %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1)) # select the previous year
    df.input$prec.conc[which(years.water %in% (pr.con$Year+1))] <- pr.con[,2]
    
    tas.con <- df.tmean %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1)) # select the previous year
    df.input$temp.conc[which(years.water %in% (tas.con$Year+1))] <- tas.con[,2]
    
    ## Lagged predictors
    var.select <- c("Year",season.lag)
    df.input <- df.input %>% add_column(prec.lag = NA) %>% add_column(temp.lag = NA)
    pr.lag <- df.ppt %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1)) # select the previous year
    df.input$prec.lag[which(years.water %in% (pr.lag$Year+1))] <- pr.lag[,2]
    
    tas.lag <- df.tmean %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1)) # select the previous year
    df.input$temp.lag[which(years.water %in% (tas.lag$Year+1))] <- tas.lag[,2]
  }else if(season == "DJF"){
    ## Concurrent predictors
    var.select <- c("Year",season)
    pr.con <- df.ppt %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(prec.conc = pr.con[,2])
    
    tas.con <- df.tmean %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(temp.conc = tas.con[,2])
    
    ## Lagged predictors
    var.select <- c("Year",season.lag)
    df.input <- df.input %>% add_column(prec.lag = NA) %>% add_column(temp.lag = NA)
    pr.lag <- df.ppt %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1)) # select the previous year
    df.input$prec.lag[which(years.water %in% (pr.lag$Year+1))] <- pr.lag[,2]
    
    tas.lag <- df.tmean %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1)) # select the previous year
    df.input$temp.lag[which(years.water %in% (tas.lag$Year+1))] <- tas.lag[,2]
  }else{
    ## Concurrent predictors
    var.select <- c("Year",season)
    pr.con <- df.ppt %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(prec.conc = pr.con[,2])
    
    tas.con <- df.tmean %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(temp.conc = tas.con[,2])
    
    ## Lagged predictors
    var.select <- c("Year",season.lag)
    pr.lag <- df.ppt %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(prec.lag = pr.lag[,2])
    
    tas.lag <- df.tmean %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(temp.lag = tas.lag[,2])
  }
  
  ####################
  # 2. model
  ####################
  # create covariate formula
  cov.conc <- c("prec.conc","temp.conc")
  cov.lag <- c("prec.lag","temp.lag")  
  formula.mu <- as.formula(paste("Y~", paste(c(cov.conc, cov.lag),collapse="+"), sep=""))
  
  # Fit parameters of gamma regression model using all predictors  
  mod1.full <- gamlss(formula.mu, 
                      data=df.input, family=("PO"))
  
  # Find the best (parsimonious) model based on SBC using stepGAIC function
  mod2 <- stepGAIC(mod1.full, what=c("mu"), k=log(dim(df.input)[1]), trace=0)
  
  ####################
  # 3. save results
  ####################
  # 3.1. mu parameters
  df.res.mu <- data.frame(Water.year = df.input$Year, Mu = fitted(mod2, "mu"))
  res.count <- merge(res.count, df.res.mu, by = "Water.year", all.x =TRUE)
  
  # 3.2. flood counts
  df.res.count <- data.frame(Water.year = df.input$Year, Count.model = qPO(0.5, df.res.mu$Mu), Count.obs = df.input$Y)
  res.count <- merge(res.count, df.res.count, by = "Water.year", all.x =TRUE)
  
  res.count <- res.count %>% add_column(Season = season)
  res.count.t <- rbind(res.count.t, res.count)
  
  # 3.3. statistical attribution model coefficients
  df.coef = matrix(-99,1,5) 
  colnames(df.coef) = c( paste0("Mu_",c("intercept",cov.conc,cov.lag)) )
  df.coef <- data.frame(Site_ID = site.name, df.coef)
  
  mu.est <- mod2$mu.coefficients
  for(ipar in 1:length(mu.est)){
    if(names(mu.est)[ipar] == "(Intercept)") df.coef$Mu_intercept <- mu.est[ipar]
    if(names(mu.est)[ipar] == "prec.conc")  df.coef$Mu_prec.conc <- mu.est[ipar]
    if(names(mu.est)[ipar] == "temp.conc")  df.coef$Mu_temp.conc <- mu.est[ipar]
    if(names(mu.est)[ipar] == "prec.lag")  df.coef$Mu_prec.lag <- mu.est[ipar]
    if(names(mu.est)[ipar] == "temp.lag")  df.coef$Mu_temp.lag <- mu.est[ipar]
  }
  df.coef <- df.coef %>% add_column(Season = season)
  df.coef.t <- rbind(df.coef.t, df.coef)
}
write.csv(df.coef.t, file=paste0(path,"Output01-1_Coefficient_Mu_",site.name,".csv"), row.names = F)

####################
# 4. Calculate annual flood counts
####################
res.count.annual <- NULL
n.yr.ref <- length(years.ref)
for(iyr in 1:n.yr.ref){
  #iyr = 1
  res.count <- res.count.t %>% filter(Water.year == years.ref[iyr])
  mu.annual <- sum(res.count$Mu)
  model.annual <- qPO(0.5, mu.annual)
  obs.annual <- sum(res.count$Count.obs)
  
  df.count.annual <- data.frame(Water.year = years.ref[iyr], Mu = mu.annual, Count.model = model.annual, Count.obs = obs.annual, Season = "Annual")
  res.count.annual <- rbind(res.count.annual, df.count.annual)
}
res.count.t <- rbind(res.count.t, res.count.annual)

write.csv(res.count.t, file=paste0(path,"Output01-2_Timeseries_FloodCounts_and_Mu_",site.name,".csv"), row.names = F)

#################################################################
## End!
#################################################################
