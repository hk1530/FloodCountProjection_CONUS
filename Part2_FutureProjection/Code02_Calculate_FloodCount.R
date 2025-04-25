rm(list=ls(all=TRUE))

library(tidyverse)
library(gamlss)

path = "/Part2_FutureProjection/"

years.water <- c(1949:2100)

names.season = c("SON","DJF","MAM","JJA")
n.season <- length(names.season)

site.name = "11342000"

model.name = "ACCESS-CM2"
scen.name = "ssp585"

# Load statistical attribution model parameters
par.dat.t <- read.csv(paste0(path,"../Part1_StatisticalAttributionModeling/Output01-1_Coefficient_Mu_.csv"))

# Load GCM driven climate predictors
df.cov.t <- read.csv(paste0(path,"Output01_Climate_Predictors_ACCESS-CM2_ssp585_eqm_11342000.csv"))
df.cov.pr <- df.cov.t %>% filter(Variable == "pr")
df.cov.tas <- df.cov.t %>% filter(Variable == "tas")



df.ts.mu <- data.frame(Water.year = years.water)
df.ts.model <- data.frame(Water.year = years.water)
for(isea in 1:n.season){
  #isea=1
  season <- names.season[isea]
  if(season == "SON") season.lag <- "JJA"
  if(season == "DJF") season.lag <- "SON"
  if(season == "MAM") season.lag <- "DJF"
  if(season == "JJA") season.lag <- "MAM"
  
  par.dat <- par.dat.t %>% filter(Season == season) %>% dplyr::select(-c("Site_ID","Season"))
  if( all(par.dat == -99) ){
    df.ts.mu <- df.ts.mu %>% add_column(!!paste0(season,".mu") := 0)
    df.ts.model <- df.ts.model %>% add_column(!!paste0(season,".model") := 0)
    next
  }else{
    par.dat[which(par.dat==-99)] <- 0
    par.dat[which(is.na(par.dat))] <- 0 # if par is NA, I replace it with zero...
  }
  
  ####################
  # 1. create a data.frame to input statistcal attribution model
  ####################
  df.input <- data.frame(Year.Water = years.water)
  
  # add climate predictors
  # Here water year (SON-JJA) is applied (because 'df.pred' is not based on water year).
  if(season == "SON"){
    ## Concurrent predictors
    var.select <- c("Year",season)
    df.input <- df.input %>% add_column(prec.conc = NA) %>% add_column(temp.conc = NA)
    pr.con <- df.cov.pr %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1))
    df.input$prec.conc[which(years.water %in% (pr.con$Year+1))] <- pr.con[,2]
    
    tas.con <- df.cov.tas %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1))
    df.input$temp.conc[which(years.water %in% (tas.con$Year+1))] <- tas.con[,2]
    
    ## Lagged predictors
    var.select <- c("Year",season.lag)
    df.input <- df.input %>% add_column(prec.lag = NA) %>% add_column(temp.lag = NA)
    pr.lag <- df.cov.pr %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1))
    df.input$prec.lag[which(years.water %in% (pr.lag$Year+1))] <- pr.lag[,2]
    
    tas.lag <- df.cov.tas %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1))
    df.input$temp.lag[which(years.water %in% (tas.lag$Year+1))] <- tas.lag[,2]
  }else if(season == "DJF"){
    ## Concurrent predictors
    var.select <- c("Year",season)
    pr.con <- df.cov.pr %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(prec.conc = pr.con[,2])
    
    tas.con <- df.cov.tas %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(temp.conc = tas.con[,2])
    
    ## Lagged predictors
    var.select <- c("Year",season.lag)
    df.input <- df.input %>% add_column(prec.lag = NA) %>% add_column(temp.lag = NA)
    pr.lag <- df.cov.pr %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1))
    df.input$prec.lag[which(years.water %in% (pr.lag$Year+1))] <- pr.lag[,2]
    
    tas.lag <- df.cov.tas %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% (years.water-1))
    df.input$temp.lag[which(years.water %in% (tas.lag$Year+1))] <- tas.lag[,2]
  }else{
    ## Concurrent predictors
    var.select <- c("Year",season)
    pr.con <- df.cov.pr %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(prec.conc = pr.con[,2])
    
    tas.con <- df.cov.tas %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(temp.conc = tas.con[,2])
    
    ## Lagged predictors
    var.select <- c("Year",season.lag)
    pr.lag <- df.cov.pr %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(prec.lag = pr.lag[,2])
    
    tas.lag <- df.cov.tas %>% dplyr::select( tidyselect::all_of(var.select) ) %>% filter(Year %in% years.water)
    df.input <- df.input %>% add_column(temp.lag = tas.lag[,2])
  }
  
  ####################
  # 2. set new parameters by combining parameter estimates and new covariates
  ####################
  mu.fitted <- exp(par.dat$Mu_intercept + 
                     par.dat$Mu_prec.conc * df.input$prec.conc + 
                     par.dat$Mu_temp.conc * df.input$temp.conc +
                     par.dat$Mu_prec.lag * df.input$prec.lag +
                     par.dat$Mu_temp.lag * df.input$temp.lag)
  df.ts.mu <- df.ts.mu %>% add_column(!!paste0(season,".mu") := mu.fitted)
  
  ##########################################
  ## (4) find quantiles
  ##########################################
  Q.ts <- qPO(p=0.5, mu=mu.fitted)
  df.ts.model <- df.ts.model %>% add_column(!!paste0(season,".model") := Q.ts)
}# end season loop

##########################################
## (5) calculate annual sum
##########################################
df.ts.mu.fin <- data.frame(df.ts.mu[,c(2:5)], Annual.mu = apply(df.ts.mu[,c(2:5)], 1, sum))
count.annual <- qPO(p=0.5, mu = df.ts.mu.fin$Annual.mu)

df.ts.fin <- df.ts.model %>% add_column(Annual.model = count.annual)

write.csv(df.ts.fin, paste0(path, "Output02_Timeseries_FloodCounts_",model.name,"_",scen.name,"_",site.name,".csv"), row.names=FALSE)

