rm(list=ls(all=TRUE))

library(tidyverse)
library(gamlss)

path = "/Part2_FutureProjection/"

years.ref <- c(1949:2014) # historical period

site.name = "11342000"

model.name = "ACCESS-CM2"
scen.name = "ssp585"

####################
# 1. Estimate trend parameters of Poisson regression model
####################
# create time covariate formula
formula.mu <- as.formula(paste("Y~Time", sep=""))

# 1.1. PRISM-driven flood counts
df.PRISM.t <- read.csv(paste0(path,"../Part1_StatisticalAttributionModeling/Output01-2_Timeseries_FloodCounts_and_Mu_",site.name,".csv"))
df.PRISM <- df.PRISM.t %>% filter(Season == "Annual" & Water.year %in% years.ref)

## Fit Poisson regression model with time covariate
df.input <- data.frame(Year = df.PRISM$Water.year, Y = df.PRISM$Count.model, Time = df.PRISM$Water.year - df.PRISM$Water.year[1]+1)
mod1.full <- gamlss(formula.mu, data=df.input, family=("PO"))

sum.gamlss <- summary(mod1.full, type="qr", save=F)
res.trend.PRISM <- data.frame(Model.name = "PRISM", mu1 = sum.gamlss[2,1], mu1.pval = sum.gamlss[2,4], N.yr = nrow(df.input))

# 1.2. GCM-driven flood counts
df.GCM.t <- read.csv(paste0(path,"Output02_Timeseries_FloodCounts_",model.name,"_",scen.name,"_",site.name,".csv"))
df.GCM <- df.GCM.t %>% filter(Water.year %in% years.ref) %>% dplyr::select(c("Water.year","Annual.model"))

## Fit Poisson regression model with time covariate
df.input <- data.frame(Year = df.GCM$Water.year, Y = df.GCM$Annual.model, Time = df.GCM$Water.year - df.GCM$Water.year[1]+1)
mod1.full <- gamlss(formula.mu, data=df.input, family=("PO"))

sum.gamlss <- summary(mod1.full, type="qr", save=F)
res.trend.GCM <- data.frame(Model.name = "GCM", mu1 = sum.gamlss[2,1], mu1.pval = sum.gamlss[2,4], N.yr = nrow(df.input))


####################
# 2. Assess if GCM reproduces historical (i.e., PRISM) trends in annual flood counts
####################
slope.PRISM <- res.trend.PRISM$mu1
slope.GCM <- res.trend.GCM$mu1
pvalue.PRISM <- res.trend.PRISM$mu1.pval
pvalue.GCM <- res.trend.GCM$mu1.pval

validity = 0
#Condition#1: Trend parameters of PRISM and GCM have the same sign and are significant.
if( sign(slope.PRISM) * sign(slope.GCM) == 1 & pvalue.PRISM <= 0.05 & pvalue.GCM <= 0.05 ) validity = 1

#Condition#2: Trend parameters of PRISM and GCM are not significant.
if( pvalue.PRISM > 0.05 & pvalue.GCM > 0.05 ) validity = 1

#Condition#3: Trend parameter of PRISM (or GCM) are significant, but the other one is not significant.
if( (pvalue.PRISM <= 0.05 & pvalue.GCM > 0.05) | (pvalue.PRISM > 0.05 & pvalue.GCM <= 0.05) ) validity = 0

#Condition#4: Trend parameters of PRISM and GCM are significant, but have opposite trend sign.
if( sign(slope.PRISM) * sign(slope.GCM) == -1 & pvalue.PRISM <= 0.05 & pvalue.GCM <= 0.05 ) validity = 0


####################
# 3. Print evaluation result
####################
if(validity==1){
  print(paste0(model.name," is suitable for ",site.name," streamgage"))
}else if(validity==0){
  print(paste0(model.name," is not suitable for ",site.name," streamgage"))
} 
