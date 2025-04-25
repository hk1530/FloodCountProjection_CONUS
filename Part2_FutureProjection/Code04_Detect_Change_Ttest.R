rm(list=ls(all=TRUE))

library(tidyverse)

path = "/Part2_FutureProjection/"


site.name = "11342000"

model.name = "ACCESS-CM2"
scen.name = "ssp585"

# Read GCM-driven flood counts
df.GCM.t <- read.csv(paste0(path,"Output02_Timeseries_FloodCounts_",model.name,"_",scen.name,"_",site.name,".csv"))

# Here, examine if there is a significant change in annual flood counts between historical period and end of 21st century.
season = "Annual"
period = "End_21st"

years.hist <- c(1985:2014)
years.future <- c(2071:2100)

# Obtain flood count
## (1) historical period
df.GCM.hist <- df.GCM.t %>% filter(Water.year %in% years.hist) %>% dplyr::select(c("Water.year","Annual.model"))
count.hist <- df.GCM.hist$Annual.model
## (2) future period
df.GCM.future <- df.GCM.t %>% filter(Water.year %in% years.future) %>% dplyr::select(c("Water.year","Annual.model"))
count.future <- df.GCM.future$Annual.model

# conduct T-test
## test: future > hist
res.increase <- t.test(count.future, count.hist, alternative = "greater")
## test: future < hist
res.decrease <- t.test(count.future, count.hist, alternative = "less")

if(res.increase$p.value <= 0.05){
  print("Annual number of flood events is projected to increase in the future")
}else if(res.decrease$p.value <= 0.05){
  print("Annual number of flood events is projected to decrease in the future")
}else{
  print("Annual number of flood events is projected not to shift in the future")
}

