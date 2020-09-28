



### This script calculates the compensating variation and equivalent variation for each household, given the difference between its current water use and optimal water use


# load packages
library(ggplot2); library(doParallel)
registerDoParallel(cores = 10)


# load definitions
source("MAIN/00-definitions.R")


# load EV-CV function
source("MAIN/Code/Functions/03a-ev-cv-fcns.R")


# load data
all.sfds <- readRDS('C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/02-optimal-consumption.rds')





##### calculate EV and CV


welfare_data <- foreach(i = 1:5) %dopar% {
  
  
  dat <- all.sfds[i,]
  
  
  
  # if the household has a sewer, only worry about the water bill
  if(dat$no.sewer == 'Other'){
    
    x <- welfare_difference(qe = dat$avg.30day.use,
                            s  = elasticity_demand,
                            bc = 80000,
                            fc = fixed_water_rate,
                            block.1 = volume_water_blk1_qty,
                            block.2 = volume_water_blk2_qty,
                            p.1 = volume_water_blk1_rate,
                            p.2 = volume_water_blk2_rate,
                            p.3 = volume_water_blk3_rate)
  
  # otherwise, have to add in sewer bill on top of water bill  
  } else {
    
    x <- welfare_difference(qe = dat$avg.30day.use,
                            s  = elasticity_demand,
                            bc = 80000,
                            fc = fixed_water_rate + fixed_sewer_rate,
                            block.1 = volume_water_blk1_qty,
                            block.2 = volume_water_blk2_qty,
                            p.1 = volume_water_blk1_rate + volume_sewer_rate,
                            p.2 = volume_water_blk2_rate + volume_sewer_rate,
                            p.3 = volume_water_blk3_rate + volume_sewer_rate)
    
  }
  
}
