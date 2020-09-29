



### This script takes the differences between actual and optimal water consumption and calculates the associated deadweight loss for each household


# load packages
library(ggplot2)


# load definitions
source("MAIN/00-definitions.R")


# load optimal consumption function
source("MAIN/Code/Functions/03a-deadweight-loss-fcn.R")


# load data
all.sfds <- readRDS("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/02-optimal-consumption.rds")




### calculate deadweight loss

# DWL - actual vs. optimal w/ no MUC
all.sfds$DWL_optimal <- NA
for(i in 1:nrow(all.sfds)){all.sfds$DWL_optimal[[i]]        <- deadweight_loss(q_actual = all.sfds$avg.30day.use[[i]], q_optimal = all.sfds$avg.30day.use_optimal[[i]],
                                                                               A = all.sfds$A[[i]], no.sewer = all.sfds$no.sewer[[i]])}

# DWL - actual vs. optimal w/ MUC = 2.13/tgal
all.sfds$DWL_optimal_MUC213 <- NA
for(i in 1:nrow(all.sfds)){all.sfds$DWL_optimal_MUC213[[i]] <- deadweight_loss(q_actual = all.sfds$avg.30day.use[[i]], q_optimal = all.sfds$avg.30day.use_optimal_MUC213[[i]],
                                                                               A = all.sfds$A[[i]], no.sewer = all.sfds$no.sewer[[i]])}

# DWL - actual vs. optimal w/ MUC = 4.26/tgal
all.sfds$DWL_optimal_MUC213 <- NA
for(i in 1:nrow(all.sfds)){all.sfds$DWL_optimal_MUC426[[i]] <- deadweight_loss(q_actual = all.sfds$avg.30day.use[[i]], q_optimal = all.sfds$avg.30day.use_optimal_MUC426[[i]],
                                                                               A = all.sfds$A[[i]], no.sewer = all.sfds$no.sewer[[i]])}





# save data
saveRDS(all.sfds, file = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/03-deadweight-loss.rds')
