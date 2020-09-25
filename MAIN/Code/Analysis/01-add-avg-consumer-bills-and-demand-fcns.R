



#### This script adds the average water bill for each household, conditional on their sewage disposal type (sewer or on-site system). Then, it estimates a demand function for each household, P = AQ^(1/e)


# load definitions and data
source("MAIN/00-definitions.R")
load("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Cesspools/Data/regression_dat_FINAL.rdata")


# For each consumer, estimate demand curve A value: P = AQ^(1/e) --> A = P/(Q^(1/e))


# create 30-day monthly consumption value
all.sfds$avg.30day.use.1000s <- all.sfds$avg.daily.use * 30 / 1000
all.sfds$avg.30day.use       <- all.sfds$avg.daily.use * 30


# calculate bill (just water, no sewer)
                                    # find block                                                                                       # add appropriate charges for consumption level
all.sfds$total.water.bill <- ifelse(all.sfds$avg.30day.use <= volume_water_blk1_qty,                                                   fixed_water_rate + volume_water_blk1_rate * all.sfds$avg.30day.use,
                                    
                             ifelse(all.sfds$avg.30day.use >  volume_water_blk1_qty & all.sfds$avg.30day.use <= volume_water_blk2_qty, fixed_water_rate + volume_water_blk1_rate * volume_water_blk1_qty +
                                                                                                                                           volume_water_blk2_rate * (all.sfds$avg.30day.use - volume_water_blk1_qty),
                                           
                             ifelse(all.sfds$avg.30day.use >  volume_water_blk2_qty,                                                   fixed_water_rate + volume_water_blk1_rate * volume_water_blk1_qty +
                                                                                                                                           volume_water_blk2_rate * volume_water_blk2_qty +
                                                                                                                                           volume_water_blk3_rate * (all.sfds$avg.30day.use - volume_water_blk2_qty), NA)))


# if household has sewer service, add on the fixed and volumetric charge (BWS calculates sewer bill with 80% of HH's consumption to account for irrigation)
all.sfds$total.water.sewer.bill <- ifelse(all.sfds$no.sewer == 'Sewer', all.sfds$total.water.bill + fixed_sewer_rate + volume_sewer_rate * all.sfds$avg.30day.use * 0.80, all.sfds$total.water.bill)


# calculate average cost per gal
all.sfds$total.water.sewer.bill.avg.per.gal <- all.sfds$total.water.sewer.bill / all.sfds$avg.30day.use


# add demand function parameter `A` in P = AQ^(1/e)
all.sfds$A <- all.sfds$total.water.sewer.bill.avg.per.gal * 1/(all.sfds$avg.30day.use^(1/elasticity_demand))


# get median A by group
avg_A <- aggregate(all.sfds$A, list(all.sfds$no.sewer), median)


# save data
saveRDS(all.sfds, file = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/01-add-avg-consumer-bills-and-demand.rds')
saveRDS(avg_A,    file = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/01b-avg_demand_A_vals.rds')
