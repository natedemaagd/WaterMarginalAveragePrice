



#### This script creates "Images/01-average-vs-marginal-price.png"


# load packages
library(ggplot2); library(doParallel)
registerDoParallel(cores = 5)


# load default parameters
source("MAIN/00-definitions.R")


# create a figure cutoff point beyond the last consumption block
volume_figure_cutoff <- 40000


# create `data.frame` of costs per gallon
costs_df <- data.frame(gal1000 = c(1:volume_water_blk1_qty, volume_water_blk1_qty:volume_water_blk2_qty, volume_water_blk2_qty:volume_figure_cutoff))





##### calculate water costs for customers without sewer connections


# marginal
costs_df$water_marginal <- ifelse(costs_df$gal1000 <= volume_water_blk1_qty,                                            volume_water_blk1_rate,
                           ifelse(costs_df$gal1000 > volume_water_blk1_qty & costs_df$gal1000 <= volume_water_blk2_qty, volume_water_blk2_rate,
                           ifelse(costs_df$gal1000 > volume_water_blk2_qty,                                             volume_water_blk3_rate, NA)))


# adjust values at kinks so there's a vertical line
costs_df$water_marginal[[volume_water_blk1_qty + 1]] <- volume_water_blk2_rate
costs_df$water_marginal[[volume_water_blk2_qty + 1]] <- volume_water_blk3_rate   


# total and average
costs_df$water_total   <- foreach(i = 1:nrow(costs_df), .combine = 'c') %dopar% {  sum(costs_df$water_marginal[1:i]) }   # get total of marginal component for each quantity
costs_df$water_average <- foreach(i = 1:nrow(costs_df), .combine = 'c') %dopar% { mean(costs_df$water_marginal[1:i]) }   # get average cost of quantity up to that point





##### calculate water costs for customers WITH sewer connections


# marginal
costs_df$water_sewer_marginal <- costs_df$water_marginal + volume_sewer_rate


# total and average
costs_df$water_sewer_total   <- foreach(i = 1:nrow(costs_df), .combine = 'c') %dopar% {  sum(costs_df$water_sewer_marginal[1:i]) }
costs_df$water_sewer_average <- foreach(i = 1:nrow(costs_df), .combine = 'c') %dopar% { mean(costs_df$water_sewer_marginal[1:i]) }





##### add fixed charges #####


# add fixed charges
costs_df$water_total       <- costs_df$water_total       + fixed_water_rate
costs_df$water_sewer_total <- costs_df$water_sewer_total + fixed_water_rate + fixed_sewer_rate


# adjust average cost
costs_df$water_average       <- costs_df$water_total       / costs_df$gal1000
costs_df$water_sewer_average <- costs_df$water_sewer_total / costs_df$gal1000


# melt the `data.frame`
costs_df_melt <- data.frame(value    = c(costs_df$water_marginal, costs_df$water_sewer_marginal,
                                                 costs_df$water_average,  costs_df$water_sewer_average),
                                    variable = c(rep('Water', times = nrow(costs_df)), rep('Water and sewer', times = nrow(costs_df)),
                                                 rep('Water', times = nrow(costs_df)), rep('Water and sewer', times = nrow(costs_df))),
                                    type     = c(rep('Marginal', times = 2*nrow(costs_df)), rep('Average', times = 2*nrow(costs_df))),
                                    x        = rep(costs_df$gal1000, times = 4))


# plot
ggplot(data = costs_df_melt) + geom_line(aes(x = x, y = value, color = variable, linetype = type), size = 0.8) + labs(x = 'Consumption (1000s gal)', y = 'Price ($/1000 gal)') +
  
  scale_color_discrete(name = 'Service type') + scale_linetype_manual(name = 'Price type', values = c('dotted', 'solid')) +
  scale_x_continuous(breaks = c(0,10000,20000,30000,40000), labels = c(0,10,20,30,40))+
  scale_y_continuous(breaks = seq(0,25,by=5)/1000, labels = seq(0,25,by=5), limits = c(0,.025))

ggsave(filename = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/WaterMarginalAveragePrice/MAIN/Figures/Images/01-average-vs-marginal-price.png', dpi = 300, height = 4, width = 7)
