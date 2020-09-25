



#### This script creates "Images/01-average-vs-marginal-price.png"


# load packages
library(ggplot2)


# load default parameters
source("MAIN/00-definitions.R")




##### add demand curves #####


# load average demand parameters and base plot
avg_A     <- readRDS('C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/01b-avg_demand_A_vals.rds')
PricePlot <- readRDS('MAIN/Figures/Images/01-average-vs-marginal-price.rds')


# create average demand curve for each consumer type: P = A*Q^(1/e)
demand_curves         <- data.frame(x = seq(5000, 20000, by = 1))
demand_curves$y_osds  <- avg_A[1,2] * demand_curves$x ^ (1/elasticity_demand)
demand_curves$y_sewer <- avg_A[2,2] * demand_curves$x ^ (1/elasticity_demand)


# plot - add dark red line for avg OSDS demand, dark blue for average sewer demand
PricePlot <- PricePlot + geom_line(data = demand_curves,  aes(x = demand_curves$x, y = demand_curves$y_osds),  size = 0.8, color = 'red') +
                         geom_line(data = demand_curves,  aes(x = demand_curves$x, y = demand_curves$y_sewer), size = 0.8, color = 'blue')

ggsave(PricePlot, filename = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/WaterMarginalAveragePrice/MAIN/Figures/Images/02a-average-vs-marginal-price-with-demand.png',
       dpi = 300, height = 4, width = 7)





##### add MUC curves #####


# load costs data.frame
costs_df <- readRDS('C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/01PLOT-cost-dataframe.rds')


# add MUC to `costs_df`
costs_df$MUC                        <- MUC
costs_df$water_total_w_MUC          <- costs_df$water_total          + MUC
costs_df$water_sewer_total_w_MUC    <- costs_df$water_sewer_total    + MUC
costs_df$water_marginal_w_MUC       <- costs_df$water_marginal       + MUC
costs_df$water_sewer_marginal_w_MUC <- costs_df$water_sewer_marginal + MUC


# melt the MUC data.frame
costs_df_melt <- data.frame(value    = c(costs_df$water_marginal, costs_df$water_sewer_marginal,
                                                         costs_df$water_marginal_w_MUC, costs_df$water_sewer_marginal_w_MUC,
                                                         costs_df$water_average,  costs_df$water_sewer_average),
                                            variable = c(rep('Water', times = nrow(costs_df)), rep('Water and sewer', times = nrow(costs_df)),
                                                         rep('Water', times = nrow(costs_df)), rep('Water and sewer', times = nrow(costs_df)),
                                                         rep('Water', times = nrow(costs_df)), rep('Water and sewer', times = nrow(costs_df))),
                                            type     = c(rep('Marginal', times = 2*nrow(costs_df)),
                                                         rep('Marginal plus MUC', times = 2*nrow(costs_df)),
                                                         rep('Average', times = 2*nrow(costs_df))),
                                            x        = rep(costs_df$gal1000, times = 6))


# remake plot
PricePlot2 <- ggplot() + geom_line(data = costs_df_melt, aes(x = x, y = value, color = variable, linetype = type), size = 0.8) + labs(x = 'Consumption (1000s gal)', y = 'Price ($/1000 gal)') +
  
  # MUC curves
  scale_color_discrete(name = 'Service type') + scale_linetype_manual(name = 'Price type', values = c('dotted', 'solid', 'twodash')) +
  scale_x_continuous(breaks = c(0,10000,20000,30000,40000), labels = c(0,10,20,30,40))+
  scale_y_continuous(breaks = seq(0,25,by=5)/1000, labels = seq(0,25,by=5), limits = c(0,.025)) +
  
  # demand curves
  geom_line(data = NULL,  aes(x = seq(5000,20000,by=1), y = avg_A[1,2]*seq(5000,20000,by=1)^(1/elasticity_demand)), size = 0.8, color = 'red') +
  geom_line(data = NULL,  aes(x = seq(5000,20000,by=1), y = avg_A[2,2]*seq(5000,20000,by=1)^(1/elasticity_demand)), size = 0.8, color = 'blue')

ggsave(PricePlot2,
       filename = 'C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/WaterMarginalAveragePrice/MAIN/Figures/Images/02b-average-vs-marginal-price-with-demand-and-MUC.png',
       dpi = 300, height = 4, width = 7)
