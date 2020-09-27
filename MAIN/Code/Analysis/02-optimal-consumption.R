



### This script takes the estimated demand function from each household, P = AQ^{1/e}, to determine utility-maximizing consumption and socially-optimal consumption


# load packages
library(ggplot2)


# load definitions
source("MAIN/00-definitions.R")


# load optimal consumption function
source("MAIN/Code/Functions/02a-uniroot-optimal-consumption-fcn.R")


# load data
all.sfds <- readRDS('C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/01-add-avg-consumer-bills-and-demand.rds')
avg_A    <- readRDS('C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Water/Marginal vs average price welfare/Data/Intermediate/01b-avg_demand_A_vals.rds')




##### calculate optimal consumption #####


# present-dat utility-maximizing consumption (no MUC)
all.sfds$avg.30day.use_optimal        <- optimal_consumption(data = all.sfds, A = all.sfds$A, MUC = 0)


# socially-optimal consumption (include MUC)
all.sfds$avg.30day.use_optimal_MUC213 <- optimal_consumption(data = all.sfds, A = all.sfds$A, MUC = MUC)


# double marginal user cost to analyze sensitivity
all.sfds$avg.30day.use_optimal_MUC426 <- optimal_consumption(data = all.sfds, A = all.sfds$A, MUC = MUC*2)




##### plot distributions #####


# melt data
water_use_melt <- data.frame(value    = c(all.sfds$avg.30day.use, all.sfds$avg.30day.use_optimal, all.sfds$avg.30day.use_optimal_MUC213, all.sfds$avg.30day.use_optimal_MUC426),
                             variable = rep(factor(c('Current use', 'Present-day\nutility maximizing', 'Socially optimal,\nMUC = $2.13/tgal', 'Socially optimal,\nMUC = $4.26/tgal'),
                                                   levels = c('Current use', 'Present-day\nutility maximizing', 'Socially optimal,\nMUC = $2.13/tgal', 'Socially optimal,\nMUC = $4.26/tgal')),
                                            each = nrow(all.sfds)))


# plot distributions of water use: current, 
ggplot(data = water_use_melt) + geom_boxplot(aes(x = variable, y = value/1000),
                                             outlier.shape = NA) +
  
  # change axis labels
  labs(x = 'Consumption type', y = 'Household consumption (tgal/month)') +
  
  # adjust plot appearance
  theme(legend.title = element_blank(), text = element_text(size = 15),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank()) +
  
  # adjust y scale to omit outliers
  coord_cartesian(ylim = quantile(water_use_melt$value/1000, c(0, 0.95), na.rm = TRUE)) +
  
  # add median use to median bar in boxplots
  annotate("text", x = 1:4,
           y     = c(      median(water_use_melt[water_use_melt$variable == 'Current use',                        1], na.rm = TRUE)/1000+0.5,
                           median(water_use_melt[water_use_melt$variable == 'Present-day\nutility maximizing',                 1], na.rm = TRUE)/1000+0.5,
                           median(water_use_melt[water_use_melt$variable == 'Socially optimal,\nMUC = $2.13/tgal', 1], na.rm = TRUE)/1000+0.5,
                           median(water_use_melt[water_use_melt$variable == 'Socially optimal,\nMUC = $4.26/tgal', 1], na.rm = TRUE)/1000+0.5),
           
           label = c(format(round(median(water_use_melt[water_use_melt$variable == 'Current use',                        1], na.rm = TRUE)/1000, 2), nsmall = 2),
                     format(round(median(water_use_melt[water_use_melt$variable == 'Present-day\nutility maximizing',                 1], na.rm = TRUE)/1000, 2), nsmall = 2),
                     format(round(median(water_use_melt[water_use_melt$variable == 'Socially optimal,\nMUC = $2.13/tgal', 1], na.rm = TRUE)/1000, 2), nsmall = 2),
                     format(round(median(water_use_melt[water_use_melt$variable == 'Socially optimal,\nMUC = $4.26/tgal', 1], na.rm = TRUE)/1000, 2), nsmall = 2)))

ggsave(filename = 'MAIN/Figures/Images/02-optimal-water-use-distributions.png', height = 5, width = 7, dpi = 300)

