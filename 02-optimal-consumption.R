



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
water_use_melt <- data.frame(value    = c(all.sfds$avg.30day.use_optimal, all.sfds$avg.30day.use_optimal_MUC213, all.sfds$avg.30day.use_optimal_MUC426),
                             variable = rep(factor(c('Utility maximizing', 'Socially optimal, MUC = $2.13/tgal', 'Socially optimal, MUC = $4.26/tgal'),
                                                   levels = c('Utility maximizing', 'Socially optimal, MUC = $2.13/tgal', 'Socially optimal, MUC = $4.26/tgal')),
                                            each = nrow(all.sfds)))


# plot distributions
ggplot(data = water_use_melt) + geom_density(aes(value/1000, fill = variable, color = variable), alpha = 0.3) +
  scale_x_continuous(limits = c(0, quantile(water_use_melt$value/1000, na.rm = TRUE, probs = c(0.99)))) +
  labs(x = 'Household consumption (tgal/month)', y = 'Density') +
  theme(legend.title = element_blank(), legend.position = c(0.7,0.8),
        text = element_text(size = 11))

ggsave(filename = 'MAIN/Figures/Images/02-optimal-water-use-distributions.png', height = 5, width = 5, dpi = 300)

