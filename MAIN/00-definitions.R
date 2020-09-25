

# This file defines water prices, quantity cutoffs, and price elasticity of demand. It is imported into the individual figure files to create the images.



# define elasticity
elasticity_demand <- -0.02


# define marginal user cost (MUC) in dollars per gallon - default is 2.13/1000
MUC <- 2.13 / 1000


# water prices and their cutoffs - PER GALLON
fixed_water_rate       <- 9.26
volume_water_blk1_rate <- 4.42 / 1000
volume_water_blk2_rate <- 5.33 / 1000
volume_water_blk3_rate <- 7.94 / 1000

volume_water_blk1_qty  <- 13000
volume_water_blk2_qty  <- 30000


# sewer prices
fixed_sewer_rate  <- 77.55
volume_sewer_rate <- 4.63 / 1000