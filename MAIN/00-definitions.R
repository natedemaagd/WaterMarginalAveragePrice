

# This file defines water prices, quantity cutoffs, and price elasticity of demand. It is imported into the individual figure files to create the images.



# define elasticity - default is -0.06
elasticity_demand <- -0.06


# define marginal user cost (MUC) in dollars per gallon - default is 2.13/1000
MUC <- 2.13 / 1000


# water prices - PER GALLON
  # defaults:
    # fixed charge: 9.26/month
    # block 1 volumetric charge: 4.42 per 1000 gallons
    # block 2 volumetric charge: 5.33 per 1000 gallons
    # block 3 volumetric charge: 7.94 per 1000 gallons
    # block 1 volume cutoff: 13,000 gallons
    # block 2 volume cutoff: 30,000 gallons
fixed_water_rate       <- 9.26
volume_water_blk1_rate <- 4.42 / 1000
volume_water_blk2_rate <- 5.33 / 1000
volume_water_blk3_rate <- 7.94 / 1000


# water volumetric cutoffs
  # defaults:
    # block 1 volume cutoff: 13,000 gallons
    # block 2 volume cutoff: 30,000 gallons
volume_water_blk1_qty  <- 13000
volume_water_blk2_qty  <- 30000


# sewer prices
  # defaults:
    # fixed charge: 77.55/month
    # volumetric charge: 4.63 per 1000 gallons
fixed_sewer_rate  <- 77.55
volume_sewer_rate <- 4.63 / 1000