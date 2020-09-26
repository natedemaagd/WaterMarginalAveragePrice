# WaterMarginalAveragePrice
Analysis of Oahu households' responses to average and marginal price, and welfare implications

* MAIN folder - contains scripts and figures
  * Code
    * Analysis - `R` scripts for the main analysis
    * Functions - `R` scripts called by certain files in the `Analysis` folder
  * Figures
    * Code - `R` scripts to make standalone figures (i.e. billing data aren't needed)
    * Images - Contains PNGs of figures included in the paper, created by the `Code` scripts
      * Also contains `*.rds` files used to create some images from existing `ggplot` objects
  * The `00-definitions.R` file specifies the main (default) parameters used to run the code and create figures. These values may be changed in individual files to make comparisons (e.g. smaller vs. larger elasticity values, changes to MUC, etc.).
