



### This script creates the function used to calculate optimal consumption using consumer demand curves and water prices


# load packages
library(doParallel)
registerDoParallel(cores = 5)


# load definitions and data
source("MAIN/00-definitions.R")


# create function that finds when the demand curve of a house intersects a given value/MC - we subtract the block rate so we can use the uniroot function to find where the demand equals 0
  # A is the calculated demand function coefficient
  # Qobs is the observed quantity consumed, in 1000s of gallons
  # blk_rate is the block rate currently being tested

Q_intercept_fcn <- function(A, Qobs, blk_rate){A*Qobs^(1/elasticity_demand) - blk_rate}



# create lazy loop function to calculate optimal consumption
  # data: data.frame. The main dataset
  # A:    vector. The estimated household demand function parameters: P=AQ^{1/e}
  # MUC:  numerical. value of the marginal user cost. Value of 0 calculates current-day utility-maximizing quantity

optimal_consumption <- function(data, A, MUC = 0){
  
  x <- list()
                                                                        
  for(i in 1:nrow(data)){
    
    x[[i]] <- 
    
    # if household doesn't have a sewer, calculate consumption based only on marginal water rate
    if(data$no.sewer[[i]] == 'Other'){
    
      # optimal consumption in the first block?
      ifelse(!(is(try(uniroot(f = Q_intercept_fcn, interval = c(                    1, volume_water_blk1_qty-0.01), A = A[[i]], blk_rate = volume_water_blk1_rate + MUC), silent = TRUE), 'try-error')),
                      uniroot(f = Q_intercept_fcn, interval = c(                    1, volume_water_blk1_qty-0.01), A = A[[i]], blk_rate = volume_water_blk1_rate + MUC)[[1]],
      
      # optimal consumption in the second block?     
      ifelse(!(is(try(uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk1_qty, volume_water_blk2_qty-0.01), A = A[[i]], blk_rate = volume_water_blk2_rate + MUC), silent = TRUE), 'try-error')),
                      uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk1_qty, volume_water_blk2_qty-0.01), A = A[[i]], blk_rate = volume_water_blk2_rate + MUC)[[1]],
       
      # optimal consumption in the third block?             
      ifelse(!(is(try(uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk2_qty, 7700000),                    A = A[[i]], blk_rate = volume_water_blk3_rate + MUC), silent = TRUE), 'try-error')),
                      uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk2_qty, 7700000),                    A = A[[i]], blk_rate = volume_water_blk3_rate + MUC)[[1]], NA)))
      
    
    # if household has a sewer connection, calculate consumption based on marginal water rate and marginal sewer rate
    } else {
        
      # optimal consumption in the first block?
      ifelse(!(is(try(uniroot(f = Q_intercept_fcn, interval = c(                    1, volume_water_blk1_qty-0.01), A = A[[i]], blk_rate = volume_water_blk1_rate + volume_sewer_rate + MUC), silent = TRUE), 'try-error')),
                      uniroot(f = Q_intercept_fcn, interval = c(                    1, volume_water_blk1_qty-0.01), A = A[[i]], blk_rate = volume_water_blk1_rate + volume_sewer_rate + MUC)[[1]],
             
      # optimal consumption in the second block?     
      ifelse(!(is(try(uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk1_qty, volume_water_blk2_qty-0.01), A = A[[i]], blk_rate = volume_water_blk2_rate + volume_sewer_rate + MUC), silent = TRUE), 'try-error')),
                      uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk1_qty, volume_water_blk2_qty-0.01), A = A[[i]], blk_rate = volume_water_blk2_rate + volume_sewer_rate + MUC)[[1]],
                    
      # optimal consumption in the third block?             
      ifelse(!(is(try(uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk2_qty, 7700000),                    A = A[[i]], blk_rate = volume_water_blk3_rate + volume_sewer_rate + MUC), silent = TRUE), 'try-error')),
                      uniroot(f = Q_intercept_fcn, interval = c(volume_water_blk2_qty, 7700000),                    A = A[[i]], blk_rate = volume_water_blk3_rate + volume_sewer_rate + MUC)[[1]], NA)))
      
      
    }
    
  }
  
  return(unlist(x))
    
}
