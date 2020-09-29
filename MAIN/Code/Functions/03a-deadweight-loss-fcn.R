



# given the actual consumption and optimal consumption of the household, calculate its deadweight loss ($/month)


deadweight_loss <- function(q_actual, q_optimal, A, no.sewer = 'Other'){
  
  
  
  # get integral from demand curve
  top_curve <- ifelse(is.na(A) | is.na(q_actual) | is.na(q_optimal),
                      NA,
                      integrate(f     = function(x){ A*x^(1/elasticity_demand) },
                                lower = q_actual,
                                upper = q_optimal))
  
  
  
  # HH water bill - actual and optimal BOTH in FIRST block
  if(q_actual >= 0 & q_actual < volume_water_blk1_qty & q_optimal >=0 & q_optimal < volume_water_blk1_qty){
    
    # get integral from water bill
    bottom_curve <- abs(q_optimal - q_actual) * volume_water_blk1_rate
    
    # get household DWL, which depends on whether the HH also pays a sewer bill
    hh_dwl <- ifelse(no.sewer == 'Other',
                     top_curve - bottom_curve,
                     ifelse(no.sewer == 'Sewer',
                            abs(top_curve - bottom_curve - abs(q_optimal - q_actual)*volume_sewer_rate)),
                     NA)
    
    return(hh_dwl)
  
    
    
  # HH water bill - actual and optimal BOTH in SECOND block    
  } else if(q_actual >= volume_water_blk2_qty & q_actual < volume_water_blk3_qty & q_optimal >= volume_water_blk2_qty & q_optimal < volume_water_blk3_qty) {
    
    # get integral from water bill
    bottom_curve <- abs(q_optimal - q_actual) * volume_water_blk2_rate
    
    # get household DWL, which depends on whether the HH also pays a sewer bill
    hh_dwl <- ifelse(no.sewer == 'Other',
                     top_curve - bottom_curve,
                     ifelse(no.sewer == 'Sewer',
                            abs(top_curve - bottom_curve - abs(q_optimal - q_actual)*volume_sewer_rate)),
                     NA)
    
    return(hh_dwl)
   
  
    
  # HH water bill - actual and optimal BOTH in THIRD block   
  } else if(q_actual >= volume_water_blk3_qty & q_optimal >= volume_water_blk3_qty) {
    
    # get integral from water bill
    bottom_curve <- abs(q_optimal - q_actual) * volume_water_blk3_rate
    
    # get household DWL, which depends on whether the HH also pays a sewer bill
    hh_dwl <- ifelse(no.sewer == 'Other',
                     top_curve - bottom_curve,
                     ifelse(no.sewer == 'Sewer',
                            abs(top_curve - bottom_curve - abs(q_optimal - q_actual)*volume_sewer_rate)),
                     NA)
    
    return(hh_dwl)
    
    
    
  # HH water bill - actual in FIRST, optimal in SECOND  
  } else if(q_actual < volume_water_blk1_qty & q_optimal >= volume_water_blk2_qty & q_optimal) {
    
    # get integral from water bill
    bottom_curve <- (abs(volume_water_blk1_qty - q_actual) * volume_water_blk1_rate) + (abs(q_optimal - volume_water_blk1_qty) * volume_water_blk2_rate)
    
    # get household DWL, which depends on whether the HH also pays a sewer bill
    hh_dwl <- ifelse(no.sewer == 'Other',
                     top_curve - bottom_curve,
                     ifelse(no.sewer == 'Sewer',
                            abs(top_curve - bottom_curve - abs(q_optimal - q_actual)*volume_sewer_rate)),
                     NA)
    
    return(hh_dwl)
    
  
    
  # HH water bill - actual in SECOND, optimal in THIRD  
  } else if(q_actual < volume_water_blk2_qty & q_optimal >= volume_water_blk2_qty) {
    
    # get integral from water bill
    bottom_curve <- (abs(volume_water_blk2_qty - q_actual) * volume_water_blk2_rate) + (abs(q_optimal - volume_water_blk3_qty) * volume_water_blk3_rate)
    
    # get household DWL, which depends on whether the HH also pays a sewer bill
    hh_dwl <- ifelse(no.sewer == 'Other',
                     top_curve - bottom_curve,
                     ifelse(no.sewer == 'Sewer',
                            abs(top_curve - bottom_curve - abs(q_optimal - q_actual)*volume_sewer_rate)),
                     NA)
    
    return(hh_dwl)
    
    
    
  # catch-all for NA, etc.  
  } else {
    
    return(NA)
    
  }
  
  
}