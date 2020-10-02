


# Household demand is assumed to take the form P = AQ^(1/e).
# Given a price per gallon, what is the demand? Q = (P/A)^e


# parameters
  # A                 = demand function parameter from equation above
  # price_water       = price per gallon of water, homes without sewer service
  # price_water_sewer = price per gallon of water, homes WITH sewer service
  # elasticity        = price elasticity of water demand
  # no.sewer          = does the household have a sewer connection? Either 'Sewer', or 'Other'


demand <- function(A, price_water, price_water_sewer = NA, elasticity, no.sewer){
  
  
  # calculate demand based on whether household has a sewer connection; return NA if connection status or price is missing
  return(ifelse(!is.na(price_water_sewer) & no.sewer == 'Sewer',
                (price_water_sewer / A) ^ elasticity,
         ifelse(no.sewer == 'Other',
                (price_water / A) ^ elasticity,
         NA)))
  
}
