



### This script is used to calculate the demand curve given price elasticity of demand

# assume the form P = AQ^(1/e) to estimate A for each household

# A = P / Q^(1/e)



                    #P                 Q                e
demand_A <- function(bill_avg_per_gal, avg_monthly_gal, elasticity){ bill_avg_per_gal * 1/(avg_monthly_gal^(1/elasticity)) }