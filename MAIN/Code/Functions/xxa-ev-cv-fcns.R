


# given actual quantity consumed, elasticity, budget, and block pricing info, calculate optimal consumption level and associated CV and EV
welfare_difference <- function(qe,       # observed quantity consumed
                               s,        # elasticity
                               bc,       # budget constraint
                               fc,       # fixed charge
                               block.1,  # top of first block
                               block.2,  # top of second block
                               p.1,      # price per unit first block
                               p.2,      # price per unit second block
                               p.3,      # price per unit third block)
                               plot_data = FALSE){    # do you want to also return the points for plotting budget constraint and util function?
  
  
  # given current utility consumption, calculate consumption of all other goods (1 unit = $1)
  qaog <- function(qe){
    x <- rep(0, times = length(qe))                                                                              # initial vector
    x[qe == 0] <- bc                                                                                             # if no electricity, all of budget goes to other goods
    x[qe > 0 & qe < block.1] <- bc - fc - p.1*qe[qe > 0 & qe < block.1]                                          # consumption in the first block
    x[qe >=block.1 & qe < block.2]      <- bc - fc - p.1*block.1 - p.2*(qe[qe >=block.1 & qe<block.2]-block.1)   # consumption in the second block
    x[qe >=block.2] = bc - fc - p.1*block.1 - p.2*(block.2-block.1) - p.3*(qe[qe >=block.2]-block.2)             # consumption in the third block
    x
  }
  
  qo <- qaog(qe)
  
  
  # given budget, what's the max quantity HH can consume?
  qzero <- uniroot(f = qaog, interval = c(0,1e10))$root
  
  
  # series of points
  zed <- seq(0, qzero, length = 1000)
  
  
  # quantity of all other goods at each point from 0 to max quantity
  joe <- qaog(zed)
  
  
  # average price function
  ave.p <- function(qe){ (bc - qaog(qe)) / qe }
  
  
  # perceived budget constraint - flat average price curve through observed choice
  intercepts <- function(choice){   # first get intercepts of perceived budget constraint
    
    ap   = ave.p(choice)
    y    = qaog(choice)
    yint = y + ap*choice
    xint = yint / ap
    return(c(yint, xint))
    
  }
  
  intercepts_choice <- intercepts(qe)
  
  
  # CES function
  ces <- function(choice, s){
    # utility = (a*qaog^r + (1-a)*qe^r)^1/r
    #  where r = (s-1)/s and s is the elasticity of substitution
    #  price of aog (all other goods) = 1
    r = (s-1)/s
    qo = qaog(choice)
    ap = ave.p(choice)
    # find parameter 'a' from implied MRS = (a/(1-a))(qo/qe)^(r-1) = 1/p
    a = uniroot( function(x) 1/ap - (x/(1-x))*(qo/choice)^(r-1), c(0,1) )$root
    u0 = (a*qo^r + (1-a)*choice^r)^(1/r)
    qoUbar = function(qe) ( (1/a)*u0^r - ((1-a)/a)*qe^r )^(1/r)
    # consider range of qe around choice, say +/- 20%
    qe.range = seq(choice-(choice*1.2), choice+(choice*1.2), length=200)
    qo.range = rep(0, 200)
    for(i in 1:200) qo.range[i] = qoUbar(qe.range[i])
    
    
    # create data.frame to store utility function points
    CES_points <- data.frame(qe.range = qe.range,
                             qo.range = qo.range)
    
    CES_points
    
  }
  
  
  # run CES function
  ces_points <- ces(qe, s)
  
  
  # define utility function
  utility = function(qe, s, choice){
    r = (s-1)/s
    qo = qaog(choice) # to calibrate budget constraint
    ap = ave.p(choice)
    a = uniroot( function(x) 1/ap - (x/(1-x))*(qo/choice)^(r-1), c(0,1) )$root
    qo = qaog(qe)  # to calculate utility for arbitrary qe
    u = (a*qo^r + (1-a)*qe^r)^1/r
    u
  }
  
  
  # find util-maximizing quantity of electricity within a range +/- 20% of actual consumption
  qe.opt = optimize(utility, c(qe-(qe*1.2),qe+(qe*1.2)), s=s, choice=qe, maximum=T)$maximum
  
  
  # add second CES curve
  ces2 = function(qe, choice, s) {
    # utility = (a*qaog^r + (1-a)*qe^r)^1/r 
    #  where r = (s-1)/s and s is the elasticity of substitution
    #  price of aog (all other goods) = 1
    r = (s-1)/s
    qo.choice = qaog(choice)
    ap = ave.p(choice)
    a = uniroot( function(x) 1/ap - (x/(1-x))*(qo.choice/choice)^(r-1), c(0,1) )$root
    qo.qe = qaog(qe)
    u0 = (a*qo.qe^r + (1-a)*qe^r)^(1/r)
    qoUbar = function(qe) ( (1/a)*u0^r - ((1-a)/a)*qe^r )^(1/r)
    qe.range = seq(choice-(choice*1.2), choice+(choice*1.2), length=200)
    qo.range = rep(0, 200)
    for(i in 1:200) qo.range[i] = qoUbar(qe.range[i])
    
    # create data.frame to store utility function points
    CES2_points <- data.frame(qe.range = qe.range,
                              qo.range = qo.range)
    
    CES2_points
    
  }
  
  
  ces2_points <- ces2(qe = qe.opt, s = s, choice = qe)
  
  
  # calculate EV and CV of sub-optimal choice
  wf = function(choice, s){
    r = (s-1)/s
    qo.choice = qaog(choice)
    ap = ave.p(choice)
    a = uniroot( function(x) 1/ap - (x/(1-x))*(qo.choice/choice)^(r-1), c(0,1) )$root
    qe.opt = optimize(utility, choice+c(-10,10), s=s, choice=choice, maximum=T)$maximum
    qo.opt = qaog(qe.opt)
    u0 = (a*qo.choice^r + (1-a)*choice^r)^(1/r)
    u1 = (a*qo.opt^r + (1-a)*qe.opt^r)^(1/r)
    qoUbar0 = function(qe) ( (1/a)*u0^r - ((1-a)/a)*qe^r )^(1/r)
    qoUbar1 = function(qe) ( (1/a)*u1^r - ((1-a)/a)*qe^r )^(1/r)
    ev = qoUbar0(choice) - qoUbar1(choice)
    cv = qoUbar0(qe.opt) - qoUbar1(qe.opt)
    return( c(ev, cv))
  }
  
  
  welfare <- wf(choice = qe, s = s)
  
  
  # output data - if they want plot data, return a list with two elements:
      # 1) a vector with the current and optimal consumption values, along with EV and CV
      # 2) all data needed to create the plot (a data.frame)
    # else, just return the vector
  if(isTRUE(plot_data)){
    
    final_vector <- c(qe, s, bc, qe.opt, welfare)
    names(final_vector) <- c('Actual consumption', 'Elasticity', 'Budget constraint', 'Util-maximizing consumption', 'EV', 'CV')
    
    plot_data <- list(zed, joe, intercepts_choice, ces_points, ces2_points)
    names(plot_data) <- c('Q_demand_range', 'Q_all_other_goods', 'avg_price_intercepts', 'utility_observed_consumption', 'utility_optimal_consumption')
    
    final_data <- list(final_vector, plot_data)
    
  } else {
    
    final_data <- c(qe, s, bc, qe.opt, welfare)
    names(final_data) <- c('Actual consumption', 'Elasticity', 'Budget constraint', 'Util-maximizing consumption', 'EV', 'CV')
    
  }
  
  return(final_data)
  
}


# example
x <- welfare_difference(qe = 25, s = 0.2,  bc = 100, fc = 20, block.1 = 10, block.2 = 30, p.1 = 1.5, p.2 = 2.5, p.3 = 4, plot_data = TRUE)

# example with our data
y <- welfare_difference(qe = 14, s = 0.06, bc = 80000, fc = 9.26, block.1 = 13, block.2 = 30, p.1 = 4.42, p.2 = 5.33, p.3 = 7.94, plot_data = TRUE)
