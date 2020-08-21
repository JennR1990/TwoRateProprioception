
# adaptated_two_parameters_one_Process -------------------------------------


oneRateModel <- function(par, schedule) {
  
  # thse values should be zero at the start of the loop:
  Et <- 0 # previous error: none
  Pt <- 0 # state of slow process: aligned
  e<- exp(1)
  
  # we'll store what happens on each trial in these vectors:
  process <- c()
  
  # now we loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    
    # first we calculate what the model does
    # this happens before we get visual feedback about potential errors
    Pt <- (100*e ^ -(1-par['R'])) - ((par['L']/(1-par['R'])) * Et)
    #Pt <- (schedule[t]*e ^ (-(1-par['R']*t))) - ((par['L']/(1-par['R'])) * Et)
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + schedule[t]
    }
    
    # at this point we save the states in our vectors:
    process <- c(process, Pt)
    
  }
  
  # after we loop through all trials, we return the model output:
  return(data.frame(process))
  
}



oneRateMSE <- function(par, schedule, reaches) {
  
  bigError <- mean(schedule^2, na.rm=TRUE) * 10
  
  return( mean((oneRateModel(par, schedule)$process - reaches)^2, na.rm=TRUE) )
  
}

oneRateFit <- function(schedule, reaches, gridpoints=6, gridfits=6) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('L'=parvals,
                            'R'=parvals)
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=oneRateMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=oneRateMSE,
                              method='L-BFGS-B',
                              lower=c(0,0),
                              upper=c(1,1),
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1:2]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optim,
                              fn=oneRateMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}




# one parameter one rate --------------------------------------------------

simpleRateModel <- function(par, schedule) {
  
  # the process and error states are initialized at 0:
  Pt <- 0
  Et <- 0
  
  # the total output is stored here:
  output <- c()
  
  for (t in c(1:length(schedule))) {
    
    Pt <- Pt - (par['LR'] * Et)
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + schedule[t]
    }
    
    # at this point we save the process state in our vector:
    output <- c(output, Pt)
    
  }
  
  return(data.frame(output))
  
}


simpleRateMSE <- function(par, schedule, reaches) {
  
  #bigError <- mean(schedule^2, na.rm=TRUE) * 2
  
  return( mean((simpleRateModel(par, schedule)$output - reaches)^2, na.rm=TRUE) )
  
}

simpleRateFit <- function(schedule, reaches, gridpoints=10, gridfits=4) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('LR'=parvals)
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=simpleRateMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( data.frame('LR'=searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=simpleRateMSE,
                              method='L-BFGS-B',
                              lower=c(0),
                              upper=c(1),
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( data.frame('LR'=searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optim,
                              fn=simpleRateMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}
