
twoRateNCModel <- function(par, schedule) {
  
  # thse values should be zero at the start of the loop:
  Et <- 0 # previous error: none
  St <- 0 # state of the slow process: aligned
  Ft <- 0 # state of the fast process: aligned
  
  # we'll store what happens on each trial in these vectors:
  slow <- c()
  fast <- c()
  total <- c()
  
  # now we loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    
    # first we calculate what the model does
    # this happens before we get visual feedback about potential errors
    St <- (par['Rs'] * St) - (par['Ls'] * Et)
    Ft <- (par['Rf'] * Ft) - (par['Lf'] * Et)
    Xt <- St + Ft
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Xt + schedule[t]
    }
    
    # at this point we save the states in our vectors:
    slow <- c(slow, St)
    fast <- c(fast, Ft)
    total <- c(total, Xt)
    
  }
  
  # after we loop through all trials, we return the model output:
  return(data.frame(slow,fast,total))
  
}

twoRateNCMSE <- function(par, schedule, reaches) {
  
  bigError <- mean(schedule^2, na.rm=TRUE) * 10
  
  # learning and retention rates of the fast and slow process are constrained:
  if (par['Ls'] > par['Lf']) {
    return(bigError)
  }
  if (par['Rs'] < par['Rf']) {
    return(bigError)
  }
  
  return( mean((twoRateNCModel(par, schedule)$slow - reaches)^2, na.rm=TRUE) )
  
}

twoRateNCFit <- function(schedule, reaches, gridpoints=6, gridfits=6) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('Ls'=parvals,
                            'Lf'=parvals,
                            'Rs'=parvals,
                            'Rf'=parvals)
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=twoRateNCMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=twoRateNCMSE,
                              method='L-BFGS-B',
                              lower=c(0,0,0,0),
                              upper=c(1,1,1,1),
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1:4]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=stats::optim,
                              fn=twoRateNCMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}






ncpars<-Reachmodelnc(newnocursor_reaches, newnocursor_nocursors, 'No-Cursor', color = colorNC)
slowpars<-Reachmodelslownc(newnocursor_reaches, newnocursor_nocursors, 'No-Cursor', color = colorNC)

reaches<-getreachesformodel(newnocursor_reaches)
nocursors<-getreachesformodel(newnocursor_nocursors)
model1MSE<-twoRateReachModelErrors(ncpars, reaches = reaches$meanreaches, schedule = reaches$distortion)
model2MSE<-twoRateNCMSE(slowpars, reaches = reaches$meanreaches, schedule = reaches$distortion)


#preparing the AIC stuff
N <- dim(df)[2] - 1
# the median length of a phase is 40 trials,
# and there are 7.2 of those in 288 trials
InOb <- seriesEffectiveSampleSize(reaches$meanreaches, method='ac_lag95%CI')
print(InOb)

twoRateAIC<-(InOb * log(model1MSE)) + (2 * 4)
oneRateAIC<-(InOb * log(model2MSE)) + (2 * 4)
