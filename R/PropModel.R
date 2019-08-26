getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  schedule<- data$distortion
  return(data.frame(meanreaches,schedule))
}

PropModel <- function(par, schedule) {
  locest<-c()
  #loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    # first we calculate what the model does, since the model is proportional, we just multiply the one parameters by the schedule to get what the person should do
    
    locest[t] <- par * schedule[t]
  }
  
  # after we loop through all trials, we return the model output:
  return(locest)
  
}

PropModelMSE <- function(par, schedule, localizations) {
  
  locesti<- PropModel(par, schedule)
  errors <- locesti - localizations
  MSE <- mean(errors^2, na.rm=TRUE)
  
  
  
  return( MSE )
  
}



PropModelFit <- function(reachdata, locadata,  gridpoints=6, gridfits=6) {
  
  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  meanreaches<-rowMeans(reachdata[241:288,2:ncol(reachdata)], na.rm=TRUE)
  reachdata$distortion[241:288]<- as.numeric(meanreaches)*-1
  schedule<- reachdata$distortion
  
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  parvals<- matrix(parvals, nrow = 2, ncol = length(parvals))
  
  #searchgrid <- expand.grid('L'=parvals, 'R'=parvals)
  
  # evaluate starting positions:
  MSE <- apply(parvals, FUN=PropModelMSE, MARGIN=c(1), schedule=schedule, localizations=localizations)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( parvals[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=PropModelMSE,
                              method='L-BFGS-B',
                              lower=c(0),
                              upper=c(1),
                              schedule=schedule,
                              localizations=localizations ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1:2]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( parvals[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optim,
                              fn=PropModelMSE,
                              method='Brent', lower = 0, upper = 1,
                              schedule=schedule,
                              localizations=localizations ) )
    
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}



gridsearch<- function(localizations, schedule, nsteps=7, topn=4) {
  
  
  cat('doing grid search...\n')
  
  steps <- nsteps #say how many points inbetween 0-1 we want
  pargrid <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) #not sure what exactly this does
  MSE<- rep(NA, length(pargrid))
  pargrid<- cbind(pargrid, MSE)
  
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1])    #take that row and take it out of df and make it par 
    pargrid[gridpoint,2] <- PropModelMSE(par, schedule,localizations)
  }
  
  bestN <- order(pargrid[,2])[1:topn]
  
  return(pargrid[bestN,])
}







