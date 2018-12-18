getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}


library(compiler)

treachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}




# ONE-RATE MODEL ----
getoneParticipantFits <- function(data) {
  
  participants <- colnames(data)[2:dim(data)[2]]
  distortions <- data$distortion
  
  participantfits <- data.frame(matrix(NA, ncol=4, nrow=length(participants)))
  colnames(participantfits) <- c('participant', 'rs', 'ls', 'MSE')
  
  for (ppno in c(1:length(participants))) {
    
    participant <- participants[ppno]
    print(participant)
    reaches <- data[,participant]
    
    pars <- fitoneratemodel(reaches, distortions)
    
    participantfits$participant[ppno] <- participant
    participantfits$rs[ppno] <- pars['rs']
    participantfits$ls[ppno] <- pars['ls']
    participantfits$MSE[ppno] <- oneRateReachModelError(pars, reaches, distortions)
    
  }
  
  return(participantfits)
}






oneratemodel<- function(par, distortions) {
  
  # set up empty vectors for function output:
  output<- c()
  
  # initial state of the processes / model:
  e0  <- 0
  X0 <- 0
  
  # loop through all trials:
  for(trial in c(1:length(distortions))) {
    
    # generate motor output:
    X1 <- (par['rs']*X0) + (par['ls']* e0)
    
    
    
    # observe current error:
    d1 <- distortions[trial]
    
    if (is.na(d1)) {
      e1 <- 0
    } else {
      e1 <- d1 - X1
    }
    
    
    # save current stuff for function output only:
    output <- c(output, X1)
    
    # store current stuff to now be the previous stuff:
    X0 <- X1
    e0 <- e1
    
    
  }
  
  model<- data.frame(output)
  return(model)
}

cmp.oneratemodel <- cmpfun(oneratemodel)

oneRateReachModelError <- function(par, reaches, distortions) {
  
  
  
  # This function is called by optim(), providing a set of parameter values, that are varied by optim.
  # Optim() also passes the data, which we use here to calculate how far off the model with the given
  # parameters is.
  
  # First we check if all the parameters are within their constraints.
  # If not, we don't even fit the model, but return an infinite error.
  
  # A reminder of what each parameter value is supposed to be:
  
  # par[1] = slow retention
  # par[2] = slow learning
  # par[3] = fast retention
  # par[4] = fast learning
  
  # par[1] should be higher than par[3]
  # par[2] should be lower than par[4]
  
  # The slow process should learn less than the fast process:
  
  # All parameters should be between 0 and 1:
  if (any(par < 0.0)) {
    return(Inf)
  }
  if (any(par > 1.0)) {
    return(Inf)
  }
  
  
  
  df<- cmp.oneratemodel(par, distortions = distortions)
  # If we got this far, the parameters are valid.
  # We can see what the model output is with those parameters.
  errors <- df$output - reaches
  
  # In fact, we only need to know how far the model is off from the real data, or how large the errors are.
  # The function twoRateReachModel() needs the parameters to generate model output, and returns on object
  # with a property called 'output' which is the full model output.
  # We immediately subtract the actual data from the model output. This should be zero for every trial
  # if the model were perfect.
  
  # uncomment this to fit only on the initial training phase:
  # errors[225:288] <- 0
  
  # We want to minimize the root mean squared errors, or RMSE:
  MSE <- mean(errors^2, na.rm=TRUE)
  
  # Optim() needs to know what the MSE is:
  return(MSE)
  
}

cmp.oneRateReachModelError <- cmpfun(oneRateReachModelError)

doonegridsearch<- function(reaches, distortions, nsteps=7, topn=5) {
  
  cat('doing grid search...\n')
  
  steps <- nsteps #say how many points inbetween 0-1 we want
  grid.steps <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) #not sure what exactly this does
  pargrid<-expand.grid(grid.steps,grid.steps,grid.steps,grid.steps) #makes the sequence of numbers have 4 dimensions (one for each parameter and shows each possible combination)
  colnames(pargrid) <-c('rs', 'ls') #named the parameters
  pargrid['MSE'] <- NA #add a column called MSE
  
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1:2])    #take that row and take it out of df and make it par 
    pargrid$MSE[gridpoint] <- cmp.oneRateReachModelError(par = par, reaches = reaches, distortions = distortions) #and use them  to do the model and get output.
  }
  
  bestN <- order(pargrid$MSE)[1:topn]
  
  return(pargrid[bestN,])
}

cmp.doonegridsearch <- cmpfun(doonegridsearch)


fitoneratemodel<- function(reaches, distortions) {
  #this function will take the dataframe made in the last function (dogridsearch) and use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- cmp.doonegridsearch(reaches = reaches, distortions = distortions, nsteps = 7, topn = 5)
  cat('optimize best fits...\n')
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1:2]) 
    
    control <- list('maxit'=10000, 'ndeps'=1e-9 )
    fit <- optim(par=par, cmp.oneRateReachModelError, gr=NULL, reaches, distortions, control=control)
    optpar<- fit$par
    
    # stick optpar back in pargrid
    pargrid[gridpoint,1:2] <- optpar
    
    pargrid$MSE[gridpoint]<- fit$value
    
  } 
  # get lowest MSE, and pars that go with that
  bestpar <- order(pargrid$MSE)[1]
  # return(those pars)
  return(unlist(pargrid[bestpar,1:2]))
  
  
  
  
  
  
  # The function optim fits the model for us (read it's help page).
  # Optim finds the parameter values, that minimize the error returned by the function we point it to.
  # We give optim some starting parameter values, a function to be minimized, a function for gradients,
  # that we don't use (it is set to NULL), stuff to pass to these functions (the reach data), and then we
  # provide some other arguments, including this 'control' thing. I set that to a value that allows optim
  # to search longer, so that the fit might be improved at the cost of longer computation.
}

cmp.fitoneratemodel <- cmpfun(fitoneratemodel)

# TWO-RATE MODEL ----
getParticipantFits <- function(data) {
  
  participants <- colnames(data)[2:dim(data)[2]]
  distortions <- data$distortion
  
  participantfits <- data.frame(matrix(NA, ncol=6, nrow=length(participants)))
  colnames(participantfits) <- c('participant', 'rs', 'ls', 'rf', 'lf', 'MSE')
  
  for (ppno in c(1:length(participants))) {
    
    participant <- participants[ppno]
    print(participant)
    reaches <- data[,participant]
    
    pars <- fittworatemodel(reaches, distortions)
    
    participantfits$participant[ppno] <- participant
    participantfits$rs[ppno] <- pars['rs']
    participantfits$ls[ppno] <- pars['ls']
    participantfits$rf[ppno] <- pars['rf']
    participantfits$lf[ppno] <- pars['lf']
    participantfits$MSE[ppno] <- twoRateReachModelError(pars, reaches, distortions)
    
  }
  
  return(participantfits)
}



fittworatemodel<- function(reaches, distortions) {
  #this function will take the dataframe made in the last function (dogridsearch) and use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- dogridsearch(reaches = reaches, distortions = distortions, nsteps = 7, topn = 5)
  cat('optimize best fits...\n')
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1:4]) 
    
    control <- list('maxit'=10000, 'ndeps'=1e-9 )
    fit <- optim(par=par, twoRateReachModelError, gr=NULL, reaches, distortions, control=control)
    optpar<- fit$par
    
    # stick optpar back in pargrid
    pargrid[gridpoint,1:4] <- optpar
    
    pargrid$MSE[gridpoint]<- fit$value
    
  } 
  # get lowest MSE, and pars that go with that
  bestpar <- order(pargrid$MSE)[1]
  # return(those pars)
  return(unlist(pargrid[bestpar,1:4]))
  
  
  
  
  
  
  # The function optim fits the model for us (read it's help page).
  # Optim finds the parameter values, that minimize the error returned by the function we point it to.
  # We give optim some starting parameter values, a function to be minimized, a function for gradients,
  # that we don't use (it is set to NULL), stuff to pass to these functions (the reach data), and then we
  # provide some other arguments, including this 'control' thing. I set that to a value that allows optim
  # to search longer, so that the fit might be improved at the cost of longer computation.
}

dogridsearch<- function(reaches, distortions, nsteps=7, topn=5) {
  
  cat('doing grid search...\n')
  
  steps <- nsteps #say how many points inbetween 0-1 we want
  grid.steps <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) #not sure what exactly this does
  pargrid<-expand.grid(grid.steps,grid.steps,grid.steps,grid.steps) #makes the sequence of numbers have 4 dimensions (one for each parameter and shows each possible combination)
  colnames(pargrid) <-c('rs', 'ls', 'rf', 'lf') #named the parameters
  pargrid['MSE'] <- NA #add a column called MSE
  
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1:4])    #take that row and take it out of df and make it par 
    if (par['rs'] <= par['rf']) {
      next()
    } 
    if (par['ls'] >= par['lf']) {
      next()
    } 
    pargrid$MSE[gridpoint] <- twoRateReachModelError(par = par, reaches = reaches, distortions = distortions) #and use them  to do the model and get output.
  }
  
  bestN <- order(pargrid$MSE)[1:topn]
  
  return(pargrid[bestN,])
}


twoRateReachModelError <- function(par, reaches, distortions) {
  
  
  
  # This function is called by optim(), providing a set of parameter values, that are varied by optim.
  # Optim() also passes the data, which we use here to calculate how far off the model with the given
  # parameters is.
  
  # First we check if all the parameters are within their constraints.
  # If not, we don't even fit the model, but return an infinite error.
  
  # A reminder of what each parameter value is supposed to be:
  
  # par[1] = slow retention
  # par[2] = slow learning
  # par[3] = fast retention
  # par[4] = fast learning
  
  # par[1] should be higher than par[3]
  # par[2] should be lower than par[4]
  if (par['rs'] <= par['rf']) {
    return(Inf)
  } 
  # The slow process should learn less than the fast process:
  if (par['ls'] >= par['lf']) {
    return(Inf)
  } 
  # All parameters should be between 0 and 1:
  if (any(par < 0.0)) {
    return(Inf)
  }
  if (any(par > 1.0)) {
    return(Inf)
  }
  
  
  
  df<- tworatemodel(par, distortions = distortions)
  # If we got this far, the parameters are valid.
  # We can see what the model output is with those parameters.
  errors <- df$output - reaches
  
  # In fact, we only need to know how far the model is off from the real data, or how large the errors are.
  # The function twoRateReachModel() needs the parameters to generate model output, and returns on object
  # with a property called 'output' which is the full model output.
  # We immediately subtract the actual data from the model output. This should be zero for every trial
  # if the model were perfect.
  
  # uncomment this to fit only on the initial training phase:
  # errors[225:288] <- 0
  
  # We want to minimize the root mean squared errors, or RMSE:
  MSE <- mean(errors^2, na.rm=TRUE)
  
  # Optim() needs to know what the MSE is:
  return(MSE)
  
}

tworatemodel<- function(par, distortions) {
  
  # set up empty vectors for function output:
  output<- c()
  fast<- c()
  slow<- c()
  
  # initial state of the processes / model:
  e0  <- 0
  Xf0 <- 0
  Xs0 <- 0
  
  # loop through all trials:
  for(trial in c(1:length(distortions))) {
    
    # generate motor output:
    Xf1 <- (par['rf']*Xf0) + (par['lf']* e0)
    Xs1 <- (par['rs']*Xs0) + (par['ls']* e0)
    X1 <- Xf1 + Xs1
    
    
    
    # observe current error:
    d1 <- distortions[trial]
    
    if (is.na(d1)) {
      e1 <- 0
    } else {
      e1 <- d1 - X1
    }
    
    
    # save current stuff for function output only:
    fast <- c(fast, Xf1)
    slow <- c(slow, Xs1)
    output <- c(output, X1)
    
    # store current stuff to now be the previous stuff:
    Xs0 <- Xs1
    Xf0 <- Xf1
    e0 <- e1
    
    
  }
  
  model<- data.frame(output, fast, slow)
  return(model)
}

