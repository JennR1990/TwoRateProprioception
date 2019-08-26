# Plotting and Running Models----

Reachmodel<- function(groups = c('active', 'passive')) {
  allpars<- list()
  counter<- 1
  
  for (task in groups) {
  filename<- sprintf('%s_reaches.csv', task)
  data<-read.csv(filename, header = TRUE, sep = ',')
  reaches<- getreachesformodel(data)
  reach_par<- fittworatemodel(reaches = reaches$meanreaches, reaches$distortion)
  reach_model<-tworatemodel(par=reach_par, distortions = data$distortion)
  
  Plotmodel(data, task, 'Two-Rate')
  lines(reach_model$output, col = c(rgb(.5,0.,.5)))
  lines(reach_model$slow, col = rgb(0.,.5,1.))
  lines(reach_model$fast, col = rgb(0.0,0.7,0.0))
  
  reach_par1<- fitoneratemodel(reaches = reaches$meanreaches, reaches$distortion)
  reach_model1<-oneratemodel(par=reach_par1, distortions = data$distortion)
  Plotmodel(data, task, 'One-Rate')
  lines(reach_model1$output, col = c(rgb(.5,0.,.5)))
  lines(reach_model1$slow, col = rgb(0.,.5,1.))
  lines(reach_model1$fast, col = rgb(0.0,0.7,0.0))

  reach_pars<- rbind(reach_par, reach_par1)
  row.names(reach_pars)<- c('two-rate', 'one-rate')
  allpars[[counter]]<- reach_pars
  counter<- counter + 1
  }
  names(allpars)<- c('active', 'passive')
  return(allpars)
}

Plotmodel<- function(dataset, name, mn){
  title<- sprintf('%s Model Applied to %s Reaches', mn, name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:ncol(dataset)], na.rm = TRUE)
  plot(dataset$Mean*-1, ylim = c(-35, 35), xlab = "Trial",lwd= 2, ylab = "Hand Direction [deg]",col = c(rgb(0.8,0.8,0.8)), axes = FALSE, main = title, type = 'l')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5, -3,legend=c('Reach data', 'model','fast','slow'),col=c(rgb(0.44,0.51,0.57), rgb(.5,0.,.5),rgb(0.0,0.7,0.0),rgb(0.,.5,1.)),lty=c(1,1,1,1),lwd=c(2,2,2,2),bty='n')
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  lines(dataset$Mean*-1,col = c(rgb(0.44,0.51,0.57)))
}

PLotLearningCurve<- function(dataset){
  #svglite(file='paradigm.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  color3       <- rgb(1.0,0.4,0.0)      # orange
  color3_trans <- rgb(1.0,0.4,0.0,0.2)  # transparent orange
  dataCIs<- trialCI(data = dataset)
  dataCIs<- dataCIs*-1
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean*-1, axes= FALSE, ylim = c(-30, 35), xlab = "Trial", ylab = "Direction of Rotation [deg]", main = 'Multi-Rate Model Paradigm', type = 'l')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  legend(-5, -15,legend=c('Reach data'),col=c(rgb(1.0,0.4,0.0)),lty=c(1),lwd=c(2),bty='n')
  x<- c(c(1:nrow(dataset)), rev(c(1:nrow(dataset))))
  y<-c(dataCIs[,1], rev(dataCIs[,2]))
  polygon(x,y, col = color3_trans, border = NA)
  lines(dataset$Mean*-1, col = color3, lwd = 1.5)
  #dev.off()
}

# Model Comparison----
CompareModel<- function(groups = c('active', 'passive'), bootstraps) {
  
  allcomps<- list()
  counter<- 1
  for (group in groups){
    
    outcomes<- list()
    
    df <- read.csv(sprintf('%s_reaches.csv', group), stringsAsFactors = FALSE)
    distortion <- df$distortion
    
    reaches <- as.matrix(df[,2:dim(df)[2]])
    
    N <- dim(df)[2] - 1
    
    InOb <-   (288/48)  # I use these values because there are 288 trials which are not technically independent observations
    # and 48 is the last lag before the autocorrelation between time points drops below .1 

    for (bootstrap in c(1:bootstraps)) {

      
      medReaches <- apply(reaches[,sample(c(1:N),N,replace=TRUE)], 1, median, na.rm=TRUE)
      distortion<- distortion
      
      
      onerate_par<- fitoneratemodel(reaches = medReaches, distortions = distortion)
      tworate_par<- fittworatemodel(reaches = medReaches, distortions = distortion)
      
      
      distortion<- distortion
      onerate_model<- oneratemodel(par = onerate_par, distortions = distortion)
      tworate_model<- tworatemodel(par = tworate_par, distortions = distortion)
      
      
      twoRateMSE<-twoRateReachModelError(tworate_par, reaches = medReaches, distortions = distortion)
      oneRateMSE<-oneRateReachModelError(onerate_par, reaches = medReaches, distortions = distortion)
      
      twoRateAIC <- InOb + InOb * log(2 * pi) + InOb * log(twoRateMSE) + 2*5
      oneRateAIC <- InOb + InOb * log(2 * pi) + InOb * log(oneRateMSE) + 2 * 2
      
      twoRateBIC <- InOb + InOb * log(2 * pi) + InOb * log(twoRateMSE) + log(InOb) * 5
      oneRateBIC<-  InOb + InOb * log(2 * pi) + InOb * log(oneRateMSE) + log(InOb) * 2
      if (length(outcomes) == 0){
        outcomes[[1]]<- c(twoRateAIC, oneRateAIC)
        outcomes[[2]]<- c(twoRateBIC, oneRateBIC)
        outcomes[[3]]<-exp((min(c(twoRateAIC, oneRateAIC))-c(twoRateAIC, oneRateAIC))/2)
      }else{
        outcomes[[1]]<- c(outcomes[[1]], twoRateAIC, oneRateAIC)
        outcomes[[2]]<- c(outcomes[[2]], twoRateBIC, oneRateBIC)
        outcomes[[3]]<-c(outcomes[[3]],exp((min(c(twoRateAIC, oneRateAIC))-c(twoRateAIC, oneRateAIC))/2))
      }

      
    }
    names(outcomes)<- c("AIC", "BIC", "Relative Likelihood")
    names(outcomes[[1]])<- c(rep(c('two-Rate', "one-Rate"), times = bootstraps))
    names(outcomes[[2]])<- c(rep(c('two-Rate', "one-Rate"), times = bootstraps)) 
    names(outcomes[[3]])<- c(rep(c('two-Rate', "one-Rate"), times = bootstraps)) 
    
    allcomps[[counter]]<- outcomes 
    counter<- counter + 1
  }

  names(allcomps)<- groups
  return(allcomps)
}



# TWO-RATE MODEL ----

getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}


fittworatemodel<- function(reaches, distortions) {
  #this function will take the dataframe made in the last function (dogridsearch) and 
  #use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- dogridsearch(reaches = reaches, distortions = distortions, nsteps = 6, topn = 6)
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
  # The function optim fits the model.
  # Optim finds the parameter values, that minimize the error returned by MSE function.

}

dogridsearch<- function(reaches, distortions, nsteps=6, topn=6) {
  

  steps <- nsteps #say how many points inbetween 0-1 we want for starting parameters
  grid.steps <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) 
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
  
 
  # First check if all the parameters are within their constraints.
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
  
  
  
  df<- tworatemodel(par, distortions = distortions*-1)
  errors <- df$output - reaches

  # We want to minimize the root mean squared errors, or RMSE:
  MSE <- mean(errors^2, na.rm=TRUE)

  return(MSE)

}

tworatemodel<- function(par, distortions) {
  
  # set up empty vectors for function output:
  output<- c()
  fast<- c()
  slow<- c()
  
  # initial state of the processes:
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



# ONE-RATE MODEL ----

oneratemodel<- function(par, distortions) {
  
  # set up empty vectors for function output:
  output<- c()

  
  # initial state of the processes:
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



oneRateReachModelError <- function(par, reaches, distortions) {

  # First check if all the parameters are within their constraints.
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
  
  
  
  df<- oneratemodel(par, distortions = distortions*-1)
  errors <- df$output - reaches

  # We want to minimize the root mean squared errors, or RMSE:
  MSE <- mean(errors^2, na.rm=TRUE)

  return(MSE)

}


doonegridsearch<- function(reaches, distortions, nsteps=7, topn=5) {

  
  steps <- nsteps #say how many points inbetween 0-1 we want for starting parameters 
  grid.steps <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) 
  pargrid<-expand.grid(grid.steps,grid.steps,grid.steps,grid.steps) #makes the sequence of numbers have 4 dimensions (one for each parameter and shows each possible combination)
  colnames(pargrid) <-c('rs', 'ls') #named the parameters
  pargrid['MSE'] <- NA #add a column called MSE
  
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1:2])    #take that row and take it out of df and make it par 
    pargrid$MSE[gridpoint] <- oneRateReachModelError(par = par, reaches = reaches, distortions = distortions) #and use them  to do the model and get output.
  }
  
  bestN <- order(pargrid$MSE)[1:topn]
  
  return(pargrid[bestN,])
}




fitoneratemodel<- function(reaches, distortions) {
  #this function will take the dataframe made in the last function (dogridsearch) and 
  #use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- doonegridsearch(reaches = reaches, distortions = distortions, nsteps = 6, topn = 6)

  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1:2]) 
    
    control <- list('maxit'=10000, 'ndeps'=1e-9 )
    fit <- optim(par=par, oneRateReachModelError, gr=NULL, reaches, distortions, control=control)
    optpar<- fit$par
    
    # stick optpar back in pargrid
    pargrid[gridpoint,1:2] <- optpar
    
    pargrid$MSE[gridpoint]<- fit$value
    
  } 
  # get lowest MSE, and pars that go with that
  bestpar <- order(pargrid$MSE)[1]
  # return(those pars)
  return(unlist(pargrid[bestpar,1:2]))

}



