#dist<- c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
#dist2 <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48))
#par<- c('rf'= .748, 'rs'= 1.00, 'lf' = .202, 'ls' = .052)# from poster
#medreaches <- c(3.283128037, 0.088265435, -1.126710212, -0.941539536, -2.768320114, -2.814756841, 0.005531715, -1.684863553, 1.533275275, -2.360002523, 2.556743256, -1.199544351, 0.772210709, 0.929806554, -0.904089781, 0.065525686, 1.210596381, -1.744177153, -1.948214908, -1.028632095, -1.710742470, -0.946543826, -0.885579030, -1.480397499, -1.015919749, 0.832226302, 1.691115704, -1.943003789, 0.693919075, -1.137709109, 0.461002876, 0.408015661, -0.573706452, -2.045953970, -1.647000781, -0.144229621, 1.201901674, 1.363873491, -0.878315702, -1.213127787, 1.571513241, -0.824285326, 0.926177682, -2.456982703, 0.159764905, 1.954141435, -1.355997013, -1.081765928, -0.030875229, 0.601594211, -0.512593916, -0.594332646, -0.230100232, 0.186697274, -1.909365901, 2.418297773, 0.194866233, 2.493820532, -1.905953464, 0.425167992, -1.093003809, 0.525923185, -0.309437282, 0.481126284, 3.287411348, 12.806366134, 14.206275862, 18.132674330, 20.846982908, 23.170723935, 22.401924319, 21.688495784, 26.785615381, 22.504904139, 28.596059316, 25.167386433, 26.445565256, 27.505713085, 26.397467861, 27.246328085, 26.263281576, 27.714794948, 23.153906946, 26.940543936, 27.924033778, 25.211646461, 26.400793948, 27.442005598, 28.067184058, 25.440862278, 24.814499145, 23.988783587, 24.952368975, 26.466530077, 23.048821456, 27.183001388, 25.300502154, 26.990167401, 26.563190078, 27.413864150, 28.312407528, 31.682089170, 27.181975851, 24.980172668, 28.004379585, 28.914478966, 25.896105128, 26.868580640, 27.541706975, 28.318385310, 25.977365432, 26.154468057, 25.038038420, 26.934978763, 26.869374830, 26.539477989, 26.678369796, 26.225624705, 26.559956884, 30.531846811, 27.160695618, 28.279833295, 25.836815254, 28.525647727, 27.414692455, 28.454362854, 28.619802809, 29.099452243, 28.885845614, 26.225077039, 29.693626164, 27.572848390, 26.852628226, 26.236813782, 29.437599569, 29.471845739, 28.700614193, 27.509009762, 28.531613726, 28.223292464, 29.494278592, 28.825000680, 28.723868516, 27.609734826, 30.662015183, 29.166667118, 29.938723473, 27.400733681, 28.671080347, 27.417666639, 30.533992654, 27.539507246, 27.765137140, 29.323394891, 31.549652757, 29.012600148, 29.245840640, 28.443619074, 30.432476073, 31.133099296, 27.574199437, 29.930334460, 28.323313400, 29.711703198, 29.124816412, 31.422016263, 25.610772099, 29.066286272, 29.661352212, 31.011568718, 27.929587430, 30.674551931, 29.838808535, 32.111537309, 30.074694665, 30.138919691, 28.224386691, 30.136947851, 28.333627030, 30.032812077, 31.260068600, 25.761523090, 32.643004517, 27.628808034, 31.299468637, 27.109342996, 31.608406134, 27.280828385, 30.432333824, 27.944321138, 28.725316111, 28.450434598, 29.367771150, 31.612998339, 29.016619751, 29.455986301, 28.631272269, 30.207792053, 28.391433398, 30.753069070, 27.338516402, 27.621032567, 29.441107272, 30.831949003, 31.779852808, 28.274832797, 29.151750192, 28.495438729, 28.951346454, 28.348155089, 28.681935365, 30.934337518, 30.539180468, 29.467195393, 28.565586394, 30.944618667, 29.256142703, 29.990054766, 30.246370428, 31.043135569, 27.480534962, 28.387654595, 30.473757170, 31.967633942, 24.228915532, 14.319479324, 9.692972217, 0.583585122, -0.747260921, -0.789399317, -4.483411789, -5.235365811, -4.134710315, -8.124877144, -5.954150683, -9.070169879, -9.369845930, -9.730443485, -8.626549238, -9.583611945, -9.314945799, 2.691235851, 6.591875863, 9.226681786, 7.697941703, 10.208340060, 10.084082891, 4.634589975, 10.590532103, 9.335852135, 8.159969847, 6.714217993, 4.035887695, 4.927638343, 12.617707660, 10.369281600, 11.776011877, 3.284922897, 3.409783690, 7.984892090, 7.662525015, 2.984856069, 1.710015997, 4.874313962, 12.093575882, 11.222585272, 6.286926929, 6.131352251, 1.651954505, 5.272433832, 9.459216375, 4.476815665, 6.505227862, 6.209145689, 6.584371334, -1.440130669, -1.548091889, 1.097286232, 5.333357953, -0.940712434, 9.907095001, 5.895035530, 8.018784652, 8.259656158, 11.460089569, 7.272618393, 4.348521649, 1.978214645)





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
    
    pars<- fitTwoRateReachModel(reaches = reaches, schedule = distortions)
    #pars <- fittworatemodel(reaches, distortions)
    
    participantfits$participant[ppno] <- participant
    participantfits$rs[ppno] <- pars['rs']
    participantfits$ls[ppno] <- pars['ls']
    participantfits$rf[ppno] <- pars['rf']
    participantfits$lf[ppno] <- pars['lf']
    participantfits$MSE[ppno] <- twoRateReachModelErrors(pars, reaches, distortions)
    
  }
  
  return(participantfits)
}



fittworatemodel<- function(reaches, distortions) {
  #this function will take the dataframe made in the last function (dogridsearch) and use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- dogridsearch(reaches = reaches, distortions = distortions, nsteps = 6, topn = 6)
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

dogridsearch<- function(reaches, distortions, nsteps=6, topn=6) {
  
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
  
  
  
  df<- tworatemodel(par, distortions = distortions*-1)
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
  e0  <- 0 # in Marius' version this is not set to anything
  Xf0 <- 0
  Xs0 <- 0
  
  
  # In marius' version he does a bunch of stuff to the parameters when the and some stuff that we do within the loop he does here
  # loop through all trials:
  for(trial in c(1:length(distortions))) {
    
    # generate motor output:
    Xf1 <- (par['rf']*Xf0) + (par['lf']* e0)
    Xs1 <- (par['rs']*Xs0) + (par['ls']* e0)
    X1 <- Xf1 + Xs1 # Marius does this earlier and then again after they find the error
    # marius constrains the parameters to act the way they should given other parameters
    
    
    
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


# EXTRACT REACH ANGLES ----

getreachesfromfileformodel<- function(filename, pnum) {
  df<- read.csv(filename)
  meanreaches<-rowMeans(df[,2:pnum], na.rm=TRUE)
  distortion<- df$distortion
  return(data.frame(meanreaches,distortion))
}
getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}



getallparticipants<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:32)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48)) 
  } else if (experiment == 3) {
    participants<- c(1:24)
    P1table <- read.table('Time Model Variant 3 Selected Data/time_model3_1/1_1__time_model_3_reach_selected.txt')
    names(P1table)[1:5] <- c('task','block','trial','targetangle','rotation')
    temp <- aggregate(rotation ~ trial, data=P1table, FUN=mean)
    distortion <- as.numeric(c(temp$rotation))
    # distortion <-  c(rep(0,49),rep(-30,12), rep(-15,12), rep(0,12),rep(-15,12), rep(15,12), rep(0,12), rep(0,12), rep(15,12), rep(15,12), rep(-30,12), rep(-30,12), rep(0,12), rep(-30,12), rep(0,12), rep(30,12), rep(0,12), rep(0,12), rep(30,12), rep(-15,12), rep(-15,12), rep(30,12), rep(30,12), rep(15,12), rep(-15,12), rep(0,12), rep(-15,12), rep(-30,12), rep(30,12), rep(0,12), rep(0,12), rep(15,12), rep(30,12), rep(-30,12), rep(15,12), rep(0,12), rep(0,11))
  } else if (experiment == 4) {
    participants<- c(1:16)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 5) {
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 7) {
    participants<- c(2)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  }
  
  expangles<- data.frame(distortion)
  
  print(participants)
  for (participant in participants){
    
    partiangles<-getparticipantdata(participant = participant, experiment = experiment)
    print(participant)
    print(dim(partiangles))
    #baselining only works for experiment 1&5, 2 & 4 only have 32 aligned in total 
    #so they would need different amount of trials
    baselinedangles <- baselinebyaligned(df = partiangles, experiment = experiment)
    print(experiment)
    if (experiment == 3) {
    expangles[,sprintf('p%d',participant)] <- baselinedangles$reachdeviations[1:480]
    } else if (experiment %in% c(1,5,7)) {
      expangles[,sprintf('p%d',participant)] <- baselinedangles$reachdeviations[1:288]  
    } else if (experiment %in% c(2,4)) {
      print(nrow(expangles))
      expangles[,sprintf('p%d',participant)] <- baselinedangles$reachdeviations[1:320]
    }
  }
  outputfilename<- sprintf('time_model%d_Updated_Reaches.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}

  




baselinebyaligned<- function(df, experiment) {
  
  #take an average of the aligned data where distortion = 0
  #subtract that from reach angles
  if (experiment == 1| experiment == 5| experiment == 7) {
    bias<-mean(df$reachdeviations[32:64], na.rm = TRUE)
    df$reachdeviations[1:288]<- df$reachdeviations[1:288] - bias
  } else if (experiment == 2 | experiment == 4) {
    bias<-mean(df$reachdeviations[64:96], na.rm = TRUE)
    df$reachdeviations[1:320]<- df$reachdeviations[1:320] - bias
  } else if (experiment == 3) {
    bias<-mean(df$reachdeviations[23:49], na.rm = TRUE)
    corrected <- df$reachdeviations - bias
    df$reachdeviations <- corrected
  }
  
  return(df)
}




getparticipantdata<- function(participant, experiment) {
  
  filenames <- getfilenames(participant, experiment)
  
  ppangles <- data.frame()
  
  for (filename in filenames) {
    
    reachNangles <- getanglesfortask(filename)
    
    if (prod(dim(ppangles)) == 0) {
      ppangles <- reachNangles
    } else {
      ppangles <- rbind(ppangles, reachNangles)
    }
    
  }
  return(ppangles)
  
}

getfilenames<- function (ppn, expn) {
  
  if (expn == 1) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 2 Selected Data/'
    
    ppfolder <- sprintf('time_model2_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model - No Cursor New Instructions Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 5 Raw & Selected Data/'
    
    ppfolder <- sprintf('time_model_activeloc_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model4_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 7) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Terminal Raw Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }
  return(filenames) 
}






loadreachfile <- function(filename) {
  df<-read.table(filename)
  if (ncol(df) == 21){
  colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step','clamped','trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")
  } else{
  colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step', 'trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")  
  }
  print(class(df$cursory_cm))
  df$cursory_cm <- df$cursory_cm +8.5
  df$handy_cm <- df$handy_cm +8.5
  df$targety_cm <- df$targety_cm +8.5
  df$homey_cm <- df$homey_cm +8.5
  df$targetangle_deg <- (-1 *(df$targetangle_deg - 90)) + 90
  return(df)
}


getanglesfortask<- function (filename) {
  df<-loadreachfile(filename = filename)
  blocknos <- unique(df$block)
  reachdeviations <- c()
  targetangles <- c()
  
  for (blockno in blocknos) {
    trialdf <- df[which(df$block == blockno),]
    angles <- getTrialReachAngleAt(trialdf)
    reachdeviations <- c(reachdeviations, angles[1])
    targetangles <- c(targetangles, angles[2])
    
  }
  return(data.frame(reachdeviations,targetangles))
  
  #the below code works to make this function automatically write the file for us, but it needs to make a bunch of files and then put all those together
  #so this isn't the best way yet. 
  #angles<-data.frame(reachdeviations,targetangles)
  #write.csv(angles, file = "aligned_output.csv")
  
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates by angle degrees
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


getTrialReachAngleAt <- function(trialdf, location='maxvel') {
  
  
  # location (string) determines where the angle of the reach is
  #determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is
  # replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=2)
  
  
  # we extract the target angle
  angle <- trialdf[1,'targetangle_deg']
  
  # so we can at least return that:
  reachangle[1,2] <- angle
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  
  X <- trialdf[trialdf$sampleselected == 1,'handx_cm']
  Y <- trialdf[trialdf$sampleselected == 1,'handy_cm']
  MV <- trialdf[trialdf$sampleselected == 1,'maxvelocity']
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is
  # reached
  # this assumes that people don't go back, or that there is only one
  #movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  
  return(reachangle)
  
}


# EXTRACT PROP DEVIATIONS -------------------------------------------------
getallTaps<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:23)
    distortion <-  c(rep(0,112)) 
  } else if (experiment == 3) {
    participants<- c(1:9)
    distortion <-  c(rep(0,49),rep(-30,12), rep(-15,12), rep(0,12),rep(-15,12), rep(15,12), rep(0,12), rep(0,12), rep(15,12), rep(15,12), rep(-30,12), rep(-30,12), rep(0,12), rep(-30,12), rep(0,12), rep(30,12), rep(0,12), rep(0,12), rep(30,12), rep(-15,12), rep(-15,12), rep(30,12), rep(30,12), rep(15,12), rep(-15,12), rep(0,12), rep(-15,12), rep(-30,12), rep(30,12), rep(0,12), rep(0,12), rep(15,12), rep(30,12), rep(-30,12), rep(15,12), rep(0,12), rep(0,11))
  } else if (experiment == 4) {
    participants<- c(1:9)
    distortion <-  c(rep(0,112))
  } else if (experiment == 5) {
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  }
  
  expangles<- data.frame(distortion)
  
  for (participant in participants){
    partiangles<-getparticipantpropdata(participant = participant, experiment = experiment)
    #baselining only works for experiment 1&5, 2 & 4 only have 32 aligned in total 
    #so they would need different amount of trials
    baselinedangles <- baselineTapbyaligned(df = partiangles, experiment = experiment)
    expangles[,sprintf('p%d',participant)] <- baselinedangles$Tapdeviations
  }
  outputfilename<- sprintf('time_model%d_baseline_output_Prop.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}


baselineTapbyaligned<- function(df, experiment) {
  
  #take an average of the aligned data where distortion = 0
  #subtract that from reach angles
  if (experiment == 1| experiment == 5) {
    bias<-mean(df$Tapdeviations[32:64], na.rm = TRUE)
    df$Tapdeviations[1:288]<- df$Tapdeviations[1:288] - bias
  } else if (experiment == 2 | experiment == 4) {
    # bias<-mean(df$Tapdeviations[1:32], na.rm = TRUE)
    print(df)#$Tapdeviations[1:nrow(df)]<- df$Tapdeviations[1:nrow(df)] - 0
  } else if (experiment == 3) {
    bias<-mean(df$reachdeviations[23:49], na.rm = TRUE)
    corrected <- df$reachdeviations - bias
    df$reachdeviations <- corrected
  }
  return(df)
}


getparticipantpropdata<- function(participant, experiment) {
  
  filenames <- getpropfilenames(participant, experiment)
  
  ppangles <- data.frame()
  
  for (filename in filenames) {
    
    TapNangles <- getanglesforTaptask(filename)
    
    if (prod(dim(ppangles)) == 0) {
      ppangles <- TapNangles
    } else {
      ppangles <- rbind(ppangles, TapNangles)
    }
    ppangles$Tapdeviations[ppangles$selected == 0] <- NA 
  }
  return(ppangles)
  
}

getpropfilenames<- function (ppn, expn) {
  
  if (expn == 1) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model/Time Model Variant 2 Selected Data/'
    
    ppfolder <- sprintf('time_model2_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1)
    
    expfolder <- '../Time Model/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model/Time Model Variant 4 Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 5 Raw & Selected Data/'
    
    ppfolder <- sprintf('time_model_activeloc_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model4_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }
  return(filenames) 
}


loadpropfile <- function(filename) {
  df<-read.table(filename, header = TRUE)
  #colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step','trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")
  # df$cursory_cm <- df$cursory_cm +8.5
  # df$handy_cm <- df$handy_cm +8.5
  # df$targety_cm <- df$targety_cm +8.5
  # df$homey_cm <- df$homey_cm +8.5
  df$targetangle_deg <- (-1 *(df$targetangle_deg - 90)) + 90
  return(df)
}


getanglesforTaptask<- function (filename) {
  df<-loadpropfile(filename = filename)
  Tapdeviations<- ((atan2(df$tapy_cm, df$tapx_cm) / pi) * 180) - df$targetangle_deg
  targetangles <- df$targetangle_deg
  selected <- df$selected
  return(data.frame(Tapdeviations,targetangles, selected))
}
# EXTRACT NO-CURSOR ANGLES ------------------------------------------------
getreachesfromfileformodel<- function(filename, pnum) {
  df<- read.csv(filename)
  meanreaches<-rowMeans(df[,2:pnum], na.rm=TRUE)
  distortion<- df$distortion
  return(data.frame(meanreaches,distortion))
}
getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}



getallncparticipants<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:32)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48)) 
  } else if (experiment == 3) {
    participants<- c(1:9)
    P1table <- read.table('Time Model Variant 3 Selected Data/time_model3_1/1_1__time_model_3_reach_selected.txt')
    names(P1table)[1:5] <- c('task','block','trial','targetangle','rotation')
    temp <- aggregate(rotation ~ trial, data=P1table, FUN=mean)
    distortion <- as.numeric(c(temp$rotation))
    # distortion <-  c(rep(0,49),rep(-30,12), rep(-15,12), rep(0,12),rep(-15,12), rep(15,12), rep(0,12), rep(0,12), rep(15,12), rep(15,12), rep(-30,12), rep(-30,12), rep(0,12), rep(-30,12), rep(0,12), rep(30,12), rep(0,12), rep(0,12), rep(30,12), rep(-15,12), rep(-15,12), rep(30,12), rep(30,12), rep(15,12), rep(-15,12), rep(0,12), rep(-15,12), rep(-30,12), rep(30,12), rep(0,12), rep(0,12), rep(15,12), rep(30,12), rep(-30,12), rep(15,12), rep(0,12), rep(0,11))
  } else if (experiment == 4) {
    participants<- c(1:16)
    distortion <-  c(rep(0,32),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 5) {
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  }
  
  expangles<- data.frame(distortion)
  
  for (participant in participants){
    
    partiangles<-getncparticipantdata(participant = participant, experiment = experiment)
    #baselining only works for experiment 1&5, 2 & 4 only have 32 aligned in total 
    #so they would need different amount of trials
    baselinedangles <- baselinebyaligned(df = partiangles, experiment = experiment)
    expangles[,sprintf('p%d',participant)] <- baselinedangles$reachdeviations
  }
  outputfilename<- sprintf('time_model%d_No-Cursors.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}






baselinebyaligned<- function(df, experiment) {
  
  #take an average of the aligned data where distortion = 0
  #subtract that from reach angles
  if (experiment == 1| experiment == 5) {
    bias<-mean(df$reachdeviations[32:64], na.rm = TRUE)
    df$reachdeviations[1:288]<- df$reachdeviations[1:288] - bias
  } else if (experiment %in% c(2,4)) {
    bias<-mean(df$reachdeviations[1:32], na.rm = TRUE)
    df$reachdeviations[1:320]<- df$reachdeviations[1:320] - bias
  } else if (experiment == 3) {
    bias<-mean(df$reachdeviations[23:49], na.rm = TRUE)
    corrected <- df$reachdeviations - bias
    df$reachdeviations <- corrected
  }
  
  return(df)
}




getncparticipantdata<- function(participant, experiment) {
  
  filenames <- getncfilenames(participant, experiment)
  
  ppangles <- data.frame()
  
  for (filename in filenames) {
    
    reachNangles <- getncanglesfortask(filename)
    
    if (prod(dim(ppangles)) == 0) {
      ppangles <- reachNangles
    } else {
      ppangles <- rbind(ppangles, reachNangles)
    }
    
  }
  return(ppangles)
  
}

getncfilenames<- function (ppn, expn) {
  
  if (expn == 1) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 2 Selected Data/'
    
    ppfolder <- sprintf('time_model2_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(2:5)
    
    expfolder <- '../Time Model Good Data/Time Model - No Cursor New Instructions Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_NoC_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 5 Raw & Selected Data/'
    
    ppfolder <- sprintf('time_model_activeloc_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model4_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }
  return(filenames) 
}






loadncreachfile <- function(filename) {
  df<-read.table(filename)
  colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step','trialselected', 'sampleselected', 'sampleinterpolated', "maxvelocity")
  df$cursory_cm <- df$cursory_cm +8.5
  df$handy_cm <- df$handy_cm +8.5
  df$targety_cm <- df$targety_cm +8.5
  df$homey_cm <- df$homey_cm +8.5
  df$targetangle_deg <- (-1 *(df$targetangle_deg - 90)) + 90
  return(df)
}


getncanglesfortask<- function (filename) {
  df<-loadncreachfile(filename = filename)
  blocknos <- unique(df$block)
  reachdeviations <- c()
  targetangles <- c()
  
  for (blockno in blocknos) {
    trialdf <- df[which(df$block == blockno),]
    angles <- getncTrialReachAngleAt(trialdf)
    reachdeviations <- c(reachdeviations, angles[1])
    targetangles <- c(targetangles, angles[2])
    
  }
  return(data.frame(reachdeviations,targetangles))
  
  #the below code works to make this function automatically write the file for us, but it needs to make a bunch of files and then put all those together
  #so this isn't the best way yet. 
  #angles<-data.frame(reachdeviations,targetangles)
  #write.csv(angles, file = "aligned_output.csv")
  
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates by angle degrees
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


getncTrialReachAngleAt <- function(trialdf, location='endpoint') {
  
  
  # location (string) determines where the angle of the reach is
  #determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is
  # replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=2)
  
  
  # we extract the target angle
  angle <- trialdf[1,'targetangle_deg']
  
  # so we can at least return that:
  reachangle[1,2] <- angle
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  
  X <- trialdf[trialdf$sampleselected == 1,'handx_cm']
  Y <- trialdf[trialdf$sampleselected == 1,'handy_cm']
  MV <- trialdf[trialdf$sampleselected == 1,'maxvelocity']
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is
  # reached
  # this assumes that people don't go back, or that there is only one
  #movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  
  return(reachangle)
  
}


# OUTLIER REMOVAL ---------------------------------------------------------

removeSDoutliers <- function(values, sds=2) {
  
  avg <- mean(values, na.rm=TRUE)
  std <- sd(values, na.rm=TRUE) * sds
  
  values[values > avg + std] <- NA
  values[values < avg - std] <- NA
  
  return(values)
  
}

removeIQRoutliers <- function(values, range=1.5) {
  
  bp <- boxplot(values, range=1.5, plot=FALSE)
  
  values[values %in% bp$out] <- NA
  
  return(values)
  
}


removeReachOutliers <- function(data) {
  
  ntrials <- nrow(data)
  
  for (trialn in c(1:ntrials)) {
    
    data[trialn,2:ncol(data)] <- removeSDoutliers(as.numeric(data[trialn,2:ncol(data)]))
    
  }
  
  return(data)
  
}

#write.csv(Active_PropC, "Active_Prop_Data.csv", quote = FALSE, row.names = FALSE)


# REPEATED MEASURES COMBINE -----------------------------------------------

mmed <- function(x,n=5){runmed(x,n)}

##repeated Measures Combine function

RepeatedMeasuresCombine1or5<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[61:64,participant]), na.rm = TRUE)
    r1<- unlist(data[65,participant])
    r2<- unlist(data[66,participant])
    r3<- unlist(data[67,participant])
    r4<- unlist(data[68,participant])
    R1_Early<- mean(unlist(data[65:68,participant]), na.rm = TRUE)
    R1_Late<- mean(unlist(data[221:224,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[237:240,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[241:244,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[265:288,participant]), na.rm = TRUE)
    
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late)
    
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  
  
  return(ParticipantRM)
  
  
}


RepeatedMeasuresCombine2<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[93:96,participant]), na.rm = TRUE)
    r1<- unlist(data[97,participant])
    r2<- unlist(data[98,participant])
    r3<- unlist(data[99,participant])
    r4<- unlist(data[100,participant])
    R1_Early<- mean(unlist(data[97:100,participant]), na.rm = TRUE)
    R1_Late<- mean(unlist(data[253:256,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[269:272,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[273:276,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[297:320,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late)
    
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  
  
  return(ParticipantRM)
  
  
}


RepeatedMeasuresCombine4<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[93:96,participant]), na.rm = TRUE)
    r1<- unlist(data[97,participant])
    r2<- unlist(data[98,participant])
    r3<- unlist(data[99,participant])
    r4<- unlist(data[100,participant])
    R1_Early<- mean(unlist(data[97:100,participant]), na.rm = TRUE)
    R1_Late<- mean(unlist(data[253:256,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[269:272,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[273:276,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[297:320,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late)
    
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  
  
  return(ParticipantRM)
  
  
}
#write.csv(Rep_AP, "Active_Prop_RM.csv", quote = FALSE, row.names = FALSE)

# ONE-RATE MODEL ----
library(compiler)

treachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}





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
  
  
  
  df<- cmp.oneratemodel(par, distortions = distortions*-1)
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
  print(MSE)
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


# Reach Angles From Unselected Data ---------------------------------------

getUSallparticipants<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:32)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48)) 
  } else if (experiment == 3) {
    participants<- c(1:9)
    P1table <- read.table('Time Model Variant 3 Selected Data/time_model3_1/1_1__time_model_3_reach_selected.txt')
    names(P1table)[1:5] <- c('task','block','trial','targetangle','rotation')
    temp <- aggregate(rotation ~ trial, data=P1table, FUN=mean)
    distortion <- as.numeric(c(temp$rotation))
    # distortion <-  c(rep(0,49),rep(-30,12), rep(-15,12), rep(0,12),rep(-15,12), rep(15,12), rep(0,12), rep(0,12), rep(15,12), rep(15,12), rep(-30,12), rep(-30,12), rep(0,12), rep(-30,12), rep(0,12), rep(30,12), rep(0,12), rep(0,12), rep(30,12), rep(-15,12), rep(-15,12), rep(30,12), rep(30,12), rep(15,12), rep(-15,12), rep(0,12), rep(-15,12), rep(-30,12), rep(30,12), rep(0,12), rep(0,12), rep(15,12), rep(30,12), rep(-30,12), rep(15,12), rep(0,12), rep(0,11))
  } else if (experiment == 4) {
    participants<- c(1:32)
    distortion <-  c(rep(0,32),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 5) {
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  }
  
  expangles<- data.frame(distortion)
  
  for (participant in participants){
    
    partiangles<-getUSparticipantdata(participant = participant, experiment = experiment)
    #baselining only works for experiment 1&5, 2 & 4 only have 32 aligned in total 
    #so they would need different amount of trials
    baselinedangles <- baselinebyaligned(df = partiangles, experiment = experiment)
    expangles[,sprintf('p%d',participant)] <- baselinedangles$reachdeviations
  }
  outputfilename<- sprintf('time_model%d_baseline_Unselected_NoC_output.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}






baselinebyaligned<- function(df, experiment) {
  
  #take an average of the aligned data where distortion = 0
  #subtract that from reach angles
  if (experiment == 1| experiment == 5) {
    bias<-mean(df$reachdeviations[32:64], na.rm = TRUE)
    df$reachdeviations[1:288]<- df$reachdeviations[1:288] - bias
  } else if (experiment == 2 | experiment == 4) {
    bias<-mean(df$reachdeviations[1:32], na.rm = TRUE)
    df$reachdeviations[1:256]<- df$reachdeviations[1:256] - bias
  } else if (experiment == 3) {
    bias<-mean(df$reachdeviations[23:49], na.rm = TRUE)
    corrected <- df$reachdeviations - bias
    df$reachdeviations <- corrected
  }
  
  return(df)
}




getUSparticipantdata<- function(participant, experiment) {
  
  filenames <- getUSfilenames(participant, experiment)
  
  ppangles <- data.frame()
  
  for (filename in filenames) {
    
    reachNangles <- getUSanglesfortask(filename)
    
    if (prod(dim(ppangles)) == 0) {
      ppangles <- reachNangles
    } else {
      ppangles <- rbind(ppangles, reachNangles)
    }
    
  }
  return(ppangles)
  
}

getUSfilenames<- function (ppn, expn) {
  
  if (expn == 1) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 2 Selected Data/'
    
    ppfolder <- sprintf('time_model2_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(2:5)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 4 Raw Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_NoC.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1:4)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 5 Raw & Selected Data/'
    
    ppfolder <- sprintf('time_model_activeloc_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model4_reach_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  }
  return(filenames) 
}






loadUSreachfile <- function(filename) {
  df<-read.table(filename,header= T, sep = "\t", stringsAsFactors = F)
  colnames(df)<-c('participant','block','trial','targetangle_deg','rotation_deg', 'time_ms', 'cursorx_cm','cursory_cm',	'handx_cm',	'handy_cm',	'homex_cm', 'homey_cm',	'targetx_cm','targety_cm','step')
  df$cursory_cm <- df$cursory_cm-395
  df$cursorx_cm <- df$cursorx_cm-528
  df$cursory_cm <- df$cursory_cm*.03065
  df$cursorx_cm <- df$cursorx_cm*.03065
  df$cursorx_cm <- df$cursorx_cm*-1
  df$handy_cm <- df$handy_cm*100
  df$handx_cm <- df$handx_cm*100
  df$handy_cm <- df$handy_cm +8.5
  df$targety_cm <- df$targety_cm-395
  df$targetx_cm <- df$targetx_cm-528
  df$targety_cm <- df$targety_cm*.03065
  df$targetx_cm <- df$targetx_cm*.03065
  df$targetx_cm <- df$targetx_cm*-1
  df$homey_cm <- df$homey_cm-395
  df$homex_cm <- df$homex_cm-528
  df$homey_cm <- df$homey_cm*.03065
  df$homex_cm <- df$homex_cm*.03065
  df$homex_cm <- df$homex_cm*-1  
  df$targetangle_deg <- (-1 *(df$targetangle_deg - 90)) + 90
  return(df)
}


getUSanglesfortask<- function (filename) {
  df<-loadUSreachfile(filename = filename)
  blocknos <- unique(df$block)
  reachdeviations <- c()
  targetangles <- c()
  
  for (blockno in blocknos) {
    trialdf <- df[which(df$block == blockno),]
    angles <- getUSTrialReachAngleAt(trialdf)
    reachdeviations <- c(reachdeviations, angles[1])
    targetangles <- c(targetangles, angles[2])
    
  }
  return(data.frame(reachdeviations,targetangles))
  
  #the below code works to make this function automatically write the file for us, but it needs to make a bunch of files and then put all those together
  #so this isn't the best way yet. 
  #angles<-data.frame(reachdeviations,targetangles)
  #write.csv(angles, file = "aligned_output.csv")
  
}

rotateUSTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates by angle degrees
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  #coordinates <- matrix(data=c(X,Y),ncol=2)
  coordinates <- matrix(data=c(as.numeric(unlist(X)),as.numeric(unlist(Y))),ncol=2)
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


getUSTrialReachAngleAt <- function(trialdf, location='endpoint') {
  
  
  # location (string) determines where the angle of the reach is
  #determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is
  # replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=2)
  
  
  # we extract the target angle
  angle <- trialdf[1,'targetangle_deg']
  
  # so we can at least return that:
  reachangle[1,2] <- angle
  
  # if the trial was rejected, return empty matrix now - Don't include this because data isn't selected
  # if (trialdf[1,'trialselected'] == 0) {
  #   
  #   return(reachangle);
  #   
  # }
  
  # extract the relevant reach information
  
  X <- trialdf['handx_cm']
  Y <- trialdf['handy_cm']
  # MV <- trialdf[trialdf$sampleselected == 1,'maxvelocity']
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateUSTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is
  # reached
  # this assumes that people don't go back, or that there is only one
  #movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  
  return(reachangle)
  
}


