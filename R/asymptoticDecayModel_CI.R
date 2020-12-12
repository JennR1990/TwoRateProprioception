
# single explonential model with asymptote -------------------

asymptoticDecayModel <- function(par, schedule) {
  
  # the process and error states are initialized at 0:
  Pt <- 0
  Et <- 0

  # the total output is stored here:
  output <- c()

  for (t in c(1:length(schedule))) {

    Pt <- Pt - (par['lambda'] * Et)

    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + (schedule[t] * par['N0'])
    }

    # at this point we save the process state in our vector:
    output <- c(output, Pt)

  }
  
  return(data.frame(output=output))
  
  #return(data.frame(output=par['N0'] - (par['N0'] * exp(par['lambda']*schedule))))
  
}

asymptoticDecayMSE <- function(par, schedule, signal, N0=FALSE) {
  
  if (N0) {
    par['N0'] = N0
  }
  
  adm <- asymptoticDecayModel(par, schedule)
  
  #print(c(length(adm$output), length(signal)))
  
  MSE <- mean((adm$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

asymptoticDecayFit <- function(schedule, signal, gridpoints=11, gridfits=10, setAsymptote=FALSE, useOptimx=FALSE) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  maxAsymptote <- 2*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  if (setAsymptote) {
    searchgrid <- expand.grid('lambda' = parvals)
  } else {
    searchgrid <- expand.grid('lambda' = parvals, 
                              'N0'     = parvals * maxAsymptote)
  }
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=asymptoticDecayMSE, MARGIN=c(1), schedule=schedule, signal=signal, N0=setAsymptote)
  
  if (setAsymptote) {
    df <- data.frame('lambda'=searchgrid[order(MSE)[1:gridfits],])
    lower <- c(0)
    upper <- c(1)
  } else {
    df <- data.frame(searchgrid[order(MSE)[1:gridfits],])
    lower <- c(0,0)
    upper <- c(1,maxAsymptote)
  }
  
  # testing if optimx is installed and making it available it so:
  #optimxInstalled <- require("optimx")
  
  if (useOptimx) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( X=df,
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=asymptoticDecayMSE,
                              method='L-BFGS-B',
                              lower=lower,
                              upper=upper,
                              schedule=schedule,
                              signal=signal,
                              N0=setAsymptote ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    if (setAsymptote) {
      return(unlist(win[1]))
    } else {
      return(unlist(win[1:2]))
    }
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optim,
                              fn=asymptoticDecayMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              signal=signal,
                              N0 = setAsymptote ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}

# bootstrapping parameters -----

# bootstrapAsymptoticDecayModels <- function(bootstraps=1000) {
#   
#   groupsignals <- list('active'=c('localization','slowprocess'),
#                        'passive'=c('localization','slowprocess'),
#                        'nocursor'=c('nocursors','slowprocess'))
#   
#   # reversel == 16 trials
#   trialsets <- list('main'=c(1:32), 'reversal'=c(161:176))
#   
#   baselines <- list(
#     'nocursor' = list( 'nocursors'   =32, 'slowprocess'=96 ), 
#     'active'   = list( 'localization'=64, 'slowprocess'=64 ),
#     'passive'  = list( 'localization'=64, 'slowprocess'=64 )
#   )
#   
#   schedules <- list( 
#     'nocursor' = list( 'nocursors'   = -1, 'slowprocess'=  1 ), 
#     'active'   = list( 'localization'=  1, 'slowprocess'=  1 ),
#     'passive'  = list( 'localization'=  1, 'slowprocess'=  1 ) 
#   )
#   
#   participants <- sprintf('p%d',c(1:32))
#   
#   # loop through groups:
#   for (group in names(groupsignals)) {
#     
#     # do each signal for each group
#     for (signalname in groupsignals[[group]]) {
#       
#       # read in the full data set:
#       df <- read.csv(sprintf('data/%s_%s.csv',group,signalname))
#       
#       # determine length of baseline period and schedule-direction:
#       BL <- baselines[[group]][[signalname]]
#       schedulesign <- schedules[[group]][[signalname]]
#       
#       # loop through parts of the signal we want to fit:
#       for (trialset in c('main','reversal')) {
# 
#         # get the part of the data we want to fit:
#         indices <- trialsets[[trialset]] + BL
#         setdf <- df[indices,]
#         
#         # here we store all the bootstrapped parameters:
#         lambda <- c()
#         N0 <- c()
#         
#         # we need to baseline to end of main training for reversal modeling:
#         for (pp in participants) {
#           setdf[,pp] <- setdf[,pp] * schedulesign
#           if (trialset == 'reversal') { # main training is already baselined
#             if (signalname == 'slowprocess') {
#               setdf[,pp] <- setdf[,pp] - df[,pp][ min(indices) - 1 ]
#             } else {
#               a_i <- c(81:160) + BL
#               asymptote <- (mean(df[,pp][a_i], na.rm=TRUE) * schedulesign)
#               setdf[,pp] <- setdf[,pp] - asymptote
#             }
#             setdf[,pp] <- setdf[,pp] * -1
#           }
#         }
#         # baselining done
#         
#         # schedule is a vector of values -1 and length the same as the signal:
#         schedule <- rep(-1, dim(setdf)[1])
#         
#         # bootstrap parameters, by resampling participants:
#         for (bs in c(1:bootstraps)) {
#         
#           cat(sprintf('group: %s, signal: %s, set: %s, bootstrap: %d/%d\n', group, signalname, trialset, bs, bootstraps))
#           
#           signal <- apply(setdf[sample(participants, replace=TRUE)], MARGIN=1, FUN=mean, na.rm=TRUE)
#           
#           par <- asymptoticDecayFit(schedule=schedule, signal=signal)
#           
#           #plot(signal, type='l', main=par)
#           #print(par)
#           
#           lambda <- c(lambda, par['lambda'])
#           N0 <- c(N0, par['N0'])
# 
#         }
#         
#         write.csv(data.frame(lambda, N0), file=sprintf('data/%s_%s_%s.csv',group,signalname,trialset), quote=F, row.names=F)
#         
#       }
#       
#     }
#     
#   }
#   
# }

asymptoticDecaySettings <- function() {
  
  # this list determines which signals get done for each group
  groupsignals <- list(
    'active'        = c('localization', 'slowprocess', 'reaches'),
    'passive'       = c('localization', 'slowprocess', 'reaches'),
#    'nocursor'      = c('nocursors',    'slowprocess', 'reaches'),
    'nocursor-47'   = c('nocursors',    'slowprocess', 'reaches'),
    'nocursor-ni32' = c('nocursors',    'slowprocess', 'reaches'),
#    'nocursor-in16' = c('nocursors',    'slowprocess', 'reaches'),
    'nocursor-in15' = c('nocursors',    'slowprocess', 'reaches'),
    'pause'         = c('reaches',      'slowprocess' )
  )
  # this list determines which signals get done for each group


  
  # we used to run it on the reversal phase too, but it takes so much time...
  trialsets <- list('main'=c(1:160), 'reversal'=c(161:176))

  baselines <- list(
#    'nocursor'      = list( 'nocursors'   =32, 'slowprocess'=96, 'reaches'=96 ), 
    'nocursor-47'   = list( 'nocursors'   =32, 'slowprocess'=96, 'reaches'=96 ), 
    'nocursor-ni32' = list( 'nocursors'   =32, 'slowprocess'=96, 'reaches'=96 ), 
#    'nocursor-in16' = list( 'nocursors'   =32, 'slowprocess'=96, 'reaches'=96 ), 
    'nocursor-in15' = list( 'nocursors'   =32, 'slowprocess'=96, 'reaches'=96 ), 
    'active'        = list( 'localization'=64, 'slowprocess'=64, 'reaches'=64 ),
    'passive'       = list( 'localization'=64, 'slowprocess'=64, 'reaches'=64 ),
    'pause'         = list(                    'slowprocess'=64, 'reaches'=96 )
  )
  
  schedules <- list( 
#    'nocursor'      = list( 'nocursors'   = -1, 'slowprocess'=  1, 'reaches'= -1 ), 
    'nocursor-47'   = list( 'nocursors'   = -1, 'slowprocess'=  1, 'reaches'= -1 ), 
    'nocursor-ni32' = list( 'nocursors'   = -1, 'slowprocess'=  1, 'reaches'= -1 ), 
#    'nocursor-in16' = list( 'nocursors'   = -1, 'slowprocess'=  1, 'reaches'= -1 ), 
    'nocursor-in15' = list( 'nocursors'   = -1, 'slowprocess'=  1, 'reaches'= -1 ), 
    'active'        = list( 'localization'=  1, 'slowprocess'=  1, 'reaches'= -1 ),
    'passive'       = list( 'localization'=  1, 'slowprocess'=  1, 'reaches'= -1 ),
    'pause'         = list(                     'slowprocess'=  1, 'reaches'= -1 )
  )
  
  optimxInstalled <- require("optimx")
  if (optimxInstalled) {
    useOptimx <- TRUE
  } else {
    useOptimx <- FALSE
  }

  settings <- list()
  settings[['groupsignals']] <- groupsignals
  settings[['trialsets']]    <- trialsets
  settings[['baselines']]    <- baselines
  settings[['schedules']]    <- schedules
  settings[['FUN']]          <- mean
  settings[['useOptimx']]    <- useOptimx

  return(settings)
  
}

bootstrapSemiAsymptoticDecayModels <- function(bootstraps=1000) {
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  # loop through groups:
  for (group in names(groupsignals)) {
    
    participants <- sprintf('p%d',c(1:32))
    if (group == 'nocursor') {
      participants <- sprintf('p%d',c(1:48))
    }
    if (group == 'nocursor-in16') {
      participants <- sprintf('p%d',c(1:16))
    }
    if (group == 'nocursor-47') {
      participants <- sprintf('p%d',c(1:39,41:48))
    }
    if (group == 'nocursor-in15') {
      participants <- sprintf('p%d',c(1:7,9:16))
    }
    
    # do each signal for each group
    for (signalname in groupsignals[[group]]) {
      
      # if (signalname != 'reaches') {
      #   next()
      # }
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      #print(leadingzero)
      
      # read in the full data set:
      df <- read.csv(sprintf('data/%s_%s.csv',group,signalname))
      df <- df[,participants]
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[group]][[signalname]]
      schedulesign <- schedules[[group]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in names(trialsets)) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        #print(indices)
        setdf <- df[indices,]
        
        # here we store all the bootstrapped parameters:
        lambda <- c()
        N0 <- c()
        
        # we need to baseline to end of main training for reversal modeling:
        for (pp in participants) {
          setdf[,pp] <- setdf[,pp] * schedulesign
          if (trialset == 'reversal') { # main training is already baselined
            if (signalname == 'slowprocess') {
              setdf[,pp] <- setdf[,pp] - df[,pp][ min(indices) - 1 ]
            } else {
              a_i <- c(81:160) + BL
              asymptote <- (mean(df[,pp][a_i], na.rm=TRUE) * schedulesign)
              setdf[,pp] <- setdf[,pp] - asymptote
            }
            setdf[,pp] <- setdf[,pp] * -1
          }
        }
        # baselining done
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedulelength <- dim(setdf)[1]
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- rep(-1, schedulelength)
        #print(schedulelength)
        
        # if in reversal phase, we want to use the asymptote from the main rotation
        # which we get from the whole data
        if (trialset == 'reversal') {
          # get the part of the data we want to fit:
          Aindices <- trialsets[['main']] + BL
          #print(Aindices)
          Asetdf <- df[Aindices,]
          for (pp in participants) {
            Asetdf[,pp] <- Asetdf[,pp] * schedulesign
          }
          Aschedulelength <- dim(Asetdf)[1]
          if (leadingzero) {Aschedulelength <- Aschedulelength + 1}
          Aschedule <- rep(-1, Aschedulelength)
          Asignal <- apply(Asetdf, MARGIN=1, FUN=FUN, na.rm=TRUE)
          if (leadingzero) {Asignal <- c(0, Asignal)}
          par <- asymptoticDecayFit(schedule=Aschedule, signal=Asignal, useOptimx=useOptimx)
          
          #print(par)
          
          # twice as large! (will reduce the fitted lambda, but that makes sense...)
          setAsymptote <- par['N0'] * 2
          
        } else {
          setAsymptote <- FALSE
        }
        
        
        # bootstrap parameters, by resampling participants:
        for (bs in c(1:bootstraps)) {
          
          cat(sprintf('group: %s, signal: %s, set: %s, bootstrap: %d/%d\n', group, signalname, trialset, bs, bootstraps))
          
          signal <- apply(setdf[sample(participants, replace=TRUE)], MARGIN=1, FUN=FUN, na.rm=TRUE)
          if (leadingzero) {signal <- c(0, signal)}
          
          #print(c(length(signal), length(schedule)))
          
          par <- asymptoticDecayFit(schedule=schedule, signal=signal, setAsymptote=setAsymptote, useOptimx=useOptimx)
          
          #plot(signal, type='l', main=par)
          #print(par)
          
          lambda <- c(lambda, par['lambda'])
          if (trialset == 'main') {
            N0 <- c(N0, par['N0'])
          } else {
            N0 <- c(N0, setAsymptote)
          }
          
        }
        
        write.csv(data.frame(lambda, N0), file=sprintf('data/%s_%s_%s_semi.csv',group,signalname,trialset), quote=F, row.names=F)
        
      }
      
    }
    
  }
  
}

getAsymptoticDecayParameterCIs <- function(semi=TRUE) {
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  
  group <- c()
  signal <- c()
  phase <- c()
  
  lambda <- c()
  lambda_025 <- c()
  lambda_500 <- c()
  lambda_975 <- c()
  
  N0 <- c()
  N0_025 <- c()
  N0_500 <- c()
  N0_975 <- c()
  
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    participants <- sprintf('p%d',c(1:32))
    if (groupname == 'nocursor') {
      participants <- sprintf('p%d',c(1:48))
    }
    if (groupname == 'nocursor-in16') {
      participants <- sprintf('p%d',c(1:16))
    }
    if (groupname == 'nocursor-47') {
      participants <- sprintf('p%d',c(1:39,41:48))
    }
    if (groupname == 'nocursor-in15') {
      participants <- sprintf('p%d',c(1:7,9:16))
    }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      cat(sprintf('%s %s\n', groupname, signalname))
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      # read in the full data set:
      rawdf <- read.csv(sprintf('data/%s_%s.csv',groupname,signalname))
      rawdf <- rawdf[,participants]
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      
      # loop through parts of the signal we want to fit:
      for (trialset in names(trialsets)) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        setdf <- rawdf[indices,]
        
        # we need to baseline to end of main training for reversal modeling:
        for (pp in participants) {
          setdf[,pp] <- setdf[,pp] * schedulesign
          if (trialset == 'reversal') { # main training is already baselined
            if (signalname == 'slowprocess') {
              setdf[,pp] <- setdf[,pp] - rawdf[,pp][ min(indices) - 1 ]
            } else {
              a_i <- c(81:160) + BL
              asymptote <- (mean(rawdf[,pp][a_i], na.rm=TRUE) * schedulesign)
              setdf[,pp] <- setdf[,pp] - asymptote
            }
            setdf[,pp] <- setdf[,pp] * -1
          }
        }
        # baselining done
        
        if (trialset == 'reversal' & semi) {
          # get the part of the data we want to fit:
          Aindices <- trialsets[['main']] + BL
          Asetdf <- rawdf[Aindices,]
          for (pp in participants) {
            Asetdf[,pp] <- Asetdf[,pp] * schedulesign
          }
          Aschedulelength <- dim(Asetdf)[1]
          if (leadingzero) {Aschedulelength <- Aschedulelength + 1}
          Aschedule <- rep(-1, Aschedulelength)
          #Aschedule <- rep(-1, dim(Asetdf)[1]+1)
          Asignal <- apply(Asetdf, MARGIN=1, FUN=FUN, na.rm=TRUE)
          if (leadingzero) {Asignal <- c(0, Asignal)}
          par <- asymptoticDecayFit(schedule=Aschedule, signal=Asignal, useOptimx=useOptimx)
          
          # twice as large! (will reduce the fitted lambda, but that makes sense...)
          setAsymptote <- par['N0'] * 2
          
        } else {
          setAsymptote <- FALSE
        }
        
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedule <- rep(-1, dim(setdf)[1])
        if (leadingzero) {schedule <- c(0, schedule)}
        
        # this gets the overal parameters on the group median data:
        datasignal <- apply(setdf, MARGIN=1, FUN=FUN, na.rm=TRUE)
        if (leadingzero) {datasignal <- c(0, datasignal)}
        par <- asymptoticDecayFit(schedule=schedule, signal=datasignal, setAsymptote=setAsymptote, useOptimx=useOptimx)
        
        
        #print(length(apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE)))
        #plot(apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE), type='l', main=par)
        
        
        # read in the bootstrapped parameter values:
        #sstr <- '_semi' if (semi) else ''
        if (semi) {
          sstr <- '_semi'
        } else {
          sstr <- ''
        }
        df <- read.csv(sprintf('data/%s_%s_%s%s.csv', groupname, signalname, trialset, sstr), stringsAsFactors = F)
        
        group <- c(group, groupname)
        signal <- c(signal, signalname)
        phase <- c(phase, trialset)
        
        qs <- quantile(df$lambda, probs = c(0.025, 0.500, 0.975))
        
        lambda <- c(lambda, as.numeric(par['lambda']))
        lambda_025 <- c(lambda_025, qs[1])
        lambda_500 <- c(lambda_500, qs[2])
        lambda_975 <- c(lambda_975, qs[3])
        
        qs <- quantile(df$N0, probs = c(0.025, 0.500, 0.975))
        
        N0 <- c(N0, as.numeric(par['N0']))
        N0_025 <- c(N0_025, qs[1])
        N0_500 <- c(N0_500, qs[2])
        N0_975 <- c(N0_975, qs[3])
        
      }
      
    }
    
  }
  
  # write output
  write.csv(data.frame( group, signal, phase,
                        lambda, lambda_025, lambda_500, lambda_975,
                        N0, N0_025, N0_500, N0_975),
            file='data/asymptoticDecayParameterCIs.csv',
            quote = F, row.names = F)
     
}

getSaturationTrials <- function(criterion='CI') {
  
  df <- read.csv('data/asymptoticDecayParameterCIs.csv', stringsAsFactors = F)
  df <- df[which(df$phase == 'main'),]
  
  settings <- asymptoticDecaySettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  group <- c()
  signal <- c()
  avg <- c()
  lwr <- c()
  upr <- c()
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    # DON'T NEED PARTICIPANTS HERE:    
    # participants <- sprintf('p%d',c(1:32))
    # if (groupname == 'nocursor') {
    #   participants <- sprintf('p%d',c(1:48))
    # }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      # read in the full data set:
      #rawdf <- read.csv(sprintf('data/%s_%s.csv',groupname,signalname))
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      
      # loop through parts of the signal we want to fit:
      #for (trialset in c('main','reversal')) {
      for (trialset in c('main')) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        #setdf <- rawdf[indices,]
        
        # schedule is a vector of values -1 and length the same as the signal:
        #schedulelength <- dim(setdf)[1]
        schedulelength <- length(indices)
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- rep(-1, schedulelength)
        
        # this gets the overal parameters on the group median data:
        #par <- asymptoticDecayFit(schedule=schedule, signal=apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE), setAsymptote=setAsymptote)
        
        trialnos <- list()
        
        for (roc in c('lambda','lambda_975','lambda_025')) {
          
          par <- c('lambda'=df[which(df$group == groupname & df$signal == signalname),roc], 'N0'=df[which(df$group == groupname & df$signal == signalname),'N0'])
          
          fitdf <- asymptoticDecayModel(par=par, schedule=schedule)
          
          if (is.numeric( criterion )) {
            crit <- (par['N0'] * criterion)
          }
          if ( criterion == 'CI' ) {
            crit <- df$N0_025[which(df$group == groupname & df$signal == signalname)]
          }
          
          trialno <- which(fitdf$output > crit)[1]
          
          # subtract 1 from trial no, as first trial depends on feedback from previous phase
          trialno <- trialno - 1
          
          trialnos[[roc]] <- trialno
          
        }
        #print(trialnos[['lambda']])
        
        group <- c(group, groupname)
        signal <- c(signal, signalname)
        avg <- c(avg, trialnos[['lambda']])
        lwr <- c(lwr, trialnos[['lambda_975']])
        upr <- c(upr, trialnos[['lambda_025']])
        
        cat(sprintf('%s, %s: trial %d (%d - %d)\n', groupname, signalname, trialnos[['lambda']], trialnos[['lambda_975']], trialnos[['lambda_025']]))

      }
      
    }
    
  }
  
  df <- data.frame(group, signal, avg, lwr, upr)
  
  write.csv(df, 'data/saturation_trials.csv', row.names = FALSE, quote = FALSE)
  
}

