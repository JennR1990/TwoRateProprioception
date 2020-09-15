
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
  
  MSE <- mean((asymptoticDecayModel(par, schedule)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

asymptoticDecayFit <- function(schedule, signal, gridpoints=11, gridfits=10, setAsymptote=FALSE) {
  
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
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
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

bootstrapSemiAsymptoticDecayModels <- function(bootstraps=1000) {
  
  groupsignals <- list('active'=c('localization','slowprocess'),
                       'passive'=c('localization','slowprocess'),
                       'nocursor'=c('nocursors','slowprocess'))
  
  # reversel == 16 trials
  trialsets <- list('main'=c(1:160), 'reversal'=c(161:176))
  
  baselines <- list(
    'nocursor' = list( 'nocursors'   =32, 'slowprocess'=96 ), 
    'active'   = list( 'localization'=64, 'slowprocess'=64 ),
    'passive'  = list( 'localization'=64, 'slowprocess'=64 )
  )
  
  schedules <- list( 
    'nocursor' = list( 'nocursors'   = -1, 'slowprocess'=  1 ), 
    'active'   = list( 'localization'=  1, 'slowprocess'=  1 ),
    'passive'  = list( 'localization'=  1, 'slowprocess'=  1 ) 
  )
  
  participants <- sprintf('p%d',c(1:32))
  
  # loop through groups:
  for (group in names(groupsignals)) {
    
    # do each signal for each group
    for (signalname in groupsignals[[group]]) {
      
      # read in the full data set:
      df <- read.csv(sprintf('data/%s_%s.csv',group,signalname))
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[group]][[signalname]]
      schedulesign <- schedules[[group]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in c('main','reversal')) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        print(indices)
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
        schedule <- rep(-1, dim(setdf)[1])
        
        # if in reversal phase, we want to use the asymptote from the main rotation
        # which we get from the whole data
        if (trialset == 'reversal') {
          # get the part of the data we want to fit:
          Aindices <- trialsets[['main']] + BL
          print(Aindices)
          Asetdf <- df[Aindices,]
          for (pp in participants) {
            Asetdf[,pp] <- Asetdf[,pp] * schedulesign
          }
          Aschedule <- rep(-1, dim(Asetdf)[1])
          par <- asymptoticDecayFit(schedule=Aschedule, signal=apply(Asetdf, MARGIN=1, FUN=mean, na.rm=TRUE))
          
          print(par)
          
          # twice as large! (will reduce the fitted lambda, but that makes sense...)
          setAsymptote <- par['N0'] * 2
          
        } else {
          setAsymptote <- FALSE
        }
        
        
        # bootstrap parameters, by resampling participants:
        for (bs in c(1:bootstraps)) {
          
          cat(sprintf('group: %s, signal: %s, set: %s, bootstrap: %d/%d\n', group, signalname, trialset, bs, bootstraps))
          
          signal <- apply(setdf[sample(participants, replace=TRUE)], MARGIN=1, FUN=mean, na.rm=TRUE)
          
          par <- asymptoticDecayFit(schedule=schedule, signal=signal, setAsymptote=setAsymptote)
          
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
  
  groupsignals <- list('active'=c('localization','slowprocess'),
                       'passive'=c('localization','slowprocess'),
                       'nocursor'=c('nocursors','slowprocess'))
  
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
  
  
  # reversal == 16 trials
  trialsets <- list('main'=c(1:160), 'reversal'=c(161:176))
  
  baselines <- list(
    'nocursor' = list( 'nocursors'   =32, 'slowprocess'=96 ), 
    'active'   = list( 'localization'=64, 'slowprocess'=64 ),
    'passive'  = list( 'localization'=64, 'slowprocess'=64 )
  )
  
  schedules <- list( 
    'nocursor' = list( 'nocursors'   = -1, 'slowprocess'=  1 ), 
    'active'   = list( 'localization'=  1, 'slowprocess'=  1 ),
    'passive'  = list( 'localization'=  1, 'slowprocess'=  1 ) 
  )
  
  participants <- sprintf('p%d',c(1:32))
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      # read in the full data set:
      rawdf <- read.csv(sprintf('data/%s_%s.csv',groupname,signalname))
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      
      # loop through parts of the signal we want to fit:
      for (trialset in c('main','reversal')) {
        
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
          Aschedule <- rep(-1, dim(Asetdf)[1])
          par <- asymptoticDecayFit(schedule=Aschedule, signal=apply(Asetdf, MARGIN=1, FUN=mean, na.rm=TRUE))
          
          # twice as large! (will reduce the fitted lambda, but that makes sense...)
          setAsymptote <- par['N0'] * 2
          
        } else {
          setAsymptote <- FALSE
        }
        
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedule <- rep(-1, dim(setdf)[1])
        
        # this gets the overal parameters on the group median data:
        par <- asymptoticDecayFit(schedule=schedule, signal=apply(setdf, MARGIN=1, FUN=mean, na.rm=TRUE), setAsymptote=setAsymptote)
        
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
