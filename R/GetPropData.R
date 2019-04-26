getallTapparticipants<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:32)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48)) 
  } else if (experiment == 3) {
    participants<- c(1:9)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 4) {
    participants<- c(1:9)
    distortion <-  c(rep(0,96),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 5) {
    participants<- c(1, 3:9)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 6) {
    participants<- c(1, 3:9)
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
    bias<-mean(df$Tapdeviations[32:96], na.rm = TRUE)
    df$Tapdeviations[1:320]<- df$Tapdeviations[1:320] - bias
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
    tasknumbers <- c(1:5)
    
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
    
    expfolder <- '../Time Model/Time Model Variant 5 Selected Data/'
    
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
  # blocknos <- unique(df$block)
  # Tapdeviations <- c()
  # targetangles <- c()
  # 
  # for (blockno in blocknos) {
  #   trialdf <- df[which(df$block == blockno),]
  #   angles <- getTrialTapAngleAt(trialdf)
  #   Tapdeviations <- c(Tapdeviations, angles[1])
  #   targetangles <- c(targetangles, angles[2])
  #   
  
  return(data.frame(Tapdeviations,targetangles, selected))
  
  #the below code works to make this function automatically write the file for us, but it needs to make a bunch of files and then put all those together
  #so this isn't the best way yet. 
  #angles<-data.frame(reachdeviations,targetangles)
  #write.csv(angles, file = "aligned_output.csv")
  
}






getallTap<- function(experiment) {
  
  if (experiment == 1){
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 2) {
    participants<- c(1:12,14:32)
    distortion <-  c(rep(0,112)) 
  } else if (experiment == 3) {
    participants<- c(1:9)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 4) {
    participants<- c(1:32)
    distortion <-  c(rep(0,112))
  } else if (experiment == 5) {
    participants<- c(1:32)
    distortion <-  c(rep(0,64),rep(30,160), rep(-30,16), rep(NA, 48))
  } else if (experiment == 6) {
    participants<- c(1:14)
    distortion <-  c(rep(0,112))
  }
  
  expangles<- data.frame(distortion)
  
  for (participant in participants){
    partiangles<-getparticipantpropdata(participant = participant, experiment = experiment)
    #baselining only works for experiment 1&5, 2 & 4 only have 32 aligned in total 
    #so they would need different amount of trials
    #baselinedangles <- baselineTapbyaligned(df = partiangles, experiment = experiment)
    #expangles[,sprintf('p%d',participant)] <- baselinedangles$Tapdeviations
    print(dim(partiangles))
    print(head(partiangles))
    expangles[,sprintf('p%d',participant)] <- partiangles
    print(head(expangles))
  }
  outputfilename<- sprintf('time_model%d_pre_post_Prop.csv', experiment)
  
  write.csv(expangles, file = outputfilename,  row.names = F, quote = F)
}






baselineTapbyaligned<- function(df, experiment) {
  
  #take an average of the aligned data where distortion = 0
  #subtract that from reach angles
  if (experiment == 1| experiment == 5) {
    bias<-mean(df$Tapdeviations[32:64], na.rm = TRUE)
    df$Tapdeviations[1:288]<- df$Tapdeviations[1:288] - bias
  } else if (experiment == 2 | experiment == 4) {
    bias<-mean(df$Tapdeviations[32:96], na.rm = TRUE)
    df$Tapdeviations[1:320]<- df$Tapdeviations[1:320] - bias
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
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model/Time Model Variant 1 Selected Data/'
    
    ppfolder <- sprintf('time_model1_%d/',ppn)
    
    
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 2) {
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 2 Selected Data/'
    
    ppfolder <- sprintf('time_model2_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 3) {
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model/Time Model Variant 3 Selected Data/'
    
    ppfolder <- sprintf('time_model3_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model_3_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 4) {
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model Good Data/Time Model Variant 4 Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 5) {
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model/Time Model Variant 5 Selected Data/'
    
    ppfolder <- sprintf('time_model_activeloc_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model4_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
    }
  } else if (expn == 6) {
    tasknumbers <- c(1,6)
    
    expfolder <- '../Time Model Good Data/Time Model - No Cursor New Instructions Selected Data/'
    
    ppfolder <- sprintf('time_model_nocursor_%d/',ppn)
    filenames <- c()
    
    for (taskno in tasknumbers) {
      
      filenames <- c(filenames, sprintf('%s%s%d_%d__time_model2_Prop_selected.txt',expfolder,ppfolder,ppn,taskno))
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
  # blocknos <- unique(df$block)
  # Tapdeviations <- c()
  # targetangles <- c()
  # 
  # for (blockno in blocknos) {
  #   trialdf <- df[which(df$block == blockno),]
  #   angles <- getTrialTapAngleAt(trialdf)
  #   Tapdeviations <- c(Tapdeviations, angles[1])
  #   targetangles <- c(targetangles, angles[2])
  #   
  
  return(data.frame(Tapdeviations,targetangles, selected))
  
  #the below code works to make this function automatically write the file for us, but it needs to make a bunch of files and then put all those together
  #so this isn't the best way yet. 
  #angles<-data.frame(reachdeviations,targetangles)
  #write.csv(angles, file = "aligned_output.csv")
  
}










