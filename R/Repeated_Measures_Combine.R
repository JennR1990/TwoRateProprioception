
mmed <- function(x,n=5){runmed(x,n)}




##repeated Measures Combine function

TCombine1or5<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
  Aligned<- mean(unlist(data[61:64,participant]), na.rm = TRUE)
  r1<- unlist(data[65,participant])
  r2<- unlist(data[66,participant])
  r3<- unlist(data[67,participant])
  r4<- unlist(data[68,participant])
  R1_Early<- mean(unlist(data[65:68,participant]), na.rm = TRUE)
  R1_Late<- mean(unlist(data[208:224,participant]), na.rm = TRUE)
  R2<- mean(unlist(data[237:240,participant]), na.rm = TRUE)
  EC<- mean(unlist(data[241:244,participant]), na.rm = TRUE)
  EC1<- mean(unlist(data[241,participant]), na.rm = TRUE)
  EC2<- mean(unlist(data[242,participant]), na.rm = TRUE)
  EC3<- mean(unlist(data[243,participant]), na.rm = TRUE)
  EC4<- mean(unlist(data[244,participant]), na.rm = TRUE)
  EC_Late<- mean(unlist(data[256:288,participant]), na.rm = TRUE)
  
  RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
  
  if (prod(dim(ParticipantRM)) == 0) {
    ParticipantRM <- RM
  } else {
    ParticipantRM <- rbind(ParticipantRM, RM)
  }
  }
  
  
  return(ParticipantRM)
  
  
}


TCombine2or4<- function(data) {
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
    EC_Late<- mean(unlist(data[288:320,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[273,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[274,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[275,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[276,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  
  
  return(ParticipantRM)
  
  
}


##repeated measures combine for ANOVAs

ANOVAcombine1or5<- function(data) {
  ParticipantARM<- data.frame()
  participants <- c(2:ncol(data))
  participants <- names(data)[2:dim(data)[2]]
  
  epochs <- list('R1_early'=c(65,6), 'R1_late'=c(208,16), 'R2D2'=c(237,4), 'EC'=c(256,32))
  Reaches<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){

    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      # mr <- mean(participant_reaches[start:finish], na.rm=TRUE)
      
      Reaches <- c(Reaches, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      
      
    }
    
    # Reaches[1]<- mean(unlist(data[61:64,participant]), na.rm = TRUE)
    # 
    # Time[1] <-'Aligned'
    # Reaches[2]<- unlist(data[65,participant])
    # Time[2] <-'r1'
    # Reaches[3]<- unlist(data[66,participant])
    # Time[3] <-'r2'
    # Reaches[4]<- unlist(data[67,participant])
    # Time[4] <-'r3'
    # Reaches[5]<- unlist(data[68,participant])
    # Time[5] <-'r4'
    # Reaches[6]<- mean(unlist(data[65:68,participant]), na.rm = TRUE)
    # Time[6] <-'R1_Early'
    # Reaches[7]<- mean(unlist(data[208:224,participant]), na.rm = TRUE)
    # Time[7] <-'R1_Late'
    # Reaches[8]<- mean(unlist(data[237:240,participant]), na.rm = TRUE)
    # Time[8] <-'R2'
    # Reaches[9]<- mean(unlist(data[241:244,participant]), na.rm = TRUE)
    # Time[9] <-'EC'
    # Reaches[10]<- mean(unlist(data[241,participant]), na.rm = TRUE)
    # Time[10] <-'EC1'
    # Reaches[11]<- mean(unlist(data[242,participant]), na.rm = TRUE)
    # Time[11] <-'EC2'
    # Reaches[12]<- mean(unlist(data[243,participant]), na.rm = TRUE)
    # Time[12] <-'EC3'
    # Reaches[13]<- mean(unlist(data[244,participant]), na.rm = TRUE)
    # Time[13] <-'EC4'
    # Reaches[14]<- mean(unlist(data[256:288,participant]), na.rm = TRUE)
    # Time[14] <-'EC_Late'
    # ID[1:14]<- sprintf('%s %d', data, participant)
    # ANOVARM<- data.frame(Reaches, Time, ID)
    #RM<- rbind(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    
    # if (prod(dim(ParticipantARM)) == 0) {
    #   ParticipantARM <- ANOVARM
    # } else {
    #   ParticipantARM <- rbind(ParticipantARM, ANOVARM)
    # }
  }
  
  ANOVARM<- data.frame(Reaches, Time, ID)
  
  return(ANOVARM)
  
  
}


ANOVAcombine2or4<- function(data) {
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
    EC_Late<- mean(unlist(data[288:320,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[273,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[274,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[275,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[276,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  
  
  return(ParticipantRM)
  
  
}



RepeatedMeasuresCombine4orNC<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[29:32,participant]), na.rm = TRUE)
    r1<- unlist(data[33,participant])
    r2<- unlist(data[34,participant])
    r3<- unlist(data[35,participant])
    r4<- unlist(data[36,participant])
    R1_Early<- mean(unlist(data[33:36,participant]), na.rm = TRUE)
    R1_Late<- mean(unlist(data[189:192,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[205:208,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[209:212,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[253:256,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[209,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[210,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[211,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[212,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  
  
  return(ParticipantRM)
  
  
}
#write.csv(PPrm, "Passive Prop RM Data-July23.csv", quote = FALSE, row.names = FALSE)
