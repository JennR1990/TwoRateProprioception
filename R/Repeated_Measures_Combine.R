
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


RepeatedMeasuresCombine2or4<- function(data) {
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
