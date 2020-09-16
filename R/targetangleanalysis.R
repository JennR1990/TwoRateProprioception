
plottargetanglesprop<- function (loc, angledata, color, title){

  
loc$distortion[is.na(loc$distortion)]<- 0
locdata<- loc[loc$distortion == 30, 2:length(loc)]

x<- 1:8
i = 1
y<- c()
SEs<-c()
angles<- sort(unique(angledata[,2]))
for (angle in angles) {
  print(angle)
  
y[i]<- mean(locdata[angledata == angle], na.rm = TRUE)
SEs[i]<- (sd(locdata[angledata == angle], na.rm = TRUE)/ sqrt(length(locdata[angledata == angle]))) * 3
i = i +1
}


plot(x, y, ylim = c(4,12), axes = FALSE, ylab = "Error in Estimated Hand Location [°]", xlab = "Target Angle [°]", main = title , col = color)
arrows(x0 = x, y0=y - SEs,  x1 = x, y1 = y + SEs, code = 3, angle = 90, length = .1, col = color)
axis(1, at=c(1,2,3,4,5,6,7,8), labels = angles)
axis(2, at= 4:12, labels = 4:12, las = 1)


}

plotbothtargetanglesprop<- function (loc, angledata, color, loc2, angledata2, color2){
  
  
  loc$distortion[is.na(loc$distortion)]<- 0
  locdata<- loc[loc$distortion == 30, 2:length(loc)]
  
  angledata$distortion[is.na(angledata$distortion)]<- 0
  angledata<- angledata[angledata$distortion == 30, 2:length(angledata)]
  
  x<- 1:8
  i = 1
  y<- c()
  SEs<- c()
    angles<- sort(unique(angledata[,2]))
  for (angle in angles) {
    print(angle)
    
    y[i]<- mean(locdata[angledata == angle], na.rm = TRUE)
    SEs[i]<- (sd(locdata[angledata == angle], na.rm = TRUE)/ sqrt(length(locdata[angledata == angle]))) * 3
    i = i +1
  }
  
  
  plot(x, y, ylim = c(0,15), axes = FALSE, ylab = "Shifts in Estimated Hand Location [°]", xlab = "Target Angle [°]", main = "Localization Shifts By Target" , col = color)
  arrows(x0 = x, y0=y - SEs,  x1 = x, y1 = y + SEs, code = 3, angle = 90, length = .1, col = color)
  axis(1, at=c(1,2,3,4,5,6,7,8), labels = angles)
  axis(2, at= 0:15, labels = 0:15, las = 1)
  
  legend(
    1,
    14,
    legend = c('Active', 'Passive'),
    col = c(
color,color2    ),
    lty = c(1),
    lwd = c(2),
    bty = 'n', 
    cex = 1
  )
  
  
  angledata2$distortion[is.na(angledata2$distortion)]<- 0
  angledata2<- angledata2[angledata2$distortion == 30, 2:length(angledata2)]
  
  loc2$distortion[is.na(loc2$distortion)]<- 0
  locdata2<- loc2[loc2$distortion == 30, 2:length(loc2)]
  i = 1
  y<- c()
  SEs<-
    angles<- sort(unique(angledata2[,2]))
  for (angle in angles) {
    print(angle)
    
    y[i]<- mean(locdata2[angledata2 == angle], na.rm = TRUE)
    SEs[i]<- (sd(locdata2[angledata2 == angle], na.rm = TRUE)/ sqrt(length(locdata2[angledata2 == angle]))) * 3
    i = i +1
  }
  points(x = x, y = y, col = color2)
  arrows(x0 = x, y0=y - SEs,  x1 = x, y1 = y + SEs, code = 3, angle = 90, length = .1, col = color2)
  
}



TargetANOVAcombine<- function(data) {
  
  
  data$distortion[is.na(data$distortion)]<- 0
  ParticipantARM<- data.frame()
  participants <- names(data)[2:dim(data)[2]]
  epochs <- list('55', '65', '75', '85', '95', '105', '115', '125')
  Reaches<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[data$distortion == 30,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Reaches <- c(Reaches, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Reaches, Time, ID)
    }
  }
  #b<- !is.nan(ANOVARM$Reaches)
  # ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}


