
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


passive55<-passive_loc
passive65<-passive_loc
passive75<-passive_loc
passive85<-passive_loc
passive95<-passive_loc
passive105<-passive_loc
passive115<-passive_loc
passive125<-passive_loc

passive55[pangles != 55]<- NA
passive65[pangles != 65]<- NA
passive75[pangles != 75]<- NA
passive85[pangles != 85]<- NA
passive95[pangles != 95]<- NA
passive105[pangles != 105]<- NA
passive115[pangles != 115]<- NA
passive125[pangles != 125]<- NA

p60<- cbind(passive55, passive65)
p80<- cbind(passive75, passive85)
p100<- cbind(passive95, passive105)
p120<- cbind(passive115, passive125)
p55<-getreachesformodel(p60)
p65<-getreachesformodel(p80)
p75<-getreachesformodel(p100)
p85<-getreachesformodel(p120)


# p55<-getreachesformodel(passive55)
# p65<-getreachesformodel(passive65)
# p75<-getreachesformodel(passive75)
# p85<-getreachesformodel(passive85)
# p95<-getreachesformodel(passive95)
# p105<-getreachesformodel(passive105)
# p115<-getreachesformodel(passive115)
# p125<-getreachesformodel(passive125)








svglite(file='passive localizations by target.svg', width=12, height=16, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4,5,6,7,8), nrow=4, byrow=TRUE), heights=c(2,2,2,2))
plot(x = 1:160, y = p55$meanreaches, type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '55')
plot( x = 1:160, y = p65$meanreaches, col = 'green', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '65')
plot( x = 1:160, y = p75$meanreaches, col = 'red', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '75')
plot( x = 1:160, y = p85$meanreaches, col = 'blue', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '85')
plot( x = 1:160, y = p95$meanreaches, col = 'orange', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '95')
plot( x = 1:160, y = p105$meanreaches, col = 'purple', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '105')
plot( x = 1:160, y = p115$meanreaches, col = 'cyan', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '115')
plot( x = 1:160, y = p125$meanreaches, col = 'Brown', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '125')
dev.off()

svglite(file='passive localizations by reach target.svg', width=16, height=10, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(2,2))
plot(x = 1:160, y = p55$meanreaches, type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '60')
plot( x = 1:160, y = p65$meanreaches, col = 'green', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '80')
plot( x = 1:160, y = p75$meanreaches, col = 'red', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '100')
plot( x = 1:160, y = p85$meanreaches, col = 'blue', type = 'l', ylim = c(-10,20), xlab = "trial", ylab = "Shift in Hand Esimates", main = '120')
dev.off()






locshift<- p55$meanreaches
blocklength=4
block = rep(c(1:(160/blocklength)), each=blocklength)
df<- data.frame(locshift, block)
df<-aggregate(locshift ~ block, data=df, FUN=mean, na.rm=TRUE)
plot(x = 1:40, y = df$locshift, type = 'l', ylim = c(-5,15), xlab = "Block", ylab = "Shift in Hand Esimates [°]", main = "Localization shifts by target across time")
legend(
  0,
  15,
  legend = c('60°', '80°', "100°", "120°"),
  col = c(
    'black', 'red', 'blue', 'green'),
  lty = c(1),
  lwd = c(2),
  bty = 'n', 
  cex = 1
)



locshift<- p65$meanreaches
blocklength=4
block = rep(c(1:(160/blocklength)), each=blocklength)
df<- data.frame(locshift, block)
df<-aggregate(locshift ~ block, data=df, FUN=mean, na.rm=TRUE)
lines(x = 1:40, y = df$locshift, col = "red")

locshift<- p75$meanreaches
blocklength=4
block = rep(c(1:(160/blocklength)), each=blocklength)
df<- data.frame(locshift, block)
df<-aggregate(locshift ~ block, data=df, FUN=mean, na.rm=TRUE)
lines(x = 1:40, y = df$locshift, col = "Blue")

locshift<- p85$meanreaches
blocklength=4
block = rep(c(1:(160/blocklength)), each=blocklength)
df<- data.frame(locshift, block)
df<-aggregate(locshift ~ block, data=df, FUN=mean, na.rm=TRUE)
lines(x = 1:40, y = df$locshift, col = "Green")

