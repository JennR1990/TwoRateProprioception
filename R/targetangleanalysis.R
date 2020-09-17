
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

##i want to do an anova on the four targets and that will be within subjects and then do a factor of experiment which is between subjects and is passive or active




ANOVAanalysis<- function(AllDataANOVA){
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Target<- as.factor(AllDataANOVA$Target)
  AllDataANOVA$Experiment<- as.factor(AllDataANOVA$Experiment)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=locs,
                       wid=ID,
                       within=Target,
                       between = Experiment,
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}

PrepTargetANOVA<-function() {
Target<- rep(c(rep(60, times = 32),rep(80, times = 32),rep(100, times = 32),rep(120, times = 32)), times = 2)
Experiment<- c(rep("Passive", times = 128), rep("Active", times = 128))
ID<- rep(1:32, times = 8)

plocalizations<-getaveragespertarget(passive_loc, pangles)
Alocalizations<-getaveragespertarget(active_loc, Aangles)
locs<- c(plocalizations, Alocalizations)

return(data.frame(Target, locs, ID, Experiment))

}

getaveragespertarget<- function (data, angles){

T55<-data
T65<-data
T75<-data
T85<-data
T95<-data
T105<-data
T115<-data
T125<-data

T55[angles != 55]<- NA
T65[angles != 65]<- NA
T75[angles != 75]<- NA
T85[angles != 85]<- NA
T95[angles != 95]<- NA
T105[angles != 105]<- NA
T115[angles != 115]<- NA
T125[angles != 125]<- NA


T60<- rbind(T55, T65)
T80<- rbind(T75, T85)
T100<- rbind(T95, T105)
T120<- rbind(T115, T125)


T60means<-as.vector(colMeans(T60[,2:33], na.rm = TRUE))
T80means<-as.vector(colMeans(T80[,2:33], na.rm = TRUE))
T100means<-as.vector(colMeans(T100[,2:33], na.rm = TRUE))
T120means<-as.vector(colMeans(T120[,2:33], na.rm = TRUE))



Locs<- c(T60means, T80means, T100means, T120means)

return(Locs)
}




getaveragesbyblockpertarget<- function (data, angles){
  
  T55<-data
  T65<-data
  T75<-data
  T85<-data
  T95<-data
  T105<-data
  T115<-data
  T125<-data
  
  T55[angles != 55]<- NA
  T65[angles != 65]<- NA
  T75[angles != 75]<- NA
  T85[angles != 85]<- NA
  T95[angles != 95]<- NA
  T105[angles != 105]<- NA
  T115[angles != 115]<- NA
  T125[angles != 125]<- NA
  
  
  T55mean<- c()
  
  j <-seq(from = 1, to = 64, by =2)
  h<- 1
  for (p in 2:33) {
    
    i<- j[h]
    
    
    firstblock<- 1:4
    lastblock<- (length(unlist(na.omit(passive55[,p])))-3):length(unlist(na.omit(passive55[,p])))
    data<- as.vector(unlist(na.omit(passive55[,p])))
    T55mean[i]<- mean(data[firstblock], na.rm = TRUE)
    T55mean[i+1]<- mean(data[lastblock], na.rm = TRUE)
    h<- h+1
    
  }
  
  
  
  
  
  
  

  
  
  T60means<-as.vector(colMeans(T60[,2:33], na.rm = TRUE))
  T80means<-as.vector(colMeans(T80[,2:33], na.rm = TRUE))
  T100means<-as.vector(colMeans(T100[,2:33], na.rm = TRUE))
  T120means<-as.vector(colMeans(T120[,2:33], na.rm = TRUE))
  
  
  
  Locs<- c(T60means, T80means, T100means, T120means)
  
  return(Locs)
}




























###PLotting shifts by reach target angle for active and passive


passive55<-active_loc
passive65<-active_loc
passive75<-active_loc
passive85<-active_loc
passive95<-active_loc
passive105<-active_loc
passive115<-active_loc
passive125<-active_loc


passive55[Aangles != 55]<- NA
passive65[Aangles != 65]<- NA
passive75[Aangles != 75]<- NA
passive85[Aangles != 85]<- NA
passive95[Aangles != 95]<- NA
passive105[Aangles != 105]<- NA
passive115[Aangles != 115]<- NA
passive125[Aangles != 125]<- NA


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







locshift<- p55$meanreaches
blocklength=4
block = rep(c(1:(160/blocklength)), each=blocklength)
df<- data.frame(locshift, block)
df<-aggregate(locshift ~ block, data=df, FUN=mean, na.rm=TRUE)
plot(x = 1:40, y = df$locshift, type = 'l', ylim = c(-5,15), xlab = "Block", ylab = "Shift in Hand Esimates [°]", main = "Passive Localization shifts by target across time")
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

