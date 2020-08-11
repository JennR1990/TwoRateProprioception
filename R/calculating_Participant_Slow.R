ncfits<- getParticipantFits2(newnocursor_reaches)

NC_Slow<- c()
for (indx in 1:48){
  if (indx == 1){
NC_Slow <-twoRateReachModel(par = unlist(ncfits[indx,2:5]), schedule = newnocursor_reaches$distortion*-1)$slow
  } else {
    reach_model1 <-twoRateReachModel(par = unlist(ncfits[indx,2:5]), schedule = newnocursor_reaches$distortion*-1)$slow
    NC_Slow<- cbind(NC_Slow, reach_model1)
  }
 
}

NC_Slow<- data.frame(NC_Slow)


pnames1<-c()
for (i in 1:48){
  pnames1[i]<-sprintf('p%.0f', i) 
}

names(NC_Slow)<- pnames1







acfits<- getParticipantFits2(active_reaches)
AC_Slow<- c()
for (indx in 1:32){
  if (indx == 1){
    AC_Slow <-twoRateReachModel(par = unlist(acfits[indx,2:5]), schedule = active_reaches$distortion*-1)$slow
  } else {
    reach_model1 <-twoRateReachModel(par = unlist(acfits[indx,2:5]), schedule = active_reaches$distortion*-1)$slow
    AC_Slow<- cbind(AC_Slow, reach_model1)
  }
  
}

AC_Slow<- data.frame(AC_Slow)


pnames<-c()
for (i in 1:32){
  pnames[i]<-sprintf('p%.0f', i) 
}

names(AC_Slow)<- pnames





pafits<- getParticipantFits2(passive_reaches)
PA_Slow<- c()
for (indx in 1:32){
  if (indx == 1){
    PA_Slow <-twoRateReachModel(par = unlist(pafits[indx,2:5]), schedule = passive_reaches$distortion*-1)$slow
  } else {
    reach_model1 <-twoRateReachModel(par = unlist(pafits[indx,2:5]), schedule = passive_reaches$distortion*-1)$slow
    PA_Slow<- cbind(PA_Slow, reach_model1)
  }
  
}

PA_Slow<- data.frame(PA_Slow)




names(PA_Slow)<- pnames



write.csv(PA_Slow, 'data/Passive_Slow_Process.csv', quote = FALSE, row.names = FALSE)
write.csv(AC_Slow, 'data/Active_Slow_Process.csv', quote = FALSE, row.names = FALSE)
write.csv(NC_Slow, 'data/No-Cursor_Slow_Process.csv', quote = FALSE, row.names = FALSE)



## Now we need to find the asymptote of each of the slow processes and the no-cursors and localizations

PA_Slow_Asymptote<- c()
for (i in 1:32){
  PA_Slow_Asymptote[i]<-mean(PA_Slow[184:224,i], na.rm = TRUE)
}
AC_Slow_Asymptote<- c()
for (i in 1:32){
  AC_Slow_Asymptote[i]<-mean(AC_Slow[184:224,i], na.rm = TRUE)
}
NC_Slow_Asymptote<- c()
for (i in 1:48){
  NC_Slow_Asymptote[i]<-mean(NC_Slow[216:256,i], na.rm = TRUE)
}


PA_Asymptote<- c()
for (i in 1:32){
  PA_Asymptote[i]<-mean(passive_localization[184:224,i+1], na.rm = TRUE)
}
AC_Asymptote<- c()
for (i in 1:32){
  AC_Asymptote[i]<-mean(active_localization[184:224,i+1], na.rm = TRUE)
}
NC_Asymptote<- c()
for (i in 1:48){
  NC_Asymptote[i]<-mean(newnocursor_nocursors[152:192,i+1], na.rm = TRUE)
}


Asymptotes<- cbind(PA_Slow_Asymptote, AC_Slow_Asymptote, PA_Asymptote, AC_Asymptote) 
NCAsymptotes<- cbind(NC_Slow_Asymptote, NC_Asymptote)


### NOw that i know the highest value for each of the conditions i need to scale the data to go from zero to one. 
AC_Slow_Scaled<- c()
PA_Slow_Scaled<- c()
AC_Scaled<- c()
PA_Scaled<- c()
for (i in 1:32){

  if (i == 1){
    AC_Slow_Scaled<- (AC_Slow[65:224,i]/AC_Slow_Asymptote[i])*100
    AC_Scaled<- (active_localization[65:224,i+1]/AC_Asymptote[i])*100
    
    PA_Slow_Scaled<- (PA_Slow[65:224,i]/PA_Slow_Asymptote[i])*100
    PA_Scaled<- (passive_localization[65:224,i+1]/PA_Asymptote[i])*100
    
    
  } else{
    AC_Slow_Scaled1<- (AC_Slow[65:224,i]/AC_Slow_Asymptote[i])*100
    AC_Slow_Scaled<- cbind(AC_Slow_Scaled, AC_Slow_Scaled1)
    AC_Scaled1<- (active_localization[65:224,i+1]/AC_Asymptote[i])*100
    AC_Scaled<- cbind(AC_Scaled, AC_Scaled1)
    
    PA_Slow_Scaled1<- (PA_Slow[65:224,i]/PA_Slow_Asymptote[i])*100
    PA_Slow_Scaled<- cbind(PA_Slow_Scaled, PA_Slow_Scaled1)
    PA_Scaled1<- (passive_localization[65:224,i+1]/PA_Asymptote[i])*100
    PA_Scaled<- cbind(PA_Scaled, PA_Scaled1)
  }
  
}

NC_Slow_Scaled<- c()
NC_Scaled<- c()
for (i in 1:48){
  if (i == 1){
    
    NC_Slow_Scaled<- (NC_Slow[97:256,i]/NC_Slow_Asymptote[i])*100
    NC_Scaled<- (newnocursor_nocursors[33:192,i+1]/NC_Asymptote[i])*100
  } else {
    NC_Slow_Scaled1<- (NC_Slow[97:256,i]/NC_Slow_Asymptote[i])*100
    NC_Scaled1<- (newnocursor_nocursors[33:192,i+1]/NC_Asymptote[i])*100
    NC_Scaled<- cbind(NC_Scaled, NC_Scaled1)
    NC_Slow_Scaled<- cbind(NC_Slow_Scaled, NC_Slow_Scaled1)
  }
  
}

NC_Slow_Scaled<- data.frame(NC_Slow_Scaled)
NC_Scaled<- data.frame(NC_Scaled)
AC_Slow_Scaled<- data.frame(AC_Slow_Scaled)
PA_Slow_Scaled<- data.frame(PA_Slow_Scaled)
AC_Scaled<- data.frame(AC_Scaled)
PA_Scaled<- data.frame(PA_Scaled)

names(NC_Slow_Scaled)<- pnames1
names(NC_Scaled)<- pnames1
names(AC_Slow_Scaled)<- pnames
names(AC_Scaled)<- pnames
names(PA_Slow_Scaled)<- pnames
names(PA_Scaled)<- pnames
# the first rotation trials of the slow model output, the actual no-cursor reaches and localizations are now scaled to the last 40 trials of the first rotation. 


PA_Scaled$p1[1] - PA_Scaled$p1[2]
PA_Scaled$p1[2] - PA_Scaled$p1[3]
PA_Scaled$p1[3]
PA_Scaled$p1[4]


PA_Scaled_Diff<- data.frame()
AC_Scaled_Diff<- data.frame()
NC_Scaled_Diff<- data.frame()
PA_Slow_Scaled_Diff<- data.frame()
AC_Slow_Scaled_Diff<- data.frame()
NC_Slow_Scaled_Diff<- data.frame()

for (j in 1:32){

  for (i in 1:159){

      PA_Scaled_Diff[i,j]<-PA_Scaled[i+1,j] - PA_Scaled[i,j]
      AC_Scaled_Diff[i,j]<-AC_Scaled[i+1,j] - AC_Scaled[i,j]
      PA_Slow_Scaled_Diff[i,j]<-PA_Slow_Scaled[i,j] - PA_Slow_Scaled[i+1,j]
      AC_Slow_Scaled_Diff[i,j]<-AC_Slow_Scaled[i,j] - AC_Slow_Scaled[i+1,j]


    }
}

for (j in 1:48){
  
  for (i in 1:159){
    
    NC_Scaled_Diff[i,j]<-NC_Scaled[i+1,j] - NC_Scaled[i,j]
    NC_Slow_Scaled_Diff[i,j]<-NC_Slow_Scaled[i+1,j] - NC_Slow_Scaled[i,j]
    
    
  }
}
names(PA_Scaled_Diff)<- pnames
names(AC_Scaled_Diff)<- pnames
names(NC_Scaled_Diff)<- pnames
names(PA_Slow_Scaled_Diff)<- pnames
names(AC_Slow_Scaled_Diff)<- pnames
names(NC_Slow_Scaled_Diff)<- pnames

##Now we have scaled the data and everything goes from zero to 100.
# now i need to find the average amount of change.  I looked at the whole 160 trials but it might make more sense to only do the first few trials when i go to get the average
# all i did here was subtract trial 2 from 1 and 3 from 2 so i could get the amount of change from trial to trial and then i took the mean of all those. 




averagediff<- colMeans(PA_Scaled_Diff, na.rm = TRUE)
averagediff1<- colMeans(PA_Slow_Scaled_Diff, na.rm = TRUE)
averagediff<- rbind(averagediff, averagediff1)
averagediff1<- colMeans(AC_Scaled_Diff, na.rm = TRUE)
averagediff<- rbind(averagediff, averagediff1)
averagediff1<- colMeans(AC_Slow_Scaled_Diff, na.rm = TRUE)
averagediff<- rbind(averagediff, averagediff1)
averagediff<- data.frame(averagediff)
averagediff$experiment<- c('Passive_Loc', "Passive_Slow", "Active_Loc", "Active_Slow")

averagediffnc<- colMeans(NC_Scaled_Diff, na.rm = TRUE)
averagediffnc1<- colMeans(NC_Slow_Scaled_Diff, na.rm = TRUE)
averagediffnc<- rbind(averagediffnc, averagediffnc1)
averagediffnc<- data.frame(averagediffnc)
averagediffnc$experiment<- c('No-Cursors', "No-Cursor_Slow")