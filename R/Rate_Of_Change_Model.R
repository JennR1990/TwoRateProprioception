# Step 1 & 2 Create Data --------------------------------------------------
##Calculate per participant fits to be able to get a slow process for each participant


##No-Cursor Fits
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



##Active Localization Fits
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


##PAssive Localization Fits
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


#save the data for easy access later
write.csv(PA_Slow, 'data/Passive_Slow_Process.csv', quote = FALSE, row.names = FALSE)
write.csv(AC_Slow, 'data/Active_Slow_Process.csv', quote = FALSE, row.names = FALSE)
write.csv(NC_Slow, 'data/No-Cursor_Slow_Process.csv', quote = FALSE, row.names = FALSE)







# Step 3 -Find Asymptote  ------------------------------------------------------------------

####### If you have this data use these lines to read in the code with the correct name to continue on
PA_Slow<- read.csv('data/Passive_Slow_Process.csv', header = TRUE)
AC_Slow<- read.csv('data/Active_Slow_Process.csv', header = TRUE)
PA_Slow<- read.csv('data/No-Cursor_Slow_Process.csv', header = TRUE)


## Now we need to find the asymptote of each of the slow processes and the no-cursors and localizations
##Took the mean of the last 40 trials of the first rotation

#Slow process data
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

#Actual Localization and no-cursor data
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




# Step 4 - Scaling --------------------------------------------------------

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
##Now we have scaled the data and everything goes from zero to 100.




# Step 5 - Amount of correction between trials ----------------------------

# the first rotation trials of the slow model output, the actual no-cursor reaches and localizations are now scaled to the last 40 trials of the first rotation. 
#Here i take the difference between trial i+1 and triali.  This is the amount learning between each trial.

PA_Scaled_Diff<- data.frame()
AC_Scaled_Diff<- data.frame()
NC_Scaled_Diff<- data.frame()
PA_Slow_Scaled_Diff<- data.frame()
AC_Slow_Scaled_Diff<- data.frame()
NC_Slow_Scaled_Diff<- data.frame()

for (j in 1:32){

  for (i in 1:159){

      PA_Scaled_Diff[i,j]<-PA_Scaled[i,j] - 100
      AC_Scaled_Diff[i,j]<-AC_Scaled[i,j] - 100
      PA_Slow_Scaled_Diff[i,j]<-PA_Slow_Scaled[i,j] -100
      AC_Slow_Scaled_Diff[i,j]<-AC_Slow_Scaled[i,j] - 100


    }
}


for (j in 1:48){
  
  for (i in 1:160){
    
    NC_Scaled_Diff[i,j]<-NC_Scaled[i,j] - 100
    NC_Slow_Scaled_Diff[i,j]<-NC_Slow_Scaled[i,j] - 100
    
    
  }
}
names(PA_Scaled_Diff)<- pnames
names(AC_Scaled_Diff)<- pnames
names(NC_Scaled_Diff)<- pnames1
names(PA_Slow_Scaled_Diff)<- pnames
names(AC_Slow_Scaled_Diff)<- pnames
names(NC_Slow_Scaled_Diff)<- pnames1




# Step 6 - Average Rate of Change -----------------------------------------


# now i need to find the average amount of change.  I looked at the whole 160 trials but it might make more sense to only do the first few trials when i go to get the average
##then i took the mean of all those. 



#get average for first 40 trials of passive localizations and slow process
averagediff<- colMeans(PA_Scaled_Diff[1:40,], na.rm = TRUE)
averagediff1<- colMeans(PA_Slow_Scaled_Diff[1:40,], na.rm = TRUE)
averagediff<- rbind(averagediff, averagediff1)

#get average for first 40 trials of Active localizations
averagediff1<- colMeans(AC_Scaled_Diff[1:40,], na.rm = TRUE)
averagediff<- rbind(averagediff, averagediff1)

#get average for first 40 trials of Active slow process
averagediff1<- colMeans(AC_Slow_Scaled_Diff[1:40,], na.rm = TRUE)
averagediff<- rbind(averagediff, averagediff1)

#make those named numbers into a data frame and give it column headers
averagediff<- data.frame(averagediff)
averagediff$experiment<- c('Passive_Loc', "Passive_Slow", "Active_Loc", "Active_Slow")


#get average for first 40 trials of No-cursors and slow process
averagediffnc<- colMeans(NC_Scaled_Diff[1:40,], na.rm = TRUE)
averagediffnc1<- colMeans(NC_Slow_Scaled_Diff[1:40,], na.rm = TRUE)
averagediffnc<- rbind(averagediffnc, averagediffnc1)

#make those named numbers into a data frame and give it column headers
averagediffnc<- data.frame(averagediffnc)
averagediffnc$experiment<- c('No-Cursors', "No-Cursor_Slow")
