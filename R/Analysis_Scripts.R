
##How do the different parameters predict the experiments, or can they? -----
#polynomial logistic regression

pLogRegression <- function(data, variable = Test_Trial) {
  
  #df <- read.csv('data/Pilot/rebound.csv', stringsAsFactors = F)
  
  data$variable<- as.factor(data$Test_Trial)
  

    
    print(summary(glm(formula = variable ~ rs + ls + rf + lf, family = binomial(link = "logit"), 
                      data = data)))

   model<- glm(formula = variable ~ rs + ls + rf + lf, family = binomial(link = "logit"), 
        data = data)
   
   #print(predict(model))
  predictions<- predict(model)
  
  predictions[predictions<.5]<-0
  predictions[predictions>.5]<-1
  print(predictions)
  print(length(which(data$Test_Trial == predictions)))
  print(correct<- length(which(data$Test_Trial == predictions)) / length(predictions) )
  
  return(correct)
  
   
   
}

tpanalyzedata<- function(AllDataRM){
  IndependentT(AllDataRM, 'Active', 'Passive', 'Localization')
  PairedT(AllDataRM, 'Active', 'Localization')
  PairedT(AllDataRM, 'Passive', 'Localization')
}


ANOVAanalysis<- function(AllDataANOVA){
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Experiment<- as.factor(AllDataANOVA$Experiment)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=Reaches,
                       wid=ID,
                       within=Time,
                       between = Experiment,
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}


PrepdataforT<- function(adata, pasdata, paudata, ncdata, ncncdata){
  #
  
  A_RM<-TCombine(adata)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Pau_RM<-TCombine(paudata)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  
  nc_RM<-TCombine(ncdata)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))
  
  ncnc_RM<-NoCursorsTCombine(ncncdata)
  ncnc_RM$Experiment <- rep('No-Cursor_No-Cursors', nrow(ncnc_RM))

  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM, ncnc_RM)
  # 
  return(AllDataRM)
}


PrepdataforANOVA <- function(adata, pasdata, paudata, ncdata) {
  
  # 
  
  A_RM<-ANOVAcombine(adata)
  A_RM$ID <- sprintf('ActLoc.%s',A_RM$ID)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Pau_RM<-ANOVAcombine(paudata)
  Pau_RM$ID <- sprintf('Pause.%s',Pau_RM$ID)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  
  nc_RM<-ANOVAcombine(ncdata)
  nc_RM$ID <- sprintf('NoCursor.%s',nc_RM$ID)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))

  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM)
  #
  return(AllDataRM)
  
}

PrepdataforANOVA1 <- function(adata, pasdata, paudata, ncdata, ncncdata, ncIdata, ncncIdata) {
  
  # 
  
  A_RM<-ANOVAcombine(adata)
  A_RM$ID <- sprintf('ActLoc.%s',A_RM$ID)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Pau_RM<-ANOVAcombine(paudata)
  Pau_RM$ID <- sprintf('Pause.%s',Pau_RM$ID)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  
  nc_RM<-ANOVAcombine(ncdata)
  nc_RM$ID <- sprintf('NoCursor.%s',nc_RM$ID)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))
  
  ncnc_RM<-NoCursorACombine(ncncdata)
  ncnc_RM$ID <- sprintf('NoCursor_No-Cursors.%s',ncnc_RM$ID)
  ncnc_RM$Experiment <- rep('No-Cursor_No-Cursors', nrow(ncnc_RM))
  
  ncI_RM<-ANOVAcombine(ncIdata)
  ncI_RM$ID <- sprintf('NoCursor.%s',ncI_RM$ID)
  ncI_RM$Experiment <- rep('No-CursorI', nrow(ncI_RM))
  
  ncncI_RM<-NoCursorACombine(ncncIdata)
  ncncI_RM$ID <- sprintf('NoCursorI_No-Cursors.%s',ncncI_RM$ID)
  ncncI_RM$Experiment <- rep('No-CursorI_No-Cursors', nrow(ncncI_RM))
  
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM, ncnc_RM, ncI_RM, ncncI_RM)
  #
  return(AllDataRM)
  
}


PrepdataforPropT<- function(adata, pasdata, paudata, ncdata, ncncdata){
  A_RM<-TCombine(adata)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  AllDataRM<- rbind(A_RM, Pas_RM)
  return(AllDataRM)
}

PrepdataforPropANOVA <- function(adata, pasdata, paudata, ncdata, ncncdata) {
  
  A_RM<-ANOVAcombine(adata)
  A_RM$ID <- sprintf('ActLoc.%s',A_RM$ID)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  AllDataRM<- rbind(A_RM, Pas_RM)
  
  return(AllDataRM)
  
}
IndependentT<- function(data, exp1, exp2, task) {
  print(sprintf('this is the between subjects comparison of %s to %s %s Data', exp1, exp2, task))
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  
  
}
PairedT<- function(data, exp1, task) {
  print(sprintf('this is the within subjects analysis of %s %s Data', exp1, task))
  print('Is there early learning?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE)) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], na.rm = TRUE))
  print('Did they return to baseline? (Should not)')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE))
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], na.rm = TRUE))
  print('Was there learning from beginning to end of 1st rotation?')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], na.rm = TRUE))
  print('Was there learning from 1st to 2nd block of 1st rotation?')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], na.rm = TRUE))
  print('How much could they learn of the 60 degree change?')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # not sig A vs. NC
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], na.rm = TRUE)) # not sig A vs. NC
  print('Do their error clamp trials reflect the trajectories in 2nd rotation?')
  print(t.test(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE)) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # not sig  A vs. NC
  print(etaSquaredTtest(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], na.rm = TRUE)) # not sig  A vs. NC
  print('Is there a change in error clamps across time? (Decay)')
  print(t.test(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # p-value = 0.005945  A vs. NC
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE)) # p-value = 0.005945  A vs. NC
  print('Did they just revert back to aligned behaviour when dealing with 2nd rotation?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], na.rm = TRUE)) 
  print('Are the 1st few error clamp trials the same as their aligned behaviour?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))  #p-value = 1.36e-07  A vs. NC
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], na.rm = TRUE))  #p-value = 1.36e-07  A vs. NC
  print('Did they eventually decay back to baseline?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE))
  print('Were the error clamp trials similar to the 1st rotation?')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE))
  
}



InitialLearning<- function (data, exp1, task){
  
  print('1st four to 2nd four')
print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], na.rm = TRUE))
print('2nd four to last four of R1')
print(t.test(data$R1_second[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
print(cohen.d(data$R1_second[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
print(etaSquaredTtest(data$R1_second[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], na.rm = TRUE))
print('2nd four to 3rd four')
print(t.test(data$R1_second[data$Experiment == exp1],data$R1_third[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
print(cohen.d(data$R1_second[data$Experiment == exp1],data$R1_third[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
print(etaSquaredTtest(data$R1_second[data$Experiment == exp1],data$R1_third[data$Experiment == exp1], na.rm = TRUE))
print('Was there learning from 3rd to 4th block of 1st rotation?')
print(t.test(data$R1_third[data$Experiment == exp1],data$R1_forth[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
print(cohen.d(data$R1_third[data$Experiment == exp1],data$R1_forth[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
print(etaSquaredTtest(data$R1_third[data$Experiment == exp1],data$R1_forth[data$Experiment == exp1], na.rm = TRUE))
print('Was there learning from 4th to 5th block of 1st rotation?')
print(t.test(data$R1_forth[data$Experiment == exp1],data$R1_fifth[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
print(cohen.d(data$R1_forth[data$Experiment == exp1],data$R1_fifth[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
print(etaSquaredTtest(data$R1_forth[data$Experiment == exp1],data$R1_fifth[data$Experiment == exp1], na.rm = TRUE))
print('Was there learning from 5th to 6th block of 1st rotation?')
print(t.test(data$R1_fifth[data$Experiment == exp1],data$R1_sixth[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
print(cohen.d(data$R1_fifth[data$Experiment == exp1],data$R1_sixth[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
print(etaSquaredTtest(data$R1_fifth[data$Experiment == exp1],data$R1_sixth[data$Experiment == exp1], na.rm = TRUE))

}




propvsREA<- function (){
  allttest<-rbind(Ttestdata[Ttestdata$Experiment == 'No-Cursor_No-Cursors',], TtestPdata)
  print('reach aftereffects versus active localizations')
  print(t.test(allttest$r1[allttest$Experiment == 'No-Cursor_No-Cursors'],allttest$r1[allttest$Experiment == 'Active']*-1)) # p-value = 0.04535 A vs. NC
  print(cohen.d(allttest$r1[allttest$Experiment == 'No-Cursor_No-Cursors'],allttest$r1[allttest$Experiment == 'Active']*-1, na.rm = TRUE))
  print(etaSquaredTtest(allttest$r1[allttest$Experiment == 'No-Cursor_No-Cursors'],allttest$r1[allttest$Experiment == 'Active']*-1, na.rm = TRUE))
  
  print('reach aftereffects versus active localizations')
  print(t.test(allttest$r1[allttest$Experiment == 'No-Cursor_No-Cursors'],allttest$r1[allttest$Experiment == 'Passive' ]*-1)) # p-value = 0.04535 A vs. NC
  print(cohen.d(allttest$r1[allttest$Experiment == 'No-Cursor_No-Cursors'],allttest$r1[allttest$Experiment == 'Passive']*-1, na.rm = TRUE))
  print(etaSquaredTtest(allttest$r1[allttest$Experiment == 'No-Cursor_No-Cursors'],allttest$r1[allttest$Experiment == 'Passive' ]*-1, na.rm = TRUE))

}







##Models

ParticipantReachmodels2<- function(adata, pasdata, paudata, ncdata) {
  #Active = 1
  #Passive = 0
  a_par<- getParticipantFits2(adata)
  a_par$Experiment<-'Active'
  a_par$Test_Trial<-1
  Pas_par<- getParticipantFits2(pasdata)
  Pas_par$Experiment<-'Passive'
  Pas_par$Test_Trial<-0
  Pau_par<- getParticipantFits2(paudata)
  Pau_par$Experiment<-'Pause'
  Pau_par$Test_Trial<-0
  nc_par<- getParticipantFits2(ncdata)
  nc_par$Experiment<-'No-Cursor'
  nc_par$Test_Trial<-1

  allpars<- rbind(a_par, Pas_par, Pau_par, nc_par)
  return(allpars)
}
# ParticipantBothReachmodels<- function(adata, pasdata, paudata, ncdata, ncidata) {
#   a_par<- getParticipantFits2(adata)
#   a_par1<- getParticipantFits1(adata)
#   a_par$Experiment<-'Active'
#   a_par$Test_Trial<-'Active'
#   a_par1$Experiment<-'Active'
#   a_par1$Test_Trial<-'Active'
#   Pas_par<- getParticipantFits2(pasdata)
#   Pas_par1<- getParticipantFits1(pasdata)
#   Pas_par$Experiment<-'Passive'
#   Pas_par$Test_Trial<-'Passive'
#   Pas_par1$Experiment<-'Passive'
#   Pas_par1$Test_Trial<-'Passive'
#   Pau_par<- getParticipantFits2(paudata)
#   Pau_par1<- getParticipantFits1(paudata)
#   Pau_par$Experiment<-'Pause'
#   Pau_par$Test_Trial<-'Passive'
#   Pau_par1$Experiment<-'Pause'
#   Pau_par1$Test_Trial<-'Passive'
#   nc_par<- getParticipantFits2(ncdata)
#   nc_par1<- getParticipantFits1(ncdata)
#   nc_par$Experiment<-'No-Cursor'
#   nc_par$Test_Trial<-'Active'
#   nc_par1$Experiment<-'No-Cursor'
#   nc_par1$Test_Trial<-'Active'
#   nci_par<- getParticipantFits2(ncidata)
#   nci_par1<- getParticipantFits1(ncidata)
#   nci_par$Experiment<-'No-Cursor_I'
#   nci_par$Test_Trial<-'Active'
#   nci_par1$Experiment<-'No-Cursor_I'
#   nci_par1$Test_Trial<-'Active'
#   allpars<- rbind(a_par, Pas_par, Pau_par, nc_par, nci_par,a_par1, Pas_par1, Pau_par1, nc_par1, nci_par1)
#   return(allpars)
# }
ParticipantReachmodels1<- function(adata, pasdata, paudata, ncdata) {
  a_par<- getParticipantFits1(adata)
  a_par$Experiment<-'Active'
  a_par$Test_Trial<-'Active'
  Pas_par<- getParticipantFits1(pasdata)
  Pas_par$Experiment<-'Passive'
  Pas_par$Test_Trial<-'Passive'
  Pau_par<- getParticipantFits1(paudata)
  Pau_par$Experiment<-'Pause'
  Pau_par$Test_Trial<-'Passive'
  nc_par<- getParticipantFits1(ncdata)
  nc_par$Experiment<-'No-Cursor'
  nc_par$Test_Trial<-'Active'
  allpars<- rbind(a_par, Pas_par, Pau_par, nc_par)
  return(allpars)
}

GroupModelAICs <- function(data, group, grid = 'restricted') {
  
  df<- getreachesformodel(data)
  #group='active'# add this to the function call when i use the commented line below
  #df <- read.csv(sprintf('data/%s_reaches.csv', group), stringsAsFactors = FALSE)
  
  schedule <- df$distortion
  
  #Reaches <- as.matrix(df[,2:dim(df)[2]])
  Reaches<- df$meanreaches
  
  twoRateFit <- fitTwoRateReachModel(reaches=Reaches, schedule=schedule, oneTwoRates=2, grid=grid, checkStability=TRUE)
  oneRateFit <- fitTwoRateReachModel(reaches=Reaches, schedule=schedule, oneTwoRates=1, grid=grid, checkStability=TRUE)
  #twoRateFit<- fittworatemodel(Reaches, schedule)
  #oneRateFit<- fitoneratemodel(Reaches, schedule)
  
  
  print(oneRateFit)
  print(twoRateFit)
  
  twoRateMSE <- twoRateReachModelErrors(par=twoRateFit, reaches=Reaches, schedule=schedule)
  oneRateMSE <- twoRateReachModelErrors(par=oneRateFit, reaches=Reaches, schedule=schedule)
  #twoRateMSE <- twoRateReachModelError(par=twoRateFit, reaches=Reaches, distortions =schedule)
  #oneRateMSE <- oneRateReachModelError(par=oneRateFit, reaches=Reaches, distortions =  schedule)
  
  #preparing the AIC stuff
  N <- dim(df)[2] - 1
  # the median length of a phase is 40 trials,
  # and there are 7.2 of those in 288 trials
  InOb <- seriesEffectiveSampleSize(Reaches, method='ac_lag95%CI')
  print(InOb)
  # this is then used for C:
  #C <- InOb*(log(2*pi)+1)
  #twoRateAIC <- (2*4) + InOb*log(twoRateMSE) + C
  #oneRateAIC <- (2*2) + InOb*log(oneRateMSE) + C
  
  twoRateAIC<-(InOb * log(twoRateMSE)) + (2 * 4)
  oneRateAIC<-(InOb * log(oneRateMSE)) + (2 * 2)
  
  cat(sprintf('1-rate AIC: %0.2f  %s  2-rate AIC: %0.2f\n',oneRateAIC,c('>=', ' <')[as.numeric(oneRateAIC<twoRateAIC)+1],twoRateAIC))
  likelihood<-exp((min(c(twoRateAIC, oneRateAIC))-c(twoRateAIC, oneRateAIC))/2)
  print(likelihood)
  #pars<- data.frame(twoRateFit)
  metrics<- data.frame(twoRateAIC, oneRateAIC, likelihood[1], likelihood[2])
  names(metrics)<- c('twoRateAIC', 'oneRateAIC', 'twoRatelikelihood', 'oneratelikelihood')
  #write.csv(AICs, sprintf("ana/AICs/Group AICs for %s Reaches.csv", group), row.names = TRUE, quote = FALSE)
  
  return(metrics)
}

getParticipantFits2 <- function(data, grid='restricted') {

  participants <- colnames(data)[2:dim(data)[2]]
  distortions <- data$distortion

  participantfits <- data.frame(matrix(NA, ncol=6, nrow=length(participants)))
  colnames(participantfits) <- c('participant', 'rs', 'ls', 'rf', 'lf', 'MSE')

  for (ppno in c(1:length(participants))) {

    participant <- participants[ppno]
    print(participant)
    reaches <- data[,participant]

    #pars<- fitTwoRateReachModel(reaches = reaches, schedule = distortions)
    pars<- fitTwoRateReachModel(reaches=reaches, schedule=distortions, oneTwoRates=2, grid=grid, checkStability=TRUE)
    #pars <- fittworatemodel(reaches, distortions)

    participantfits$participant[ppno] <- participant
    participantfits$rs[ppno] <- pars['Rs']
    participantfits$ls[ppno] <- pars['Ls']
    participantfits$rf[ppno] <- pars['Rf']
    participantfits$lf[ppno] <- pars['Lf']
    participantfits$MSE[ppno] <- twoRateReachModelErrors(pars, reaches, distortions)

  }

  return(participantfits)
}

getParticipantFits1 <- function(data, grid='restricted') {

  participants <- colnames(data)[2:dim(data)[2]]
  distortions <- data$distortion

  participantfits <- data.frame(matrix(NA, ncol=4, nrow=length(participants)))
  colnames(participantfits) <- c('participant', 'rs', 'ls', 'MSE')

  for (ppno in c(1:length(participants))) {

    participant <- participants[ppno]
    print(participant)
    reaches <- data[,participant]

    #pars <- fitOneRateReachModel(reaches, distortions)
    pars <- fitTwoRateReachModel(reaches=reaches, schedule=distortions, oneTwoRates=1, grid=grid, checkStability=TRUE)

    participantfits$participant[ppno] <- participant
    participantfits$rs[ppno] <- pars['rs']
    participantfits$ls[ppno] <- pars['ls']
    participantfits$MSE[ppno] <- twoRateReachModelErrors(pars, reaches, distortions)

  }

  return(participantfits)
}


mmed <- function(x,n=5){runmed(x,n)}

LocalizationModelCompare<- function (dataset, dataset2, color) {
  data<- getreachesformodel(dataset)
  dist<- data$distortion
  localizations<- mmed(data$meanreaches)
  Average<- mean(localizations[182:224], na.rm = TRUE)
  Scale<- Average/30
  reachdata<- getreachesformodel(dataset2)$meanreaches
  Reach_par<- fitTwoRateReachModel(reaches = reachdata,schedule= dist)
  Reach_model<-twoRateReachModel(par=Reach_par, schedule = dist)
  Reach_model$total<-Reach_model$total*Scale
  Reach_model$slow<-Reach_model$slow*Scale
  prop_par<-fitPropModel(dataset2,dataset)
  meanreaches<-rowMeans(dataset2[241:288,2:ncol(dataset2)], na.rm=TRUE)
  meanreaches<- meanreaches*-1
  dataset2$distortion[241:288]<- as.numeric(meanreaches)
  output<- PropModel(unlist(prop_par), dataset2$distortion)
  plot(localizations,main = "Localizations vs Scaled Model",ylim = c(-5, 10), lwd = 2.5, axes= F,col=color, xlab = "Trial", ylab = "Difference in Hand Direction [?]", type = "l")
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  axis(2, at=c(-5,0,5,10), cex.axis=0.75)
  lines(Reach_model$total*-1, col = c(rgb(.5,0.,.5)))
  lines(Reach_model$slow*-1, col = rgb(0.,.5,1.))
  lines(output, col = 'yellow')
  legend(-3,-1,legend=c('Localization Data','Reach Model - Total', "Reach Model - Slow", 'Prop Model'),col=c(color,rgb(.5,0.,.5), rgb(0.,.5,1.), 'yellow'),lty=c(1,1),lwd=c(2,2),bty='n')
  TotalMSE<- mean((localizations - Reach_model$total)^2)
  SlowMSE<- mean((localizations - Reach_model$slow)^2)
  PropMSE<- mean((localizations - output)^2)
  
  
  InOb <- seriesEffectiveSampleSize(reachdata, method='ac_lag95%CI')
  TotalAIC<-(InOb * log(TotalMSE)) + (2 * 4)
  SlowAIC<-(InOb * log(SlowMSE)) + (2 * 2)
  PropAIC<-(InOb * log(PropMSE)) + (2 * 2)
  
  
  # N<- 22.15
  # P <- 4
  # C <- N*(log(2*pi)+1)
  # 
  
  
  # 
  # TotalAIC <- 2*P + N*log(TotalMSE) + C
  # SlowAIC <- 2*P + N*log(SlowMSE) + C
  # PropAIC <- 2*P + N*log(PropMSE) + C
  AICs<- c('slow'=SlowAIC, 'Total'=TotalAIC, 'Prop' = PropMSE)
  relativeLikelihoods <- exp((min(AICs) - AICs)/2)
  names(relativeLikelihoods)<- c('slow', 'Total', 'Prop')
  metrics<- list(AICs, relativeLikelihoods)
  totallh<- sprintf('Total %1.2f', relativeLikelihoods[2])
  text(180, 2, totallh)
  Slowlh<- sprintf('Slow %1.2f', relativeLikelihoods[1])
  text(180, 0, Slowlh)
  Proplh<- sprintf('Prop %1.2f', relativeLikelihoods[3])
  text(180, -2, Proplh)
  
  return(metrics)
  
}


fitPropModel<- function(reachdata, locadata) {

  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  meanreaches<-rowMeans(reachdata[241:288,2:ncol(reachdata)], na.rm=TRUE)
  meanreaches<- meanreaches*-1
  reachdata$distortion[241:288]<- as.numeric(meanreaches)
  schedule<- reachdata$distortion


  #this function will take the dataframe made in the last function (dogridsearch) and use the list of parameters to make a new model then compare to output and get a new mse.
  pargrid <- gridsearch(localizations, schedule, nsteps = 7, topn = 4)
  cat('optimize best fits...\n')
  for (gridpoint in c(1:nrow(pargrid))) { #for each row
    par<-unlist(pargrid[gridpoint,1])

    control <- list('maxit'=10000, 'ndeps'=1e-9 )
    fit <- optim(par=par, PropModelMSE, gr=NULL, schedule, localizations, control=control, method = "Brent", lower = 0, upper = 1)
    optpar<- fit$par


    # stick optpar back in pargrid
    pargrid[gridpoint,1] <- optpar

    pargrid[gridpoint,2]<- fit$value

  }
  # get lowest MSE, and pars that go with that
  bestpar <- order(pargrid[,2])[1]

  output<- PropModel(unlist(pargrid[bestpar]), schedule)
  proportion<- sprintf('Proportion = %.2f', unlist(pargrid[bestpar]))

  reaches <- getreachesformodel(reachdata)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = schedule,
      oneTwoRates = 2,
      checkStability = TRUE
    )
  reach_model <-
    twoRateReachModel(par = reach_par, schedule = schedule)
  Average<- mean(localizations[182:224], na.rm = TRUE)
  Scale<- Average/30
  reach_model$slow<- reach_model$slow*Scale
  reach_model$fast<- reach_model$fast*Scale

  return(unlist(pargrid[bestpar]))

}
PropModel <- function(par, schedule) {
  locest<-c()
  #loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    # first we calculate what the model does, since the model is proportional, we just multiply the one parameters by the schedule to get what the person should do

    locest[t] <- par * schedule[t]
  }

  # after we loop through all trials, we return the model output:
  return(locest)

}

PropModelMSE <- function(par, schedule, localizations) {

  locesti<- PropModel(par, schedule)
  errors <- locesti - localizations
  MSE <- mean(errors^2, na.rm=TRUE)



  return( MSE )

}


gridsearch<- function(localizations, schedule, nsteps=7, topn=4) {


  cat('doing grid search...\n')

  steps <- nsteps #say how many points inbetween 0-1 we want
  pargrid <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) #not sure what exactly this does
  MSE<- rep(NA, length(pargrid))
  pargrid<- cbind(pargrid, MSE)

  for (gridpoint in c(1:nrow(pargrid))) { #for each row
    par<-unlist(pargrid[gridpoint,1])    #take that row and take it out of df and make it par
    pargrid[gridpoint,2] <- PropModelMSE(par, schedule,localizations)
  }

  bestN <- order(pargrid[,2])[1:topn]

  return(pargrid[bestN,])
}



##Equivalence tests

equivalence<- function(){
meanNC<-mean(allttest$r1[allttest$Task== 'Reach' & allttest$Experiment == 'No-Cursor_No-Cursors'], na.rm = TRUE)*-1
SDNC<-sd(allttest$r1[allttest$Task== 'Reach' & allttest$Experiment == 'No-Cursor_No-Cursors'], na.rm = TRUE)
meanA<-mean(allttest$r1[allttest$Task== 'Prop' & allttest$Experiment == 'Active'], na.rm = TRUE)
SDA<-sd(allttest$r1[allttest$Task== 'Prop' & allttest$Experiment == 'Active'], na.rm = TRUE)
SDP<-sd(allttest$r1[allttest$Task== 'Prop' & allttest$Experiment == 'Passive'], na.rm = TRUE)
meanP<-mean(allttest$r1[allttest$Task== 'Prop' & allttest$Experiment == 'Passive'], na.rm = TRUE)

x <- mean(allttest$r1[allttest$Task == "Reach" & allttest$Experiment == "No-Cursor"], na.rm = TRUE)*-1
sd <- sd(allttest$r1[allttest$Task == "Reach" & allttest$Experiment == "No-Cursor"], na.rm = TRUE)
n <- 48
t<- 2.01
se<- sd/(sqrt(n))
CI<- c((x+t*se),(x-t*se))
pwr.t2n.test(n1 = 48, n2 = 32, sig.level = .05, power = .33)

TOSTtwo(meanNC, meanA, SDNC,SDA,48,32,-.35,.35,0.05)
TOSTtwo(meanNC, meanP, SDNC,SDP,48,32,-.35,.35,0.05)
}




# Descriptive statistics about the data -----
Allmeans<- function() {
  
  ActiveReach<- means(active_reaches)
  ActiveReach$Experiment<- "ActiveR"
  ActiveProp<- means(active_localization)
  ActiveProp$Experiment<- "ActiveL"
  PassiveReach<- means(passive_reaches)
  PassiveReach$Experiment<- "PassiveR"
  PassiveProp<- means(passive_localization)
  PassiveProp$Experiment<- "PassiveL"
  PauseReach<- means(pause_reaches[33:320,])
  PauseReach$Experiment<- "PauseR"
  NocursorReach<- means(nocursor_reaches[33:320,])
  NocursorReach$Experiment<- "NocursorR"
  NocursorIReach<- means(nocursorI_reaches[33:320,])
  NocursorIReach$Experiment<- "NocursorIR"
  NocursorNCReach<- NCmeans(nocursor_nocursors)
  NocursorNCReach$Experiment<- "NocursorNC"
  NocursorNCIReach<- NCmeans(nocursorI_nocursors)
  NocursorNCIReach$Experiment<- "NocursorINC"
  Allmeans<- rbind(ActiveReach, ActiveProp, PassiveReach, PassiveProp, PauseReach, NocursorReach, NocursorNCReach, NocursorIReach, NocursorNCIReach)
  write.csv(Allmeans, 'ana/All Experiments Descriptive Stats_updated.csv', quote = FALSE, row.names = FALSE )
  
  return(Allmeans)
  
  
}

NCmeans<- function (data) {
  
  
  
  AlignedMean<- mean(unlist(data[29:32,2:ncol(data)]), na.rm= TRUE)
  AlignedMax<- max(unlist(data[29:32,2:ncol(data)]), na.rm= TRUE)
  AlignedMin<- min(unlist(data[29:32,2:ncol(data)]), na.rm= TRUE)
  InitialRotationMean<- mean(unlist(data[33:36,2:ncol(data)]), na.rm= TRUE)
  InitialRotationMax<- max(unlist(data[33:36,2:ncol(data)]), na.rm= TRUE)
  InitialRotationMin<- min(unlist(data[33:36,2:ncol(data)]), na.rm= TRUE)
  EndofIRotationMean<- mean(unlist(data[189:192,2:ncol(data)]), na.rm= TRUE)
  EndofIRotationMax<- max(unlist(data[189:192,2:ncol(data)]), na.rm= TRUE)
  EndofIRotationMin<- min(unlist(data[189:192,2:ncol(data)]), na.rm= TRUE)
  SecondRotationMean<- mean(unlist(data[205:208,2:ncol(data)]), na.rm= TRUE)
  SecondRotationMax<- max(unlist(data[205:208,2:ncol(data)]), na.rm= TRUE)
  SecondRotationMin<- min(unlist(data[205:208,2:ncol(data)]), na.rm= TRUE)
  ErrorClampMean<- mean(unlist(data[209:212,2:ncol(data)]), na.rm= TRUE)
  ErrorClampMax<- max(unlist(data[209:212,2:ncol(data)]), na.rm= TRUE)
  ErrorClampMin<- min(unlist(data[209:212,2:ncol(data)]), na.rm= TRUE)
  ErrorClampLateMean<- mean(unlist(data[253:256,2:ncol(data)]), na.rm= TRUE)
  ErrorClampLateMax<- max(unlist(data[253:256,2:ncol(data)]), na.rm= TRUE)
  ErrorClampLateMin<- min(unlist(data[253:256,2:ncol(data)]), na.rm= TRUE)
  
  
  return(Descriptives<- (data.frame(AlignedMean, AlignedMax, AlignedMin, InitialRotationMean, InitialRotationMax, InitialRotationMin, EndofIRotationMean, EndofIRotationMax, EndofIRotationMin, SecondRotationMean, SecondRotationMax, SecondRotationMin, ErrorClampMean, ErrorClampMax, ErrorClampMin, ErrorClampLateMean, ErrorClampLateMax, ErrorClampLateMin))*-1)
}
means<- function (data) {
  
  
  
  AlignedMean<- mean(unlist(data[61:64,2:ncol(data)]), na.rm= TRUE)
  AlignedMax<- max(unlist(data[61:64,2:ncol(data)]), na.rm= TRUE)
  AlignedMin<- min(unlist(data[61:64,2:ncol(data)]), na.rm= TRUE)
  InitialRotationMean<- mean(unlist(data[65:68,2:ncol(data)]), na.rm= TRUE)
  InitialRotationMax<- max(unlist(data[65:68,2:ncol(data)]), na.rm= TRUE)
  InitialRotationMin<- min(unlist(data[65:68,2:ncol(data)]), na.rm= TRUE)
  EndofIRotationMean<- mean(unlist(data[208:224,2:ncol(data)]), na.rm= TRUE)
  EndofIRotationMax<- max(unlist(data[208:224,2:ncol(data)]), na.rm= TRUE)
  EndofIRotationMin<- min(unlist(data[208:224,2:ncol(data)]), na.rm= TRUE)
  SecondRotationMean<- mean(unlist(data[237:240,2:ncol(data)]), na.rm= TRUE)
  SecondRotationMax<- max(unlist(data[237:240,2:ncol(data)]), na.rm= TRUE)
  SecondRotationMin<- min(unlist(data[237:240,2:ncol(data)]), na.rm= TRUE)
  ErrorClampMean<- mean(unlist(data[241:244,2:ncol(data)]), na.rm= TRUE)
  ErrorClampMax<- max(unlist(data[241:244,2:ncol(data)]), na.rm= TRUE)
  ErrorClampMin<- min(unlist(data[241:244,2:ncol(data)]), na.rm= TRUE)
  ErrorClampLateMean<- mean(unlist(data[256:288,2:ncol(data)]), na.rm= TRUE)
  ErrorClampLateMax<- max(unlist(data[256:288,2:ncol(data)]), na.rm= TRUE)
  ErrorClampLateMin<- min(unlist(data[256:288,2:ncol(data)]), na.rm= TRUE)
  
  
  return(Descriptives<- (data.frame(AlignedMean, AlignedMax, AlignedMin, InitialRotationMean, InitialRotationMax, InitialRotationMin, EndofIRotationMean, EndofIRotationMax, EndofIRotationMin, SecondRotationMean, SecondRotationMax, SecondRotationMin, ErrorClampMean, ErrorClampMax, ErrorClampMin, ErrorClampLateMean, ErrorClampLateMax, ErrorClampLateMin))*-1)
}