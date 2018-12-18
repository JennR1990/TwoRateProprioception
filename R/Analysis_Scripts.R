tanalyzedata<- function(adata, pasdata, paudata, ncdata){
  AllDataRM<- PrepdataforT(adata, pasdata, paudata, ncdata)
  IndependentT(AllDataRM, 'Active', 'Passive')
  IndependentT(AllDataRM, 'Pause', 'No-Cursor')
  IndependentT(AllDataRM, 'Active', 'No-Cursor')
  IndependentT(AllDataRM, 'Passive', 'No-Cursor')
  IndependentT(AllDataRM, 'Active', 'Pause')
  IndependentT(AllDataRM, 'Passive', 'Pause')
  PairedT(AllDataRM, 'Active')
  PairedT(AllDataRM, 'Passive')
  PairedT(AllDataRM, 'Pause')
  PairedT(AllDataRM, 'No-Cursor')
}

ANOVAanalysis<- function(adata, pasdata, paudata, ncdata){
  AllDataANOVA<- PrepdataforANOVA(adata, pasdata, paudata, ncdata)
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


PrepdataforT<- function(adata, pasdata, paudata, ncdata){
  A_RM<-TCombine1or5(adata)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  Pas_RM<-TCombine1or5(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  Pau_RM<-TCombine1or5(paudata)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  nc_RM<-TCombine1or5(ncdata)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM)
  return(AllDataRM)
}

PrepdataforANOVA <- function(adata, pasdata, paudata, ncdata) {
  
  A_RM<-ANOVAcombine1or5(adata)
  A_RM$ID <- sprintf('ActLoc.%s',A_RM$ID)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-ANOVAcombine1or5(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Pau_RM<-ANOVAcombine1or5(paudata)
  Pau_RM$ID <- sprintf('Pause.%s',Pau_RM$ID)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  
  nc_RM<-ANOVAcombine1or5(ncdata)
  nc_RM$ID <- sprintf('NoCursor.%s',nc_RM$ID)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))
  
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM)
  
  return(AllDataRM)
  
}


IndependentT<- function(data, exp1, exp2) {
  sprintf('this is the between subjects comparison of %s to %s', exp1, exp2)
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2])) #not sig A vs. NC
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2])) # p-value = 0.04535 A vs. NC
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2])) # not sig A vs. NC
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2])) # not sig  A vs. NC
  print('Beginnig of Error Clamp')
  print(t.test(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2])) # p-value = 0.005945  A vs. NC
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2]))  #p-value = 1.36e-07  A vs. NC
  
  
}
PairedT<- function(data, exp1) {
  sprintf('this is the within subjects comparison of %s', exp1)  
  print('Is there early learning?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE)) #not sig A vs. NC
  print('Did they return to baseline? (Should not)')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE))
  print('Was there learning from beginning to end of 1st rotation?')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print('How much could they learn of the 60 degree change?')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) # not sig A vs. NC
  print('Do their error clamp trials reflect the trajectories in 2nd rotation?')
  print(t.test(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE)) # not sig  A vs. NC
  print('Is there a change in error clamps across time? (Decay)')
  print(t.test(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.005945  A vs. NC
  print('Did they just revert back to aligned behaviour when dealing with 2nd rotation?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) 
  print('Are the 1st few error clamp trials the same as their aligned behaviour?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE))  #p-value = 1.36e-07  A vs. NC
  print('Did they eventually decay back to baseline?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  
}


##Models

ParticipantReachmodels<- function(adata, pasdata, paudata, ncdata) {
  a_par<- prepdataformodel(adata)
  a_par$Experiment<-'Active'
  Pas_par<- prepdataformodel(pasdata)
  a_par$Experiment<-'Passive'
  Pau_par<- prepdataformodel(paudata)
  a_par$Experiment<-'Pause'
  nc_par<- prepdataformodel(ncdata)
  a_par$Experiment<-'No-Cursor'
  allpars<- rbind(a_par, Pas_par, Pau_par, nc_par)
  return(allpars)
}

prepdataformodel<- function (data){
  data$distortion<- data$distortion*-1
  # modeldata<- getreachesformodel(data)
  pars<- getParticipantFits(data)
  return(pars)
}

# data$distortion<- data$distortion*-1
# reach_model<-tworatemodel(par=reach_par, distortions = data$distortion)
#cohensD(EC_Late[AllDataRM$Experiment == 'Active'],EC_Late[AllDataRM$Experiment == 'No-Cursor'], data = AllDataRM)
