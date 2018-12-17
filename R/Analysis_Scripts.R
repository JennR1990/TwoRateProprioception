tanalyzedata<- function(adata, pasdata, paudata, ncdata){
AllDataRM<- PrepdataforRM(adata, pasdata, paudata, ncdata)
IndependentT(AllDataRM, 'Active', 'Passive')
IndependentT(AllDataRM, 'Pause', 'No-Cursor')
IndependentT(AllDataRM, 'Active', 'No-Cursor')
IndependentT(AllDataRM, 'Passive', 'No-Cursor')
IndependentT(AllDataRM, 'Active', 'Pause')
IndependentT(AllDataRM, 'Passive', 'Pause')
}



PrepdataforRM<- function(adata, pasdata, paudata, ncdata){
  A_RM<-RepeatedMeasuresCombine1or5(adata)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  Pas_RM<-RepeatedMeasuresCombine1or5(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  Pau_RM<-RepeatedMeasuresCombine1or5(paudata)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  nc_RM<-RepeatedMeasuresCombine1or5(ncdata)
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
  print('Aligned')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print('Aligned')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) # not sig A vs. NC
  print('Aligned')
  print(t.test(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE)) # not sig  A vs. NC
  print('Aligned')
  print(t.test(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.005945  A vs. NC
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) 
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE))  #p-value = 1.36e-07  A vs. NC
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  
}
#cohensD(EC_Late[AllDataRM$Experiment == 'Active'],EC_Late[AllDataRM$Experiment == 'No-Cursor'], data = AllDataRM)
