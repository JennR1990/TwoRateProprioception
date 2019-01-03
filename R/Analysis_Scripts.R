tanalyzedata<- function(AllDataRM){
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

#adata, pasdata, paudata, ncdata
ANOVAanalysis<- function(AllDataANOVAadata){
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
  return(AllDataRM)
}

PrepdataforANOVA <- function(adata, pasdata, paudata, ncdata, ncncdata) {
  
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
  
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM, ncnc_RM)
  
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
  Pas_par$Experiment<-'Passive'
  Pau_par<- prepdataformodel(paudata)
  Pau_par$Experiment<-'Pause'
  nc_par<- prepdataformodel(ncdata)
  nc_par$Experiment<-'No-Cursor'
  allpars<- rbind(a_par, Pas_par, Pau_par, nc_par)
  return(allpars)
}

prepdataformodel<- function (data){
  data$distortion<- data$distortion*-1
  # modeldata<- getreachesformodel(data)
  pars<- getParticipantFits(data)
  return(pars)
}


randomcodes<- function () {
  mmed <- function(x,n=5){runmed(x,n)}  #run this function to do the median smoothing
  Active_p<- mmed(ActiveP$meanreaches)
  Passive_p<- mmed(PassiveP$meanreaches)
  Diff<-ActiveP$meanreaches - PassiveP$meanreaches
  Diff<- Active_p - Passive_p
  PSC<- data.frame(Diff, dist)
  PSC$dist<- PSC$dist*-1
  Average<- mean(PSC$Diff[182:224], na.rm = TRUE)
  Scale<- Average/30
  AllReaches<- data.frame(cbind(PassiveR$meanreaches, PauseR$meanreaches[33:320],ActiveR$meanreaches ))
  AllReachesMean<- rowMeans(allreaches, na.rm = TRUE)
  Reaches<- data.frame(AllReachesMean, dist*-1)
  Reaches_par<- fittworatemodel(Reaches$AllReachesMean, Reaches$dist)
  Reach_model<-tworatemodel(par=Reaches_par, distortions = dist)
  Reach_model$output<-Reach_model$output*Scale
  Reach_model$slow<-Reach_model$slow*Scale
  Diff<- Diff*-1
  Diff <- filtfilt(butter(1, W=0.5, type='low'), Diff)
  layout(matrix(c(1,1,1,1,2,3,4,5), nrow=2, byrow=TRUE), heights=c(3,1))
  plot(Diff*-1,main = "Predicted Sensory Consequences",ylim = c(-10, 10), lwd = 2.5, axes= F,col=rgb(0.44,0.51,0.57), xlab = "Trial", ylab = "Difference in Hand Direction [?]", type = "l")
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  axis(2, at=c(-6,-3,0,3,6), cex.axis=0.75)
  lines(Reach_model$output, col = c(rgb(.5,0.,.5)))
  lines(Reach_model$slow, col = rgb(0.,.5,1.))
  legend(-3,-2,legend=c('Difference','Reach Model - Output', "Reach Model - Slow"),col=c(rgb(0.44,0.51,0.57),rgb(.5,0.,.5), rgb(0.,.5,1.)),lty=c(1,1),lwd=c(2,2),bty='n')
  OutputMSE<- mean((Diff - Reach_model$output)^2)
  SlowMSE<- mean((Diff - Reach_model$slow)^2)
  N<- 22.15
  P <- 4
  C <- N*(log(2*pi)+1)
  OutputAIC <- 2*P + N*log(OutputMSE) + C
  SlowAIC <- 2*P + N*log(SlowMSE) + C
  AICs<- c('slow'=SlowAIC, 'Output'=OutputAIC)
  relativeLikelihoods <- exp((min(AICs) - AICs)/2)
  relativeLikelihoods
  
  
}

oneratevstworate<- function () {
  
  ##Getting AICS for one-rate model vs. two-rate model
  #need to run one rate model
  #we can use the same distortion as the abrupt, everything will be the same as the abrupt stuff
  tf<- getreachesformodel(TF)
  cf<- getreachesformodel(CF)
  #Run the actual model fitting function for No-Cursor trials
  tf1_par<- fitoneratemodel(reaches = tf$meanreaches, distortions = tf$distortion)
  #rs        ls        rf        lf 
  #0.9823168 0.1011206 0.6817995 0.2184631 
  cf1_par<- fitoneratemodel(reaches = cf$meanreaches, distortions = cf$distortion)
  #rs         ls         rf         lf 
  #0.99322853 0.09372752 0.81847991 0.21359619 
  #now run the model function to get the model output
  tf1_model<-oneratemodel(par=tf1_par, distortions = tf$distortion)
  cf1_model<-oneratemodel(par=cf1_par, distortions = cf$distortion)
  
  
  #Now we can do the comparison to get the AICS
  tfreaches<- tf$meanreaches
  cfreaches<- cf$meanreaches
  terminal1MSE<- mean((tfreaches - tf1_model$output)^2)
  terminal2MSE<- mean((tfreaches - tf_model$output)^2)
  continous1MSE<- mean((cfreaches - cf1_model$output)^2)
  continous2MSE<- mean((cfreaches - cf_model$output)^2)
  N<- 5
  P1 <- 2
  P2 <- 4
  C <- N*(log(2*pi)+1)
  terminal1AIC <- 2*P1 + N*log(terminal1MSE) + C
  terminal2AIC <- 2*P2 + N*log(terminal2MSE) + C
  continous1AIC <- 2*P1 + N*log(continous1MSE) + C
  continous2AIC <- 2*P2 + N*log(continous2MSE) + C
  AICts<- c('Terminal1'=terminal1AIC,'Terminal2'=terminal2AIC)
  AICcs<- c('continous1'=continous1AIC, 'continous2'=continous2AIC)
  relativeLikelihoodst <- exp((min(AICts) - AICts)/2)
  relativeLikelihoodsc <- exp((min(AICcs) - AICcs)/2)
  relativeLikelihoodst
  relativeLikelihoodsc
  
  
}
# data$distortion<- data$distortion*-1
# reach_model<-tworatemodel(par=reach_par, distortions = data$distortion)
#cohensD(EC_Late[AllDataRM$Experiment == 'Active'],EC_Late[AllDataRM$Experiment == 'No-Cursor'], data = AllDataRM)
