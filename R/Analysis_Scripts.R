###Compare pre- post localization
#layout(c(1,2,3))
averagedprepost<- function (dataset = c('Pause', 'NoCursor', 'NewNoC')) {
  svglite(file='doc/Pre_Post_Data.svg', width=6, height=9, system_fonts=list(sans = "Arial"))
  layout(c(1,2,3), heights = c(2,2,2))

  exp = list('No-Localization Task','No-Cursor Task', 'New No-Cursor Task')
  counter<- 1
  
  for (data in dataset) {
    filename<- sprintf('data/%s_pre_post_Prop.csv', data)
    df<- read.csv(filename, sep = ',', header = TRUE)
    pre<- mean(unlist(df[56:64, 2:ncol(df)]), na.rm = TRUE)
    post<- mean(unlist(df[65:73, 2:ncol(df)]), na.rm = TRUE)
    stuff<- c(pre, post)
    print(stuff)
    barplot(stuff, ylim = c(-1.5,1.5), names = c('Pre', 'Post'), col= c('mediumorchid3', 'red'))
    text(.75,pre+.1, labels = pre)
    text(2,post+.1, labels = post)
    title( main = exp[counter] )
    counter<- counter + 1
  }
  dev.off()
}





#regression for error clamp versus whatever you want

plotRegressionWithCI <- function(X,Y,colors=c('#99999999','black')) {
  
  # fit regression model
  this.lm <- lm(Y ~ X, na.rm = TRUE)
  
  # where is the interesting data
  pointlocs <- seq(min(X, na.rm = TRUE),max(X, na.rm = TRUE),.1)
  
  # get the confidence interval
  y1 = predict( this.lm, newdata=data.frame(X=pointlocs), interval =
                  "confidence" )[ , "upr" ]
  y2 = predict( this.lm, newdata=data.frame(X=pointlocs), interval =
                  "confidence" )[ , "lwr" ]
  
  # show the confidence interval
  polygon(c(pointlocs,rev(pointlocs)),c(y1,rev(y2)), col=colors[1],
          border=NA)
  
  # and show a regression line:
  lines(range(X, na.rm = TRUE), predict(this.lm, newdata=data.frame(X=range(X, na.rm = TRUE))),
        col=colors[2], lwd=2)
  
}




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
  write.csv(Allmeans, 'ana/All Experiments Descriptive Stats.csv', quote = FALSE, row.names = FALSE )
  
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

##per participant fits for each group 
ppfits<- function (groups = c('active', 'passive', 'pause', 'nocursor', 'nocursor_NI')) {
  pars<- data.frame()
  counter<- 1
  for (group in groups){
  filename<- sprintf('data/%s_reaches.csv', group)
  data<- read.csv(filename, stringsAsFactors = F, header = TRUE)
  par<- getParticipantFits(data)
  par$experiment<- rep(group, times = nrow(par))
  
  if (counter >1){
    pars<- rbind(pars, par)
  } else {
    pars<- par
  }
  counter<- counter + 1
  print(dim(pars))
  # output<- sprintf('%s_participant_parameters.csv', group)
  # write.csv(pars, output, quote = FALSE, row.names = FALSE)
  }
  return(pars)
}

#polynomial logistic regression

pLogRegression <- function(data) {
  
  #df <- read.csv('data/Pilot/rebound.csv', stringsAsFactors = F)
  
  data$experiment <- as.factor(data$experiment)
  

    
    print(summary(glm(formula = experiment ~ rs + ls + rf + lf, family = binomial(link = "logit"), 
                      data = data)))

  
}


tanalyzedata<- function(AllDataRM){
  IndependentT(AllDataRM, 'Active', 'Passive', 'Reach')
  IndependentT(AllDataRM, 'Pause', 'No-Cursor', 'Reach')
  IndependentT(AllDataRM, 'Active', 'No-Cursor', 'Reach')
  IndependentT(AllDataRM, 'Passive', 'No-Cursor', 'Reach')
  IndependentT(AllDataRM, 'Active', 'Pause', 'Reach')
  IndependentT(AllDataRM, 'Passive', 'Pause', 'Reach')
  PairedT(AllDataRM, 'Active', 'Reach')
  PairedT(AllDataRM, 'Passive', 'Reach')
  PairedT(AllDataRM, 'Pause', 'Reach')
  PairedT(AllDataRM, 'No-Cursor', 'Reach')
}

tpanalyzedata<- function(AllDataRM){
  IndependentT(AllDataRM, 'Active', 'Passive', 'Localization')
  # IndependentT(AllDataRM, 'Pause', 'No-Cursor')
  # IndependentT(AllDataRM, 'Active', 'No-Cursor')
  # IndependentT(AllDataRM, 'Passive', 'No-Cursor')
  # IndependentT(AllDataRM, 'Active', 'Pause')
  # IndependentT(AllDataRM, 'Passive', 'Pause')
  PairedT(AllDataRM, 'Active', 'Localization')
  PairedT(AllDataRM, 'Passive', 'Localization')
  # PairedT(AllDataRM, 'Pause')
  # PairedT(AllDataRM, 'No-Cursor')
}

#adata, pasdata, paudata, ncdata
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


PrepdataforT<- function(adata, pasdata, paudata, ncdata, ncncdata, ncIdata, ncncIdata){
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
  
  ncI_RM<-TCombine(ncIdata)
  ncI_RM$Experiment <- rep('No-CursorI', nrow(ncI_RM))

  ncncI_RM<-NoCursorsTCombine(ncncIdata)
  ncncI_RM$Experiment <- rep('No-CursorI_No-Cursors', nrow(ncncI_RM))
  
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM, ncnc_RM, ncI_RM, ncncI_RM)
  # 
  return(AllDataRM)
}

PrepdataforANOVA <- function(adata, pasdata, paudata, ncdata, ncncdata, ncIdata, ncncIdata) {
  
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
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2])) # p-value = 0.04535 A vs. NC
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2])) # not sig A vs. NC
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2])) # not sig  A vs. NC
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2])) # p-value = 0.005945  A vs. NC
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2]))  #p-value = 1.36e-07  A vs. NC
  
  
}
PairedT<- function(data, exp1, task) {
  print(sprintf('this is the within subjects analysis of %s %s Data', exp1, task))
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

prepdatagetfits<- function (data){
  data$distortion<- data$distortion
  # modeldata<- getreachesformodel(data)
  pars<- getParticipantFits(data)
  return(pars)
}

prepdatagetonefits<- function (data){
  data$distortion<- data$distortion
  # modeldata<- getreachesformodel(data)
  pars<- getoneParticipantFits(data)
  return(pars)
}


GroupAICS<- function(data, bootstraps=1) {
  
  reaches <- as.matrix(data[,2:33])
  distortion<- data[,1]
  
  onerateAICs <- c()
  tworateAICs <- c()
  
  for (boots in c(1:bootstraps)) {
    LC <- apply(reaches[,sample(c(1:32),32,replace=TRUE)],1,median,na.rm=TRUE)
    
    distortion<- distortion*-1
    onerate_par<- fitoneratemodel(reaches = LC, distortions = distortion)
    tworate_par<- fittworatemodel(reaches = LC, distortions = distortion)
    print(onerate_par)
    print(tworate_par)

    distortion<- distortion*-1
    onerate_model<- oneratemodel(par = onerate_par, distortions = distortion)
    tworate_model<- tworatemodel(par = tworate_par, distortions = distortion)
    
    onerateMSE<- mean((LC - onerate_model$output)^2)
    tworateMSE<- mean((LC - tworate_model$output)^2)
    N<- 5
    P1 <- 2
    P2 <- 4
    C <- N*(log(2*pi)+1)
    onerateAIC <- 2*P1 + N*log(onerateMSE) + C
    tworateAIC <- 2*P2 + N*log(tworateMSE) + C


    onerateAICs <- c(onerateAICs, onerateAIC)
    tworateAICs <- c(tworateAICs, tworateAIC)
    
    
    # relativeLikelihoodsone <- exp((min(onerateAICs) - onerateAICs)/2)
    # relativeLikelihoodstwo <- exp((min(tworateAICs) - tworateAICs)/2)
    # print(relativeLikelihoodsone)
    # print(relativeLikelihoodstwo)
  }
  
  return(data.frame(onerateAICs,tworateAICs))
  
}

bootstrapModelAICsjENN <- function(group='active', bootstraps=1) {
  
  
  
  df <- read.csv(sprintf('data/%s_reaches.csv', group), stringsAsFactors = FALSE)
  distortion <- df$distortion
  
  reaches <- as.matrix(df[,2:dim(df)[2]])
  
  N <- dim(df)[2] - 1
  # prep for AICs:
  
  # the median length of a phase is 40 trials,
  # and there are 7.2 of those in 288 trials
  InOb <- 5
  # # the mean length is 55 though:
  # N <- 4
  
  # this is then used for C:
  C <- InOb*(log(2*pi)+1)
  
  for (bootstrap in c(1:bootstraps)) {
    
    medReaches <- apply(reaches[,sample(c(1:N),N,replace=TRUE)], 1, median, na.rm=TRUE)
    distortion<- distortion

    
    onerate_par<- fitoneratemodel(reaches = medReaches, distortions = distortion)
    tworate_par<- fittworatemodel(reaches = medReaches, distortions = distortion)
    
    
    distortion<- distortion
    onerate_model<- oneratemodel(par = onerate_par, distortions = distortion)
    tworate_model<- tworatemodel(par = tworate_par, distortions = distortion)
    
    
    twoRateMSE<-twoRateReachModelError(tworate_par, reaches = medReaches, distortions = distortion*-1)
    oneRateMSE<-oneRateReachModelError(onerate_par, reaches = medReaches, distortions = distortion*-1)
    print(oneRateMSE<- mean((medReaches - onerate_model$output*-1)^2))
    print(twoRateMSE<- mean((medReaches - tworate_model$output*-1)^2))

    twoRateAIC <- (2*4) + InOb*log(twoRateMSE) + C
    oneRateAIC <- (2*2) + InOb*log(oneRateMSE) + C

    
    cat(sprintf('1-rate AIC: %0.2f  %s  2-rate AIC: %0.2f\n',oneRateAIC,c('>=', ' <')[as.numeric(oneRateAIC<twoRateAIC)+1],twoRateAIC))
    
  }
  
}




bootstrapModelAICs <- function(data, bootstraps=1) {
  #group='active'# add this to the function call when i use the commented line below
  
  library(RateRate)
  
  #df <- read.csv(sprintf('data/%s_reaches.csv', group), stringsAsFactors = FALSE)
  df<-data
  schedule <- df$distortion
  
  reaches <- as.matrix(df[,2:dim(df)[2]])
  
  N <- dim(df)[2] - 1
  # prep for AICs:
  
  # the median length of a phase is 40 trials,
  # and there are 7.2 of those in 288 trials
  InOb <- 5
  # # the mean length is 55 though:
  # N <- 4
  
  # this is then used for C:
  C <- InOb*(log(2*pi)+1)
  
  for (bootstrap in c(1:bootstraps)) {
    
    medReaches <- apply(reaches[,sample(c(1:N),N,replace=TRUE)], 1, median, na.rm=TRUE)
    
    twoRateFit <- fitTwoRateReachModel(reaches=medReaches, schedule=schedule, oneTwoRates=2, grid='restricted', checkStability=TRUE)
    oneRateFit <- fitTwoRateReachModel(reaches=medReaches, schedule=schedule, oneTwoRates=1, grid='restricted', checkStability=TRUE)
    print(oneRateFit)
    print(twoRateFit)
    
    twoRateMSE <- twoRateReachModelErrors(par=twoRateFit, reaches=medReaches, schedule=schedule)
    oneRateMSE <- twoRateReachModelErrors(par=oneRateFit, reaches=medReaches, schedule=schedule)
    
    
    twoRateAIC <- (2*4) + InOb*log(twoRateMSE) + C
    oneRateAIC <- (2*2) + InOb*log(oneRateMSE) + C
    
    cat(sprintf('1-rate AIC: %0.2f  %s  2-rate AIC: %0.2f\n',oneRateAIC,c('>=', ' <')[as.numeric(oneRateAIC<twoRateAIC)+1],twoRateAIC))
    
  }
  return(data.frame(twoRateAIC, oneRateAIC))
}


Poneratevstworate<- function (data, group = 'Passive') {
  ##Getting AICS for one-rate model vs. two-rate model
  #need to run one rate model
  par1<- prepdatagetonefits(data)
  write.csv(par1, sprintf("One Rate Parameters for %s Reaches.csv", group), row.names = TRUE, quote = FALSE)
  #need to run two rate model
  par2<- prepdatagetfits(data)
  write.csv(par2, sprintf("Two Rate Parameters for %s Reaches.csv", group), row.names = TRUE, quote = FALSE)

  Data1MSE<- par1$MSE
  Data2MSE<- par2$MSE
  N<- 5
  P1 <- 2
  P2 <- 4
  C <- N*(log(2*pi)+1)
  Data1AIC <- 2*P1 + N*log(Data1MSE) + C
  Data2AIC <- 2*P2 + N*log(Data2MSE) + C
  count<-sum(Data1AIC<Data2AIC)
  AICs<- c('One Rate Model'=Data1AIC,'Two Rate Model'=Data2AIC)
  write.csv(AICs, sprintf("AICs for one and two rate reach data.csv"), row.names = TRUE, quote = FALSE)
  #relativeLikelihoods <- exp((min(AICs) - AICs)/2)
  return(sprintf('the number of participants with a higher AIC for two rates are %d',count))
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
