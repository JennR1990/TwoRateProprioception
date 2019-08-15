ModelAICs <- function(df, group) {
  #group='active'# add this to the function call when i use the commented line below
  #df <- read.csv(sprintf('data/%s_reaches.csv', group), stringsAsFactors = FALSE)

  schedule <- df$distortion
  
  #Reaches <- as.matrix(df[,2:dim(df)[2]])
  Reaches<- df$meanreaches
  
  twoRateFit <- fitTwoRateReachModel(reaches=Reaches, schedule=schedule, oneTwoRates=2, grid='restricted', checkStability=TRUE)
  oneRateFit <- fitTwoRateReachModel(reaches=Reaches, schedule=schedule, oneTwoRates=1, grid='restricted', checkStability=TRUE)
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
  InOb <- 6
  # this is then used for C:
  C <- InOb*(log(2*pi)+1)
  twoRateAIC <- (2*4) + InOb*log(twoRateMSE) + C
  oneRateAIC <- (2*2) + InOb*log(oneRateMSE) + C
  
  cat(sprintf('1-rate AIC: %0.2f  %s  2-rate AIC: %0.2f\n',oneRateAIC,c('>=', ' <')[as.numeric(oneRateAIC<twoRateAIC)+1],twoRateAIC))
  

  AICs<- data.frame(twoRateAIC, oneRateAIC)
  #write.csv(AICs, sprintf("ana/AICs/Group AICs for %s Reaches.csv", group), row.names = TRUE, quote = FALSE)
  
  return(AICs)
}


ParticipantAICs<- function (data, group) {
  
  
  
  
  
  
  
  
}