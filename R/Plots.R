# Codes to create final plots for manuscript -----

loadcolors <- function() {
  ##Active
  colorA       <<- rgb(1.0, 0.4, 0.0)         # orange
  colorA_trans <<- rgb(1.0, 0.4, 0.0, 0.2)     # transparent orange
  
  
  ## Passive
  colorPA       <<- rgb(0.7, 0.0, 0.7)          # purple
  colorPA_trans <<- rgb(0.7, 0.0, 0.7, 0.2)     # transparent purple
  
  
  ## Pause
  colorNL       <<- rgb(0.63, 0.71, 0.81)      # blue-gray
  colorNL_trans <<- rgb(0.63, 0.71, 0.81, 0.2)  # transparent blue-gray
  
  
  ##No-Cursor
  colorNC       <<- rgb(0.0, 0.7, 0.0)         # green
  colorNC_trans <<- rgb(0.0, 0.7, 0.0, 0.2)     # transparent green
  
  ##New No-Cursor
  colorNNC       <<- rgb(0.1, 0.3, 0.5)         # purple
  colorNNC_trans <<- rgb(0.1, 0.3, 0.5, 0.2)     # transparent purple
}


Plotexp1CI <- function (acd, pad, nld) {
  PlotoutLine(acd, 1:3, 1:3, "Training Trials")
  PlotData(acd, 1, 1)
  PlotData(pad, 2, 2)
  PlotData(nld, 3, 3)
}

Plotexp2CI <- function (acd, ncd, ncdI, nld) {
  PlotoutLine(acd, 3:5, 3:5, "Training Trials")
  PlotData(nld, 3, 3)
  PlotData(ncd, 4, 4)
  PlotData(ncdI, 5, 5)
}

PlotallTapCI <- function (pl = dataset1, al = dataset2) {
  PlotoutLine(pl, 6:7, 1:2, "Hand Localizations")
  PlotData(al, 1, 1, 1)
  PlotData(pl, 2, 2, 1)
}



Plotnocursors <- function (acd,ncd_NC, ncdI) {
  PlotoutLine(acd, 4:5, 4:5, "Open-Loop Reaches")
  PlotData(ncd_NC, 4, 4, x =  c(c(33:288), rev(c(33:288))))
  PlotData(ncdI, 5, 5, x =  c(c(33:288), rev(c(33:288))))
}

PlotallreachesCI <-
  function (acd = dataset1,
            pad = dataset2,
            nld = dataset3,
            ncd = dataset4,
            ncdI = dataset5) {
    PlotoutLine(acd, 1:5, 1:5, "Training Trials")
    PlotData(acd, 1, 1)
    PlotData(pad, 2, 2)
    PlotData(nld, 3, 3)
    PlotData(ncd, 4, 4)
    PlotData(ncdI, 5, 5)
  }


RegressionPLot <- function(exp) {
  if (exp == 1) {
    PRrm <- TCombine(passive_reaches)
    PRRm <- PRrm$EC_Late * -1
    PPec <- TCombine(passive_localization)
    PPec <- PPec$EC_Late
    plot(
      PPec ~ PRRm,
      col = colorPA,
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-12, 25),
      ylim = c(-12, 25),
      axes = FALSE
    )
    axis(2,
         at = c(-30, -20, -10, 0, 10, 20, 30),
         cex.axis = 0.75)
    axis(1,
         at = c(-30, -20 - 10, 0, 10, 20, 30),
         cex.axis = 0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    
    
    Arm <- TCombine(active_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(active_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorA)
    plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
    
    PARrm <- TCombine(pause_reaches[33:320,])
    PARrm <- PARrm[-13,]
    PARRm <- PARrm$EC_Late * -1
    PAPec <- colMeans(Pause[1:32, 2:32], na.rm = TRUE)
    points(PAPec ~ PARRm, col = colorNL)
    plotRegressionWithCI(PARRm, PAPec, colors = c(colorNL_trans, colorNL))
    legend(
      -14,
      23,
      legend = c(
        'Passive Localization',
        'Active Localization',
        'Pause'
      ),
      col = c(colorPA, colorA, colorNL),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n'
    )
    
    
    
  } else if (exp == 2) {
    PRrm <- TCombine(pause_reaches[33:320,])
    PRrm <- PRrm[-13,]
    PRRm <- PRrm$EC_Late * -1
    PPec <- colMeans(Pause[1:32, 2:32], na.rm = TRUE)
    plot(
      PPec ~ PRRm,
      col = colorNL,
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-12, 25),
      ylim = c(-12, 25),
      axes = FALSE
    )
    axis(2, at = c(-20, -10, 0, 10, 20), cex.axis = 0.75)
    axis(1, at = c(-10, 0, 10, 20, 30), cex.axis = 0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorNL_trans, colorNL))
    NCrm <- TCombine(nocursor_reaches[33:320,])
    NCRm <- NCrm$EC_Late * -1
    NCPec <- colMeans(NoCursor[1:32, 2:33], na.rm = TRUE)
    points(NCPec ~ NCRm, col = colorNC)
    plotRegressionWithCI(NCRm, NCPec, colors = c(colorNC_trans, colorNC))
    NCIrm <- TCombine(nocursorI_reaches[33:320,])
    NCIRm <- NCIrm$EC_Late * -1
    NCIPec <- colMeans(NewNoC[1:32, 2:17], na.rm = TRUE)
    points(NCIPec ~ NCIRm, col = colorNNC)
    plotRegressionWithCI(NCIRm, NCIPec, colors = c(colorNNC_trans, colorNNC))
    legend(
      -14,
      23,
      legend = c('New No-Cursor', 'No-Cursor', 'Pause'),
      col = c(colorNNC, colorNC, colorNL),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n'
    )
    
  }
}

RegressionPLotec <- function() {

    PRrm <- TCombine(passive_reaches)
    PRRm <- PRrm$EC_Late * -1
    PPec <- TCombine(passive_localization)
    PPec <- PPec$EC_Late
    plot(
      PPec ~ PRRm,
      col = colorPA_trans,
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-12, 25),
      ylim = c(-12, 25),
      axes = FALSE,
      pch = 19
    )
    axis(2,
         at = c( -10, 0, 10, 20, 30),
         cex.axis = 0.75)
    axis(1,
         at = c(- 10, 0, 10, 20, 30),
         cex.axis = 0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    
    
    Arm <- TCombine(active_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(active_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorA_trans, pch = 19)
    plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))

    legend(
      -14,
      23,
      legend = c(
        'Passive Localization',
        'Active Localization'
      ),
      col = c(colorPA, colorA),
      lty = c(1, 1),
      lwd = c(2, 2),
      bty = 'n'
    )

}

RegressionPLotR1 <- function() {
  PRrm <- TCombine(passive_reaches)
  PRRm <- PRrm$R1_Late * -1
  PPec <- TCombine(passive_localization)
  PPec <- PPec$R1_Late
  plot(
    PPec ~ PRRm,
    col = colorPA,
    xlab = 'Reaches',
    ylab = 'Localization',
    xlim = c(10, 40),
    ylim = c(-5, 25),
    axes = FALSE, asp = 1
  )
  axis(2,
       at = c( -5, 0, 5, 15, 25),
       cex.axis = 0.75)
  axis(1,
       at = c( 15, 20,30,40),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  
  Arm <- TCombine(active_reaches)
  Arm <- Arm[-21,]
  ARm <- Arm$R1_Late * -1
  APec <- TCombine(active_localization)
  APec<- APec[-21,]
  APec <- APec$R1_Late
  points(APec ~ ARm, col = colorA)
  gm<-plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  # legend(
  #   16,
  #   32,
  #   legend = c(
  #     'Passive Localization',
  #     'Active Localization'
  #   ),
  #   col = c(colorPA, colorA),
  #   lty = c(1, 1),
  #   lwd = c(2, 2),
  #   bty = 'n'
  # )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}
RegressionPLotR1E <- function() {
  PRrm <- TCombine(passive_reaches)
  PRRm <- PRrm$R1_Early * -1
  PPec <- TCombine(passive_localization)
  PPec <- PPec$R1_Early
  plot(
    PPec ~ PRRm,
    col = colorPA,
    xlab = 'Reaches',
    ylab = 'Localization',
    xlim = c(3, 35),
    ylim = c(-5, 20),
    axes = FALSE, asp = 1
  )
  axis(2,
       at = c(  0, 10, 20),
       cex.axis = 0.75)
  axis(1,
       at = c( 5,10, 20, 30),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  
  Arm <- TCombine(active_reaches)
  #Arm <- Arm[-21,]
  ARm <- Arm$R1_Early * -1
  APec <- TCombine(active_localization)
  #APec<- APec[-21,]
  APec <- APec$R1_Early
  points(APec ~ ARm, col = colorA)
  gm<-plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  # legend(
  #   0,
  #   32,
  #   legend = c(
  #     'Passive Localization',
  #     'Active Localization'
  #   ),
  #   col = c(colorPA, colorA),
  #   lty = c(1, 1),
  #   lwd = c(2, 2),
  #   bty = 'n'
  # )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}

RegressionPLot3P <- function() {

  PPec <- TCombine(passive_localization)
  loc <- c(PPec$R2,PPec$Aligned,PPec$R1_Late)
  pert<- c(rep(-30, 32),rep(0, 32),rep(30, 32))
  pert1<- c(rep(-29, 32),rep(1, 32),rep(31, 32))
   plot(
    loc ~ pert1,
    col = colorPA_trans,
    xlab = 'Size of Perturbation [°]',
    ylab = 'Change in Hand Localization [°]',
    xlim = c(-35, 35),
    ylim = c(-20, 20),
    main = 'Size of Perturbation Vs. Localizations',
    axes = FALSE, asp = 1, pch = 19
  )
  axis(2,
       at = c(-20, -10,0,10, 20),
       cex.axis = 0.75)
  axis(1,
       at = c( -30, 0,  30),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(pert, loc, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  

  APec <- TCombine(active_localization)
  loca <- c(APec$R2,APec$Aligned,APec$R1_Late)
  perta<- c(rep(-30, 32),rep(0, 32),rep(30, 32))
  pertb<- c(rep(-31, 32),rep(-1, 32),rep(29, 32))

  points(loca ~ pertb, col = colorA_trans, pch = 19)
  gm<-plotRegressionWithCI(perta, loca, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  legend(
    -30,
    22,
    legend = c(
      'Passive Localization',
      'Active Localization'
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n'
  )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}

RegressionPLotchange <- function() {
  
  PPec <- TCombine(passive_localization)
  a<-PPec$Aligned
  b<-PPec$R1_Late
  c<-PPec$R1_Late - PPec$R2
  loc <- c(a,b,c)
  pert<- c(rep(0, 32),rep(30, 32),rep(60, 32))
  pert1<- c(rep(1, 32),rep(31, 32),rep(61, 32))
  plot(
    loc ~ pert1,
    col = colorPA_trans,
    xlab = 'Change in Size of Pertubation [°]',
    ylab = 'Change in Hand Localization [°]',
    xlim = c(0, 60),
    ylim = c(-10, 30),
    main = 'Change in Perturbation Vs. Localizations',
    axes = FALSE,
    pch = 19
  )
  axis(2,
       at = c( -10, 0, 10 ,20, 30),
       cex.axis = 0.75)
  axis(1,
       at = c( 0, 30,  60),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(pert, loc, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  
  
  
  APec <- TCombine(active_localization)
  loca <- c(APec$Aligned,APec$R1_Late,APec$R1_Late - APec$R2)
  perta<- c(rep(0, 32),rep(30, 32),rep(60, 32))  
  pertb<- c(rep(-1, 32),rep(29, 32),rep(59, 32))
  
  points(loca ~ pertb, col = colorA_trans, pch = 19)
  gm<-plotRegressionWithCI(perta, loca, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])

  legend(
    0,
    32,
    legend = c(
      'Passive Localization',
      'Active Localization'
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n'
  )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}


# Below are the codes to make the above functions run: these make the subplots inside the main plots -----

##These codes make the plots that have confidence intervals and show each experiments learning curves
##this plots reach, localization and no-cursor data.


PlotData <- function(dataset, color, trans, rotate = -1, x =  c(c(1:288), rev(c(1:288)))) {
  colorlist <- c(colorA, colorPA, colorNL, colorNC, colorNNC)
  translist <-
    c(colorA_trans,
      colorPA_trans,
      colorNL_trans,
      colorNC_trans,
      colorNNC_trans)
  dataCIs <- trialCI(data = dataset)
  dataCIs <- dataCIs * rotate
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  x <- x
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = translist[trans], border = NA)
  lines(x[1:length(dataset$Mean)],dataset$Mean * rotate, col = colorlist[color], lwd = 1.5)
}

Plotschedule <- function(dataset) {
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  plot(
    dataset$p1,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Direction [°]",
    axes = F,
    main = "Schedule",
    type = 'l',
    col = 'white'
  )
  rect(65,0,68,30, col = 'grey',border = NA)
  text(67,33,'R1', adj = .5)
  rect(221,0,224,30, col = 'grey',border = NA)
  text(223,33,'R1_late', adj = .5)
  rect(237,-30,240,0, col = 'grey',border = NA)
  text(239,-33,'R2', adj = .5)
  rect(273,-15,288,15, col = 'grey',border = NA)
  text(280,18,'EC', adj = .5)
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))

  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 0.75)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 0.75)
}
PlotoutLine <- function(dataset, exp, color,title) {
  labels <-
    list (
      'Active Localization Group (N=32)',
      'Passive Localization Group (N=32)',
      'Pause Group (N=32)',
      'No-Cursor Group (N=32)',
      'No-Cursor Instructed Group (N=16)',
      'Active Localizations (N=32)',
      'Passive Localizations (N=32)'
    )
  colorlist <- list(colorA, colorPA, colorNL, colorNC, colorNNC)
  label <- labels[exp]
  colors <- colorlist[color]
  dataCIs <- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Direction [°]",
    axes = F,
    main = title,
    type = 'l',
    col = 'white'
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -5,
    -10,
    legend = c(label),
    col = c(unlist(colors)),
    lty = c(1),
    lwd = c(2),
    bty = 'n'
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 0.75)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 0.75)
}




trialCI <- function(data) {
  AllCIs <- data.frame()
  for (trial in 1:nrow(data)) {
    y <- unlist(data[trial, 2:length(data)])
    CItrial <- t.interval(unlist(y))
    if (prod(dim(AllCIs)) == 0) {
      AllCIs <- CItrial
    } else {
      AllCIs <- rbind(AllCIs, CItrial)
    }
  }
  return(AllCIs)
}


t.interval = function(data,
                      variance = var(data, na.rm = TRUE),
                      conf.level = 0.95) {
  z = qt((1 - conf.level) / 2,
         df = length(data) - 1,
         lower.tail = FALSE)
  
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance / length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}


## This plots the data without a confidence interval but will run the model and add the output to the figure. ----
## It reruns the model everytime you plot so it does take a second or two.

Reachmodel <- function(data, name, grid = 'restricted', condition, ncdata = NA, loc_data = NA) {
  grid <- grid
  reaches <- getreachesformodel(data)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = grid,
      checkStability = TRUE
    )
  reach_model <-
    twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  
  if (condition == 'nc'){
    reach_model <- reach_model[33:320, ]
    Plotncmodel(data[33:320, ], name)
    lines(reach_model$total * -1, col = c(rgb(.5, 0., .5)))
    lines(reach_model$slow * -1, col = rgb(0., .5, 1.))
    lines(reach_model$fast * -1, col = rgb(0.0, 0.7, 0.0))
    ncreaches <- getreachesformodel(ncdata)
    lines(x = 33:288, y = ncreaches$meanreaches * -1)
    
  } else if (condition == 'loc') {
    Plotlocmodel(data, name)
    lines(reach_model$total * -1, col = c(rgb(.5, 0., .5)))
    lines(reach_model$slow * -1, col = rgb(0., .5, 1.))
    lines(reach_model$fast * -1, col = rgb(0.0, 0.7, 0.0))
    lines(rowMeans(loc_data[, 2:ncol(loc_data)], na.rm = TRUE))
  } else{
    Plotmodel(data, name)
    lines(reach_model$total * -1, col = c(rgb(.5, 0., .5)))
    lines(reach_model$slow * -1, col = rgb(0., .5, 1.))
    lines(reach_model$fast * -1, col = rgb(0.0, 0.7, 0.0))
  }
  

  MSE<- twoRateReachModelErrors(reach_par, reaches = reaches$meanreaches,schedule = reaches$distortion )
  RMSE<- sqrt(MSE)
  pars<-c(reach_par,MSE, RMSE)
  names(pars)[5]<-"MSE"
  names(pars)[6]<-"RMSE"
  return(pars)
}

Reachmodelnc <- function(data, ncdata, name) {
  reaches <- getreachesformodel(data)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = 'skewed',
      checkStability = TRUE
    )
  reach_model1 <-
    twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  reach_model <- reach_model1[33:320, ]
  Plotncmodel(data[33:320, ], name)
  lines(reach_model$total * -1, col = c(rgb(.5, 0., .5)))
  lines(reach_model$slow * -1, col = rgb(0., .5, 1.))
  lines(reach_model$fast * -1, col = rgb(0.0, 0.7, 0.0))
  ncreaches <- getreachesformodel(ncdata)
  lines(x = 33:288, y = ncreaches$meanreaches * -1)
  return(reach_par)
}



Plotmodel <- function(dataset, name) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l'
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    -10,
    legend = c('Reach data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      rgb(.5, 0., .5),
      rgb(0.0, 0.7, 0.0),
      rgb(0., .5, 1.)
    ),
    lty = c(1, 1, 1, 1),
    lwd = c(2, 2, 2, 2),
    bty = 'n'
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 0.75,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 0.75)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}



Plotncmodel <- function(dataset, name) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l'
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    -10,
    legend = c('Reach data', 'No-cursor data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      rgb(0, 0, 0),
      rgb(.5, 0., .5),
      rgb(0.0, 0.7, 0.0),
      rgb(0., .5, 1.)
    ),
    lty = c(1, 1, 1, 1, 1),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 0.75,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 0.75)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}

Plotlocmodel <- function(dataset, name) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l'
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    -10,
    legend = c('Reach data', 'Localization data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      rgb(0, 0, 0),
      rgb(.5, 0., .5),
      rgb(0.0, 0.7, 0.0),
      rgb(0., .5, 1.)
    ),
    lty = c(1, 1, 1, 1, 1),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 0.75,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 0.75)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}


plotpropmodel<- function (reachdata, locadata){
  
  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  schedule<- reachdata$distortion
  
  
  par<- fitPropModel(reachdata, locadata)
  plot(localizations, type = 'l', ylim = c(-15,15))
  output<- PropModel(par, schedule)
  lines(output, col = "blue")
  
  
}




## this code runs the linear regression between the two pieces of data you give it and it also plots it for you----
## This is called by the above function RegressionPLot(exp) exp 1 is active passive pause and exp 2 is pause and the two no-cursors

plotRegressionWithCI <-
  function(X, Y, colors = c('#99999999', 'black')) {
    # fit regression model
    this.lm <- lm(Y ~ X)
    
    # where is the interesting data
    pointlocs <- seq(min(X, na.rm = TRUE), max(X, na.rm = TRUE), .1)
    
    # get the confidence interval
    y1 = predict(this.lm,
                 newdata = data.frame(X = pointlocs),
                 interval =
                   "confidence")[, "upr"]
    y2 = predict(this.lm,
                 newdata = data.frame(X = pointlocs),
                 interval =
                   "confidence")[, "lwr"]
    
    # show the confidence interval
    polygon(c(pointlocs, rev(pointlocs)),
            c(y1, rev(y2)),
            col = colors[1],
            border = NA)
    
    # and show a regression line:
    lines(
      range(X, na.rm = TRUE),
      predict(this.lm, newdata = data.frame(X = range(X, na.rm = TRUE))),
      col = colors[2],
      lwd = 2
    )
    return(this.lm)
  }


## This one plots the invidual traces of each participant over the group average. ----

PlotIndividualdata <- function (data, exp, title) {
  labels <-
    list (
      'Active Localization Group (N=32)',
      'Passive Localization Group (N=32)',
      'No Localization Group (N=32)',
      'No-Cursor Group (N=32)',
      'Instructed No-Cursor Group (N=32)'
    )
  PlotoutLine(data, exp, exp, title)
  PlotData(data, exp, exp)
  subdata <- data * -1
  participants <- 2:ncol(data)
  for (pn in participants) {
    lines(subdata[, pn], col = rgb(0.0, 0.7, 0.0, 0.06))
  }
  PlotData(data, exp, exp)
}


## This plots the pre and post localization data from the pause, and two no-cursor paradigms -----
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

