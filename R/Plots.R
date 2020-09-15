# Codes to create final plots for manuscript -----

loadcolors <- function() {
  ##Active
  colorA       <<- rgb(1.0, 0.4, 0.0)         # orange
  colorA_trans <<- rgb(1.0, 0.4, 0.0, 0.2)     # transparent orange
  
  
  ## Passive
  colorPA       <<- rgb(0.7, 0.0, 0.7)          # purple
  colorPA_trans <<- rgb(0.7, 0.0, 0.7, 0.2)     # transparent purple
  
  
  ## Pause
  colorNL       <<- rgb(0.1, 0.3, 0.5)         # Blue
  colorNL_trans <<- rgb(0.1, 0.3, 0.5, 0.2)     # transparent Blue

  
  
  ##No-Cursor
  colorNC       <<- rgb(0.0, 0.7, 0.0)         # green
  colorNC_trans <<- rgb(0.0, 0.7, 0.0, 0.2)     # transparent green
  
  ##New No-Cursor
  colorNNC       <<- rgb(0.63, 0.71, 0.81)      # blue-gray
  colorNNC_trans <<- rgb(0.63, 0.71, 0.81, 0.2)  # transparent blue-gray
  
  ##Variation
  colorV       <<- rgb(0.63, 0.71, 0.81)      # blue-gray
  colorV_trans <<- rgb(0.63, 0.71, 0.81, 0.2)  # transparent blue-gray
  
  #Terminal
  colorT       <<- rgb(1, 0.0, 0.0)         # Red
  colorT_trans <<- rgb(1, 0.0, 0., 0.2)     # transparent Red
}

## This one plots the invidual traces of each participant over the group average. ----

PlotIndividualdata <- function (data, exp, title, yaxis) {
  
  PlotoutLine(dataset = data,exp = exp,color = exp, title = title, ylabel = yaxis)
  PlotData(data, exp, exp)
  subdata <- data * -1
  participants <- 2:ncol(data)
  for (pn in participants) {
    lines(subdata[, pn], col = rgb(0.0, 0.7, 0.0, 0.06))
  }
  PlotData(data, exp, exp)
}



Plotexp1CI <- function (acd, pad, nld) {
  PlotoutLine(acd, 1:3, 1:3, "Training Trials", 'Reach Deviations [°]')
  PlotData(acd, 1, 1)
  PlotData(pad, 2, 2)
  PlotData(nld, 3, 3)
}

Plotexp2CI <- function (acd, ncd, nld) {
  PlotoutLine(acd, 3:4, 3:4, "Training Trials", 'Reach Deviations [°]')
  PlotData(nld, 3, 3)
  PlotData(ncd, 4, 4)

}

PlotallTapCI <- function (pl = dataset1, al = dataset2) {
  PlotoutLine(pl, 5:6, 5:6, "Hand Localizations", "Hand Localization Shift [°]")
  PlotData(al, 5, 5, 1)
  PlotData(pl, 6, 6, 1)
}



Plotnocursors <- function (acd,ncd_NC, ncdI) {
  PlotoutLine(acd, 7:8, 7:8, "Reach Aftereffects", 'Reach Deviations [°]'  )
  PlotData(ncd_NC, 8, 8, x =  c(c(33:288), rev(c(33:288))))
  PlotData(ncdI, 7, 7, x =  c(c(33:288), rev(c(33:288))))
}


PlotallreachesCI <-
  function (acd = dataset1,
            pad = dataset2,
            nld = dataset3,
            ncd = dataset4) {
    PlotoutLine(acd, 1:4, 1:4, "Training Trials", 'Reach Deviations [°]')
    PlotData(acd, 1, 1)
    PlotData(pad, 2, 2)
    PlotData(nld, 3, 3)
    PlotData(ncd, 4, 4)
  }


####Regression plots ###
  
  
  

RegressionPLotec <- function() {

    PRrm <- TCombine(passive_reaches)
    PRRm <- PRrm$EC_Late * -1
    PPec <- TCombine(passive_localization)
    PPec <- PPec$EC_Late
    plot(
      PPec ~ PRRm,
      col = colorPA_trans,
      xlab = 'Reach Deviations [°]',
      ylab = NA,
      main = 'Error Clamp',
      xlim = c(-30, 30),
      ylim = c(-12, 30),
      axes = FALSE,
      pch = 19, cex.lab = 1.5, cex.main = 1.5,
      asp=1
    )
    axis(2,
         at = c( -10, 0, 10, 20, 30),
         cex.axis = 1.5, las =2)
    axis(1,
         at = c(-30,-20,- 10, 0, 10, 20, 30),
         cex.axis = 1.5)
    lm<-plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    slopes<-lm$coefficients[2]
    intercepts<- lm$coefficients[1]
    rsquareds<-summary(lm)$adj.r.squared
    
    
    Arm <- TCombine(active_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(active_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorA_trans, pch = 19)
    gm<-plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
    slopes<-c(slopes,gm$coefficients[2])
    intercepts<- c(intercepts,gm$coefficients[1])
    rsquareds<-c(rsquareds,summary(gm)$adj.r.squared)

    legend(
      -30,
      30,
      legend = c(
        sprintf('Passive slope =  %.2f, r2 = %.2f', slopes[1],  rsquareds[1]),
        sprintf('Active slope = %.2f, r2 = %.2f', slopes[2],  rsquareds[2])
      ),
      col = c(colorPA, colorA),
      lty = c(1, 1),
      lwd = c(2, 2),
      bty = 'n', cex = 1.25
    )

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
    xlim = c(-32, 32),
    ylim = c(-32, 32),
    main = 'Perturbation Size',
    axes = FALSE, pch = 19, cex.lab = 1.5, cex.main = 1.5, asp = 1
  )
  axis(2,
       at = c(-30,-20, -10,0,10, 20, 30),
       cex.axis = 1.50, las = 2)
  axis(1,
       at = c( -30, 0,  30),
       cex.axis = 1.5)
  lm<-plotRegressionWithCI(pert, loc, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  intercepts<- lm$coefficients[1]
  rsquareds<-summary(lm)$adj.r.squared
  

  APec <- TCombine(active_localization)
  loca <- c(APec$R2,APec$Aligned,APec$R1_Late)
  perta<- c(rep(-30, 32),rep(0, 32),rep(30, 32))
  pertb<- c(rep(-31, 32),rep(-1, 32),rep(29, 32))

  points(loca ~ pertb, col = colorA_trans, pch = 19)
  gm<-plotRegressionWithCI(perta, loca, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  intercepts<- c(intercepts,gm$coefficients[1])
  rsquareds<-c(rsquareds,summary(gm)$adj.r.squared)
  legend(
    -30,
    30,
    legend = c(
      sprintf('Passive slope =  %.2f, r2 = %.2f', slopes[1],  rsquareds[1]),
      sprintf('Active slope = %.2f, r2 = %.2f', slopes[2],  rsquareds[2])
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n', cex = 1.25
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
    pch = 19, cex.lab = 1.5, cex.main = 1.5, asp = 1
  )
  axis(2,
       at = c( -10, 0, 10 ,20, 30),
       cex.axis = 1.5)
  axis(1,
       at = c( 0, 30,  60),
       cex.axis = 1.5)
  lm<-plotRegressionWithCI(pert, loc, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  intercepts<- lm$coefficients[1]
  rsquareds<-summary(lm)$adj.r.squared
  
  
  
  APec <- TCombine(active_localization)
  loca <- c(APec$Aligned,APec$R1_Late,APec$R1_Late - APec$R2)
  perta<- c(rep(0, 32),rep(30, 32),rep(60, 32))  
  pertb<- c(rep(-1, 32),rep(29, 32),rep(59, 32))
  
  points(loca ~ pertb, col = colorA_trans, pch = 19)
  gm<-plotRegressionWithCI(perta, loca, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  intercepts<- c(intercepts,gm$coefficients[1])
  rsquareds<-c(rsquareds,summary(gm)$adj.r.squared)
  

  legend(
    0,
    32,
    legend = c(
      sprintf('Passive slope =  %.2f, r2 = %.2f', slopes[1],  rsquareds[1]),
      sprintf('Active slope = %.2f, r2 = %.2f', slopes[2],  rsquareds[2])
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n', cex = 1.25
  )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}


# Below are the codes to make the above functions run: these make the subplots inside the main plots -----

##These codes make the plots that have confidence intervals and show each experiments learning curves
##this plots reach, localization and no-cursor data.


PlotData <- function(dataset, color, trans, rotate = -1, x =  c(c(1:288), rev(c(1:288)))) {
  colorlist <- c(colorA, colorPA, colorNL, colorNC,colorA, colorPA, colorNNC, colorNC)
  translist <-
    c(colorA_trans,
      colorPA_trans,
      colorNL_trans,
      colorNC_trans,
      colorA_trans,
      colorPA_trans,
      colorNNC_trans,
      colorNC_trans)
  dataCIs <- trialCI(data = dataset)
  dataCIs <- dataCIs * rotate
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  x <- x
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = translist[trans], border = NA)
  lines(x[1:length(dataset$Mean)],dataset$Mean * rotate, col = colorlist[color])
}

Plotschedule <- function(dataset) {
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  plot(
    dataset$p1,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Cursor Rotation [°]",
    axes = F,
    main = "Schedule",
    type = 'l',
    col = 'white', 
    cex.lab = 1.5,
    cex.main = 1.5
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

  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
}
PlotoutLine <- function(dataset, exp, color,title,ylabel) {
  labels <-
    list (
      'Active Localization (N=32)',
      'Passive Localization (N=32)',
      'Pause (N=32)',
      'No-Cursor (N=48)',
      'Active Localizations (N=32)',
      'Passive Localizations (N=32)',
      'No-Cursor Instructed (N=16)',
      'No-Cursor (N=32)'
    )
  colorlist <- list(colorA, colorPA, colorNL, colorNC,colorA, colorPA, colorNNC,colorNC)
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
    ylab = ylabel,
    axes = F,
    main = title,
    type = 'l',
    col = 'white', 
    cex.lab = 1.5,
    cex.main = 1.5
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
    0,
    legend = c(label),
    col = c(unlist(colors)),
    lty = c(1),
    lwd = c(2),
    bty = 'n', 
    cex = 1.5
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
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

Reachmodel <- function(data, name, grid = 'restricted', condition = 'Reach', ncdata = NA, loc_data = NA, color, yaxis = 'Hand Direction [°]') {
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
    Plotncmodel(data[33:320, ], name, color, yaxis)
    lines(reach_model$total * -1, col = 'black',lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    ncreaches <- getreachesformodel(ncdata)
    lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
    
  } else if (condition == 'loc') {
    Plotlocmodel(data, name, color, yaxis)
    lines(reach_model$total * -1, col = 'black',lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    lines(rowMeans(loc_data[, 2:ncol(loc_data)], na.rm = TRUE), col = color)
  } else{
    Plotmodel(data, name, color, yaxis)
    lines(reach_model$total * -1, col = 'black',lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
  }
  

  MSE<- twoRateReachModelErrors(reach_par, reaches = reaches$meanreaches,schedule = reaches$distortion )
  RMSE<- sqrt(MSE)
  pars<-c(reach_par,MSE, RMSE)
  names(pars)[5]<-"MSE"
  names(pars)[6]<-"RMSE"
  return(pars)
}

Reachmodelnc <- function(data, ncdata, name, color) {
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
  Plotncmodel(data[33:320, ], name, color, "Hand Direction [°]")
  lines(reach_model$total * -1, col = 'black',lty = 4)
  lines(reach_model$slow * -1, col = color,lty = 2)
  lines(reach_model$fast * -1, col = color,lty = 3)
  ncreaches <- getreachesformodel(ncdata)
  lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
  return(reach_par)
}



Plotmodel <- function(dataset, name, color, yaxis) {
  title <- sprintf('%s', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = yaxis,
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
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
    0,
    legend = c('Reaches', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      'black',
      color,
      color
    ),
    lty = c(1, 4, 3, 2),
    lwd = c(2, 2, 2, 2),
    bty = 'n', 
    cex = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}



Plotncmodel <- function(dataset, name, color, yaxis) {
  title <- sprintf('%s', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab =  yaxis,
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
  -10,0,
    legend = c('Reaches', 'No-cursors', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
      'black',
      color,
      color
    ),
    lty = c(1, 1, 4, 3, 2),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2, 
    cex = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}

Plotlocmodel <- function(dataset, name, color, yaxis) {
  title <- sprintf('%s', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab =  yaxis,
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
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
    0,
    legend = c('Reaches', 'Localizations', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
      'black',
      color,
      color
    ),
    lty = c(1, 1, 4, 3, 2),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2, 
    cex = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
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


plotfitPropModel<- function(reachdata, locadata, color, title) {

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
  plot(localizations, type = 'l',  ylim = c(-15,15), axes = FALSE, main = title, ylab = "Hand Localization Shift [°]", xlab = "Trial", col = color, cex.lab = 1.5, cex.main = 1.5)
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-15, -10,-5,0, 5,10,15), cex.axis = 1.5, las = 2)
  output<- PropModel(unlist(pargrid[bestpar]), schedule)
  lines(output, col = "black")
  lines(localizations, col = color)
  proportion<- sprintf('Proportion = %.2f', unlist(pargrid[bestpar]))
  print(proportion)
  legend(-10, -2, legend = c('Localization data', 'Model Prediction'), col = c(color, "black"), lty = c(1,1), lwd = 2, bty = 'n', cex = 1.5)
  text(144, 0, labels = proportion, lwd = 2, cex = 1.5)
  #, 'fast', 'slow', , color, color, ,3,2 , ncol =  2

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
  #lines(reach_model$slow * -1, col = color,lty = 2)
  #lines(reach_model$fast * -1, col = color,lty = 3)

  # return(those pars)
  return(unlist(pargrid[bestpar]))

}
