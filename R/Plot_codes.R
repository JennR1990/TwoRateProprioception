##Figures we want to make often##

loadcolors <- function() {
  ##Active
  colorA       <<- rgb(1.0, 0.4, 0.0)         # orange
  colorA_trans <<- rgb(1.0, 0.4, 0.0, 0.2)     # transparent orange
  
  ## Passive
  colorPA       <<- rgb(0.0, 0.7, 0.0)         # green
  colorPA_trans <<- rgb(0.0, 0.7, 0.0, 0.2)     # transparent green
  
  
  ## Pause
  colorNL       <<- rgb(0.63, 0.71, 0.81)      # blue-gray
  colorNL_trans <<- rgb(0.63, 0.71, 0.81, 0.2)  # transparent blue-gray
  
  
  ##No-Cursor
  colorNC       <<- rgb(0.0, 0.7, 0.0)         # green
  colorNC_trans <<- rgb(0.0, 0.7, 0.0, 0.2)     # transparent green
  
  ##New No-Cursor
  colorNNC       <<- rgb(0.1,0.3,0.5)         # purple
  colorNNC_trans <<- rgb(0.1,0.3,0.5, 0.2)     # transparent purple
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
    axis(2, at=c(-30,-20,-10,0,10,20,30), cex.axis=0.75)
    axis(1, at=c(-30,-20-10,0,10,20,30), cex.axis=0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    
    
    Arm <- TCombine(active_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(active_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorA)
    plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
    
    PARrm <- TCombine(pause_reaches[33:320, ])
    PARrm <- PARrm[-13, ]
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
        'No-Localization'
      ),
      col = c(rgb(0.5, 0.7, 0.8), rgb(1.0, 0.4, 0.0), rgb(0.7, 0.0, 0.7)),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n'
    )
    
    
    
  } else if (exp == 2) {
    PRrm <- TCombine(pause_reaches[33:320, ])
    PRrm <- PRrm[-13, ]
    PRRm <- PRrm$EC_Late * -1
    PPec <- colMeans(Pause[1:32, 2:32], na.rm = TRUE)
    plot(
      PPec ~ PRRm,
      col = rgb(0.7, 0.0, 0.7),
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-12, 25),
      ylim = c(-12, 25),
      axes = FALSE
    )
    axis(2, at=c(-20,-10,0,10,20), cex.axis=0.75)
    axis(1, at=c(-10,0,10,20,30), cex.axis=0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(rgb(0.7, 0.0, 0.7, 0.2), rgb(0.7, 0.0, 0.7)))
    NCrm <- TCombine(nocursor_reaches[33:320, ])
    NCRm <- NCrm$EC_Late * -1
    NCPec <- colMeans(NoCursor[1:32, 2:33], na.rm = TRUE)
    points(NCPec ~ NCRm, col = rgb(1.0, 0.4, 0.0))
    plotRegressionWithCI(NCRm, NCPec, colors = c(rgb(1.0, 0.4, 0.0, 0.2), rgb(1.0, 0.4, 0.0)))
    NCIrm <- TCombine(nocursorI_reaches[33:320, ])
    NCIRm <- NCIrm$EC_Late * -1
    NCIPec <- colMeans(NewNoC[1:32, 2:11], na.rm = TRUE)
    points(NCIPec ~ NCIRm, col = rgb(0.5, 0.7, 0.8))
    plotRegressionWithCI(NCIRm, NCIPec, colors = c(rgb(0.5, 0.7, 0.8, 0.2), rgb(0.5, 0.7, 0.8)))
    legend(
      -14,
      23,
      legend = c('New No-Cursor', 'No-Cursor', 'No-Localization'),
      col = c(rgb(0.5, 0.7, 0.8), rgb(1.0, 0.4, 0.0), rgb(0.7, 0.0, 0.7)),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n'
    )
    
  }
}



#Codes for actual Plots
Plotexp1CI <- function (acd, pad, nld ){

  PlotoutLineforexp1CI(acd)
  PlotActiveLineReachesCI(acd)
  PlotpassiveLineReachesCI(pad)
  PlotPauseLineReachesCI(nld)

}

Plotexp2CI <- function (acd,ncd, ncdI, nld){
  PlotoutLineforexp1aCI(acd)
  #PlotoutLineforexp2CI(nld)
  PlotnocursorLineReachesCI(ncdI, instruction = TRUE)
  PlotnocursorLineReachesCI(ncd)
  PlotPauseLineReachesCI(nld)
  
}

PlotallreachesCI <- function (acd=dataset1, pad=dataset2, nld=dataset3, ncd=dataset4){
  #svglite(file='all_reaches_CI.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  PlotoutLineforReachesCI(acd)
  PlotActiveLineReachesCI(acd)
  PlotpassiveLineReachesCI(pad)
  PlotPauseLineReachesCI(nld)
  PlotnocursorLineReachesCI(ncd)
  
  #dev.off()
}
PlotallTapCI <- function (pl=dataset1, al=dataset2){
  #svglite(file='all_localizations_CI.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  PlotoutlineTapCI(pl)
  PlotactiveLineTapCI(al)
  PlotpassiveLineTapCI(pl)
  #dev.off()
}


PlotoutLine<- function(dataset){
  svglite(file='doc/paradigm-analysis.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Learning Curves", type = 'l', col= 'white')
  rect(65,25,70,35, col = rgb(.75, .75, .75, .2), border = NULL)
  rect(208,25,224,35, col = rgb(.75, .75, .75, .2), border = NULL)
  rect(237,-25,240,-35, col = rgb(.75, .75, .75, .2), border = NULL)
  rect(256,-5,288,5, col = rgb(.75, .75, .75, .2), border = NULL)
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  dev.off()

}

PlotoutLineforexp1CI<- function(dataset){
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Reach Trials", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-10,-5,legend=c('Active Localization (N=32)','Passive Localization (N=32)','No Localization Group (N=32)'),col=c(rgb(1.0,0.4,0.0),rgb(0.7,0.0,0.7),rgb(0.63,0.71,0.81)),lty=c(1,1,1),lwd=c(2,2,2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=1)
  axis(1, at=c(1,64,224,240,288), cex.axis=1, las = 2)
}

PlotoutLineforexp1aCI<- function(dataset){
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Reach Trials", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-10,-5,legend=c('No-Cursor Group (N=32)', 'No-Cursor Alt Instructions (N=15)', 'No Localization Group (N=32)'),col=c(rgb(0.0,0.7,0.0), rgb(0.1,0.3,0.5), rgb(0.63,0.71,0.81)),lty=c(1,1,1),lwd=c(2,2,2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75, las = 2)
}

PlotoutLineforexp2CI<- function(dataset){

  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Reach Trials", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-10,-5,legend=c( 'No-Cursor Group (N=32)', 'No-Cursor Alt Instructions (N=15)', 'No Localization Group (N=32)'),col=c(rgb(0.0,0.7,0.0), rgb(0.1,0.3,0.5), rgb(0.63,0.71,0.81)),lty=c(1,1,1),lwd=c(2,2,2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75, las = 2)
}


PlotoutLineforReachesCI<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)      # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)
  color2       <- rgb(0.0,0.7,0.0)      # green
  color2_trans <- rgb(0.0,0.7,0.0,0.2)  # transparent green
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Learning Curves", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5,-10,legend=c('Active Localization Group (N=32)','Passive Localization Group (N=32)','No Localization Group (N=32)', 'No-Cursor Group (N=32)'),col=c(rgb(1.0,0.4,0.0),rgb(0.7,0.0,0.7),rgb(0.63,0.71,0.81), rgb(0.0,0.7,0.0)),lty=c(1,1,1,1),lwd=c(2,2,2,2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
}

PlotoutLineforPassiveReaches<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)      # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)
  color2       <- rgb(0.0,0.7,0.0)      # green
  color2_trans <- rgb(0.0,0.7,0.0,0.2)  # transparent green
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Learning Curves", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5,-15,legend=c('Passive Localization Group (N=32)'),col=c(rgb(0.7,0.0,0.7)),lty=c(1),lwd=c(2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
}

PlotoutLineforactiveReaches<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)      # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)
  color2       <- rgb(0.0,0.7,0.0)      # green
  color2_trans <- rgb(0.0,0.7,0.0,0.2)  # transparent green
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Learning Curves", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5,-15,legend=c('Active Localization Group (N=32)'),col=c(rgb(1.0,0.4,0.0)),lty=c(1),lwd=c(2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
}

PlotoutLinefornocursorReaches<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)      # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)
  color2       <- rgb(0.0,0.7,0.0)      # green
  color2_trans <- rgb(0.0,0.7,0.0,0.2)  # transparent green
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Learning Curves", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5,-15,legend=c('No-Cursor Group (N=32)'),col=c(rgb(0.0,0.7,0.0)),lty=c(1),lwd=c(2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
}

PlotoutLineforPauseReaches<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)      # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)
  color2       <- rgb(0.0,0.7,0.0)      # green
  color2_trans <- rgb(0.0,0.7,0.0,0.2)  # transparent green
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Learning Curves", type = 'l', col= 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5,-15,legend=c('No Localization Group (N=32)'),col=c(rgb(0.63,0.71,0.81)),lty=c(1),lwd=c(2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
}
PlotPauseLineReachesCI<- function(dataset){
  color2       <- rgb(0.63,0.71,0.81)      # blue-gray
  color2_trans <- rgb(0.63,0.71,0.81,0.2)  # transparent blue-gray
  dataCIs<- trialCI(data = dataset)
  dataCIs <- dataCIs*-1
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  
  x <- c(c(1:288), rev(c(1:288)))
  y<-c(dataCIs[33:320,1], rev(dataCIs[33:320,2]))
  polygon(x,y, col = color2_trans, border = NA)
  lines(dataset$Mean[33:320]*-1, col = color2, lwd = 1.5)
  
}

PlotActiveLineReachesCI<- function(dataset){
  color3       <- rgb(1.0,0.4,0.0)      # orange
  color3_trans <- rgb(1.0,0.4,0.0,0.2)  # transparent orange
  dataCIs<- trialCI(data = dataset)
  dataCIs <- dataCIs*-1
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  
  x<- c(c(1:nrow(dataset)), rev(c(1:nrow(dataset))))
  y<-c(dataCIs[,1], rev(dataCIs[,2]))
  polygon(x,y, col = color3_trans, border = NA)
  lines(dataset$Mean*-1, col = color3, lwd = 1.5)
  
}

PlotnocursorLineReachesCI<- function(dataset, instruction = FALSE){
  color4       <- rgb(0.0,0.7,0.0)         # green
  color4_trans <- rgb(0.0,0.7,0.0,0.2)     # transparent green
  color1       <- rgb(0.1,0.3,0.5)         # green
  color1_trans <- rgb(0.1,0.3,0.5,0.2)     # transparent green
  
  
  dataCIs<- trialCI(data = dataset)
  dataCIs <- dataCIs*-1
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  x <- c(c(1:288), rev(c(1:288)))
  y<-c(dataCIs[33:320,1], rev(dataCIs[33:320,2]))
  
  if (instruction == TRUE){
    polygon(x,y, col = color1_trans, border = NA)
    lines(dataset$Mean[33:320]*-1, col = color1, lwd = 1.5)
  } else {
  polygon(x,y, col = color4_trans, border = NA)
  lines(dataset$Mean[33:320]*-1, col = color4, lwd = 1.5)
  }
}

PlotpassiveLineReachesCI<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)         # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)     # transparent purple
  dataCIs<- trialCI(data = dataset)
  dataCIs <- dataCIs*-1
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  
  x<- c(c(1:nrow(dataset)), rev(c(1:nrow(dataset))))
  y<-c(dataCIs[,1], rev(dataCIs[,2]))
  polygon(x,y, col = color1_trans, border = NA)
  lines(dataset$Mean*-1, col = color1, lwd = 1.5)
  
}


PlotPausedata<- function (data) {
  PlotoutLineforPauseReaches(pause_reaches)
  PlotPauseLineReachesCI(pause_reaches)
  PauseR<- pause_reaches[33:320,]*-1
  participants <- 2:33
  for (pn in participants) {
    lines(PauseR[,pn], col = rgb(0.0,0.7,0.0,0.06))
  }
  PlotPauseLineReachesCI(pause_reaches)
}
PlotActivedata<- function (data) {
  PlotoutLineforactiveReaches(active_reaches)
  PlotActiveLineReachesCI(active_reaches)
  activeR<- active_reaches*-1
  participants <- 2:33
  for (pn in participants) {
    lines(activeR[,pn], col = rgb(0.0,0.7,0.0,0.06))
  }
  PlotActiveLineReachesCI(active_reaches)
}
PlotPassivedata<- function (data) {
  PlotoutLineforPassiveReaches(passive_reaches)
  PlotpassiveLineReachesCI(passive_reaches)
  PassiveR<- passive_reaches*-1
  participants <- 2:33
  for (pn in participants) {
    lines(PassiveR[,pn], col = rgb(0.0,0.7,0.0,0.06))
  }
  PlotpassiveLineReachesCI(passive_reaches)
}
Plotnocursordata<- function (data) {
  PlotoutLinefornocursorReaches(data)
  PlotnocursorLineReachesCI(data)
  nocursorR<- data[33:320,]*-1
  str(nocursorR)
  participants <- 2:11
  for (pn in participants) {
    lines(nocursorR[,pn], col = rgb(0.0,0.7,0.0,0.6))
  }
  #PlotnocursorLineReachesCI(data)
}


trialCI<- function(data) {
  
  AllCIs <- data.frame()
  for (trial in 1:nrow(data)) {
    y <- unlist(data[trial, 2:length(data)])
    CItrial <-t.interval(unlist(y))
    if (prod(dim(AllCIs)) == 0) {
      AllCIs <- CItrial
    } else {
      AllCIs <- rbind(AllCIs, CItrial)
    }
  }
  return(AllCIs)
}


t.interval = function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  
  z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}


PlotoutlineTapCI<- function(dataset){
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Hand Localization", type = 'l', col = 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-8,-5,legend=c('Active Localizations (N=32)','Passive Localizations (N=32)'),col=c(rgb(1.0,0.4,0.0),rgb(0.7,0.0,0.7)),lty=c(1,1),lwd=c(2,2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75, las = 2)
  
}

PlotpassiveLineTapCI<- function(dataset){
  color1       <- rgb(0.7,0.0,0.7)         # purple
  color1_trans <- rgb(0.7,0.0,0.7,0.2)     # transparent purple
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  x<- c(c(1:nrow(dataset)), rev(c(1:nrow(dataset))))
  y<-c(dataCIs[,1], rev(dataCIs[,2]))
  polygon(x,y, col = color1_trans, border = NA)
  lines(dataset$Mean, col = color1)
  
}
PlotactiveLineTapCI<- function(dataset){
  color5       <- rgb(1.0,0.4,0.0)         # orange
  color5_trans <- rgb(1.0,0.4,0.0,0.2)     # transparent orange
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  x<- c(c(1:nrow(dataset)), rev(c(1:nrow(dataset))))
  y<-c(dataCIs[,1], rev(dataCIs[,2]))
  polygon(x,y, col = color5_trans, border = NA)
  lines(dataset$Mean, col = color5)
  
}

Reachmodelnc<- function(data, ncdata, name) {
  library(RateRate)
  #svglite(file='reach_models_ncdata.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  #layout(matrix(c(1,1,1,1,2,3,4,5), nrow=2, byrow=TRUE), heights=c(3,1))
  #data$distortion<- data$distortion*-1
  reaches<- getreachesformodel(data)
  #reach_par<- fittworatemodel(reaches = reaches$meanreaches, reaches$distortion)
  reach_par <- fitTwoRateReachModel(reaches=reaches$meanreaches, schedule=reaches$distortion, oneTwoRates=2, grid='skewed', checkStability=TRUE)
  #data$distortion<- data$distortion*-1
  #reach_model<-tworatemodel(par=reach_par, distortions = data$distortion)
  reach_model1<- twoRateReachModel(par=reach_par, schedule = reaches$distortion)
  reach_model<- reach_model1[33:320,]
  Plotncmodel(data[33:320,], name)
  lines(reach_model$total*-1, col = c(rgb(.5,0.,.5)))
  lines(reach_model$slow*-1, col = rgb(0.,.5,1.))
  lines(reach_model$fast*-1, col = rgb(0.0,0.7,0.0))
  ncreaches<- getreachesformodel(ncdata)
  lines(x= 33:288,y=ncreaches$meanreaches*-1)
  return(reach_par)
}

Allreachmodels<- function (data1, data2, data3, data4) {
  library("svglite", lib.loc="~/R/win-library/3.4")
  svglite(file='All_reach_models.svg', width=12, height=7, system_fonts=list(sans = "Arial"))
  par(mfrow=c(2,2), mai= c(.85, .68, .68, .1))
  #layout(matrix(c(1,2,3,4),nrow=2, ncol = 2, byrow=TRUE), widths=c(1.5, 1.5, 1.5, 1.5), heights=c(1,1,1,1))
  data1_par<-Reachmodel(data1, 'Active')
#  par(new=TRUE)
 data2_par<-Reachmodel(data2, 'Passive')
 #par(new=TRUE)
 data3_par<-Reachmodel(data3, 'Pause')
 #par(new=TRUE)
 data4_par<-Reachmodel(data4, "No-Cursor")
 pars<-rbind(data1_par, data2_par, data3_par, data4_par)
 return(pars)
 dev.off()
}

Reachmodel<- function(data, name, grid = 'restricted') {
  grid <- grid
  #layout(matrix(c(1,1,1,1,2,3,4,5), nrow=2, byrow=TRUE), heights=c(3,1))
  reaches<- getreachesformodel(data)
  reach_par <- fitTwoRateReachModel(reaches=reaches$meanreaches, schedule=reaches$distortion, oneTwoRates=2, grid=grid, checkStability=TRUE)
  reach_model<- twoRateReachModel(par=reach_par, schedule = reaches$distortion)
  Plotmodel(data, name)
  lines(reach_model$total*-1, col = c(rgb(.5,0.,.5)))
  lines(reach_model$slow*-1, col = rgb(0.,.5,1.))
  lines(reach_model$fast*-1, col = rgb(0.0,0.7,0.0))
  return(reach_par)
}

Plotmodel<- function(dataset, name){
  title<- sprintf('%s Reaches', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:ncol(dataset)], na.rm = TRUE)
  plot(dataset$Mean*-1, ylim = c(-35, 35), xlab = "Trial",lwd= 2, ylab = "Hand Direction [°]",col = c(rgb(0.8,0.8,0.8)), axes = FALSE, main = title, type = 'l')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-10, 2,legend=c('Reach data', 'model','fast','slow'),col=c(rgb(0.44,0.51,0.57), rgb(.5,0.,.5),rgb(0.0,0.7,0.0),rgb(0.,.5,1.)),lty=c(1,1,1,1),lwd=c(2,2,2,2),bty='n')
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75, las = 2)
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  lines(dataset$Mean*-1,col = c(rgb(0.44,0.51,0.57)))
}

Plotncmodel<- function(dataset, name){
  title<- sprintf('%s Reaches', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:ncol(dataset)], na.rm = TRUE)
  plot(dataset$Mean*-1, ylim = c(-35, 35), xlab = "Trial",lwd= 2, ylab = "Hand Direction [°]",col = c(rgb(0.8,0.8,0.8)), axes = FALSE, main = title, type = 'l')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-10, 2,legend=c('Reach data', ' no-cursor data', 'model','fast','slow'),col=c(rgb(0.44,0.51,0.57),rgb(0,0,0), rgb(.5,0.,.5),rgb(0.0,0.7,0.0),rgb(0.,.5,1.)),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2),bty='n', ncol = 2)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75, las = 2)
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  lines(dataset$Mean*-1,col = c(rgb(0.44,0.51,0.57)))
}

experiment1plots<- function (){
  svglite(file='doc/Experiment_1_Figures.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1,2,3,4),nrow=2, ncol = 2, byrow=TRUE), widths=c(1.5, 1.5, 1.5, 1.5), heights=c(1,1,1,1))
  #par(mai = c(1, 0.1, 0.1, 0.1))
  par(mai = c(.32, 0.32, 0.22, 0.22))
  Plotexp1CI(active_reaches, passive_reaches, pause_reaches)
  PlotallTapCI(passive_localization, active_localization)
  Reachmodel(active_reaches, 'Active')
  Reachmodel(passive_reaches, 'Passive')
  dev.off()
}


experiment2plots<- function (){
  
  svglite(file='doc/Experiment_2_Figures.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(2,2))
  par(mai = c(.32, 0.32, 0.22, 0.22))
  Plotexp2CI(active_reaches,nocursor_reaches, nocursorI_reaches, pause_reaches)
  RegressionPLot(2)
  Reachmodelnc(nocursor_reaches, nocursor_nocursors, 'No-Cursor')
  Reachmodelnc(nocursorI_reaches, nocursorI_nocursors, 'New No-Cursor')
  dev.off()
}

experiment1aplots<- function (){
  
  
  svglite(file='doc/Experiment_1a_Figures.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(2,2))
  par(mai = c(.32, 0.32, 0.22, 0.22))
  RegressionPLot(1)
  dev.off()
}

