svglite(file='doc/MLMC fig 3.svg', width=7, height=8,pointsize = 13, system_fonts=list(sans = "Arial"))
#layout(matrix(c(1,2,3), nrow=3, byrow=TRUE), heights=c(3,1))
par(mfrow = c(3,1))
#want to put both no-cursor datasets on the same figure - their models and the RA and the reaches as a transparent confidence interval
Reachmodel(nocursor_reaches, 'No-Cursor', grid = 'skewed', condition = 'nc', ncdata = nocursor_nocursors, color = colorNC)
Reachmodel(nocursorI_reaches, 'No-Cursor_Instructed', grid = 'skewed', condition = 'nc', ncdata = nocursorI_nocursors, color = colorNNC)
Plotnocursors(active_reaches, nocursor_nocursors, nocursorI_nocursors)
dev.off()
Plotexp2CI(pause_reaches[33:320,],nocursor_reaches[33:320,], nocursorI_reaches[33:320,], pause_reaches[33:320,])


svglite(file='doc/MLMC fig 2new_updated.svg', width=15, height=10,pointsize = 13, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4,5,6), nrow=3, byrow=TRUE), heights=c(1,1,1))
layout(matrix(c(1,1,2,2,0,0,3,3,4,4,5,6), nrow=2, byrow=TRUE) )
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(1,1))
#par(mfrow = c(3,2))
Reachmodel(passive_reaches, 'Passive', condition = 'loc', loc_data = passive_localization, color = colorPA)
Reachmodel(active_reaches, 'Active', condition = 'loc', loc_data = active_localization, color = colorA)
plotfitPropModel(passive_reaches, passive_localization, colorPA, 'Passive Localizations')
plotfitPropModel(active_reaches, active_localization, colorA, 'Active Localizations')
dev.off()
svglite(file='doc/MLMC fig 2a_updated.svg', width=5, height=4,pointsize = 13, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(1,1))
RegressionPLot3P()
RegressionPLotec()
dev.off()

svglite(file='doc/MLMCnocursor.svg', width=17, height=10,pointsize = 13, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,1,2,2,0,0,3,3,4,4,5,6), nrow=2, byrow=TRUE) )
fi3outLine(nocursor_reaches,1:6, 1:2, ' Reach Aftereffects', nocursorI_reaches, grid = 'skewed', nocursor_nocursors, nocursorI_nocursors)
dev.off()


MLMCfig3 <- function (acd,ncd_NC, ncdI) {
  fig3outLine(acd, 4:5, 4:5, "Reach Aftereffects")
  PlotData(ncd_NC, 4, 4, x =  c(c(33:288), rev(c(33:288))))
  PlotData(ncdI, 5, 5, x =  c(c(33:288), rev(c(33:288))))
}

fi3outLine <- function(dataset1, exp, color,title, dataset2, grid, ncdata1, ncdata2) {
  labels <-
    list (
      'RAE (N=32)',
      'RAE-I(N=16)',
      'No Cursor data',
      'model','fast', "slow"
    )
  translist <-
    c(colorNC_trans,
      colorNNC_trans)
  colorlist <- list(colorNC, colorNNC)
  label <- labels[exp]
  colors <- colorlist[color]
  dataCIs1 <- trialCI(data = dataset1)
  dataCIs2 <- trialCI(data = dataset2)
  
  dataset1["distortion"][is.na(dataset1["distortion"])] <- 0
  dataset1$Mean <-
    rowMeans(dataset1[, 2:length(dataset1)], na.rm = TRUE)
  plot(
    dataset1$Mean,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Direction [°]",
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
    col = c(unlist(colors), 'black', 'black', 'black', 'black'),
    lty = c(1,1,1,4,3,2),
    lwd = c(2),
    bty = 'n', 
    cex = 1.5
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
  PlotData(nocursor_reaches[33:320,], 4, 4)
  PlotData(nocursorI_reaches[33:320,], 5, 5)
  
  grid <- grid
  reaches <- getreachesformodel(dataset1[33:320,])
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
  lines(reach_model$total * -1, col = unlist(colors[1]),lty = 4)
  lines(reach_model$slow * -1, col = unlist(colors[1]),lty = 2)
  lines(reach_model$fast * -1, col = unlist(colors[1]),lty = 3)
  ncreaches <- getreachesformodel(ncdata1)
  lines(x = 33:288, y = ncreaches$meanreaches * -1, col = colorNC)
  
  reaches <- getreachesformodel(dataset2[33:320,])
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
  lines(reach_model$total * -1, col = unlist(colors[2]),lty = 4)
  lines(reach_model$slow * -1, col = unlist(colors[2]),lty = 2)
  lines(reach_model$fast * -1, col = unlist(colors[2]),lty = 3)
  ncreaches <- getreachesformodel(ncdata2)
  lines(x = 33:288, y = ncreaches$meanreaches * -1, col = colorNNC)
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
