svglite(file='doc/MLMC fig 3.svg', width=7, height=8,pointsize = 13, system_fonts=list(sans = "Arial"))
#layout(matrix(c(1,2,3), nrow=3, byrow=TRUE), heights=c(3,1))
par(mfrow = c(3,1))
#want to put both no-cursor datasets on the same figure - their models and the RA and the reaches as a transparent confidence interval
Reachmodel(nocursor_reaches, 'No-Cursor', grid = 'skewed', condition = 'nc', ncdata = nocursor_nocursors, color = colorNC)
Reachmodel(nocursorI_reaches, 'No-Cursor_Instructed', grid = 'skewed', condition = 'nc', ncdata = nocursorI_nocursors, color = colorNNC)
Plotnocursors(active_reaches, nocursor_nocursors, nocursorI_nocursors)
dev.off()
Plotexp2CI(pause_reaches[33:320,],nocursor_reaches[33:320,], nocursorI_reaches[33:320,], pause_reaches[33:320,])


svglite(file='doc/MLMC fig 2.svg', width=10, height=8,pointsize = 13, system_fonts=list(sans = "Arial"))
par(mfrow = c(3,2))
Reachmodel(passive_reaches, 'Passive', condition = 'loc', loc_data = passive_localization, color = colorPA)
Reachmodel(active_reaches, 'Active', condition = 'loc', loc_data = active_localization, color = colorA)
plotfitPropModel(passive_reaches, passive_localization, colorPA, 'Passive Localizations')
plotfitPropModel(active_reaches, active_localization, colorA, 'Active Localizations')
RegressionPLot3P()
RegressionPLotec()
dev.off()


MLMCfig3 <- function (acd,ncd_NC, ncdI) {
  fig3outLine(acd, 4:5, 4:5, "Reach Aftereffects")
  PlotData(ncd_NC, 4, 4, x =  c(c(33:288), rev(c(33:288))))
  PlotData(ncdI, 5, 5, x =  c(c(33:288), rev(c(33:288))))
}

fi3outLine <- function(dataset, exp, color,title) {
  labels <-
    list (
      'No-Cursor Group (N=32)',
      'No-Cursor Instructed Group (N=16)',
      'No Cursor Data',
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
