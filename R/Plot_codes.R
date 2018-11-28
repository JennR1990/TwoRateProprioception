countdeletedtrials<- function (data) {
  R <- nrow(data)
  C <- ncol(data)
  deleted<- sum(is.na(data[,2:C]))
  C-1
  total<- R*C
  percentdeleted<- deleted/total*100
  return(percentdeleted)
}

##Figures we want to make often##
# color1       <- rgb(0.7,0.0,0.7)         # purple
# color1_trans <- rgb(0.7,0.0,0.7,0.2)     # transparent purple
# color2       <- rgb(0.63,0.71,0.81)      # blue-gray
# color2_trans <- rgb(0.63,0.71,0.81,0.2)  # transparent blue-gray
# color4       <- rgb(0.0,0.7,0.0)         # green
# color4_trans <- rgb(0.0,0.7,0.0,0.2)     # transparent green
# color5       <- rgb(1.0,0.4,0.0)         # orange
# color5_trans <- rgb(1.0,0.4,0.0,0.2)     # transparent orange

PlotPausedata<- function (data) {
PlotoutLineforReachesCI(pause_reaches)
PlotPauseLineReachesCI(pause_reaches)
PauseR<- pause_reaches[33:320,]*-1
participants <- 2:33
for (pn in participants) {
  
 lines(PauseR[,pn], col = rgb(0.0,0.7,0.0,0.06))
}
PlotPauseLineReachesCI(pause_reaches)
}
#reaches overlayed with confidence intervals
PlotallreachesCI(active_reaches,passive_reaches, pause_reaches,nocursor_reaches)


#localizations overlayed with confidence intervals
PlotallTapCI(passive_localization, active_localizations)

#reach data avergaed for similar training and their model output
#Reachmodel(pp_reaches)
#Reachmodelnc(acnc_reaches,nocursor_nocursors)


# > library("svglite", lib.loc="~/R/win-library/3.4")
# > svglite(file='active_p_fig.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
# > plot(Active_p, type='l')
# > dev.off()


#Codes for actual Plots
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
#  PlotallreachesCI <- function (dataset1, dataset2, dataset3, dataset4){
#    datasets<- c(dataset1, dataset2, dataset3, dataset4)
#    
#    PlotoutLineforReachesCI(dataset1)
# for (group in datasets) {
#   if ('active_reaches' == group) {
#     PlotActiveLineReachesCI(group)
#   } else if ('passive_reaches' == group) {
#     PlotpassiveLineReachesCI(group)
#   } else if ('pause_reaches' == group) {
#     PlotpauseLineReachesCI(group)
#   }else if ('nocursor_reaches' == group) {
#     PlotnocursorLineReachesCI(group)
# #   }
# # }
# 
# 
# 
# return(datasets)
#  }



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
  legend(-5,-15,legend=c('Active Localization Group (N=32)','Passive Localization Group (N=32)','No Localization Group (N=32)', 'No-Cursor Group (N=32)'),col=c(rgb(1.0,0.4,0.0),rgb(0.7,0.0,0.7),rgb(0.63,0.71,0.81), rgb(0.0,0.7,0.0)),lty=c(1,1,1,1),lwd=c(2,2,2,2),bty='n')
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

PlotnocursorLineReachesCI<- function(dataset){
  color4       <- rgb(0.0,0.7,0.0)         # green
  color4_trans <- rgb(0.0,0.7,0.0,0.2)     # transparent green
  dataCIs<- trialCI(data = dataset)
  dataCIs <- dataCIs*-1
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  
  x <- c(c(1:288), rev(c(1:288)))
  y<-c(dataCIs[33:320,1], rev(dataCIs[33:320,2]))
  polygon(x,y, col = color4_trans, border = NA)
  lines(dataset$Mean[33:320]*-1, col = color4, lwd = 1.5)
  
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
  plot(dataset$Mean, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [°]",axes=F, main = "Active vs. Passive Proprioceptive Localizations", type = 'l', col = 'white')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5,-15,legend=c('Active Localizations (N=32)','Passive Localizations (N=32)'),col=c(rgb(1.0,0.4,0.0),rgb(0.7,0.0,0.7)),lty=c(1,1),lwd=c(2,2),bty='n')
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  
}

PlotpassiveLineTapCI<- function(dataset){
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
  dataCIs<- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:length(dataset)], na.rm = TRUE)
  #plot(dataset$distortion*-1, ylim = c(-35, 35), xlab = "Trial", ylab = "Hand Direction [deg]", main = paste("Experiment", enum, "Reaches"), type = 'l')
  x<- c(c(1:nrow(dataset)), rev(c(1:nrow(dataset))))
  y<-c(dataCIs[,1], rev(dataCIs[,2]))
  polygon(x,y, col = color5_trans, border = NA)
  lines(dataset$Mean, col = color5)
  
}

Reachmodelnc<- function(data, ncdata) {
  svglite(file='reach_models_ncdata.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1,1,1,1,2,3,4,5), nrow=2, byrow=TRUE), heights=c(3,1))
  data$distortion<- data$distortion*-1
  reaches<- getreachesformodel(data)
  reach_par<- fittworatemodel(reaches = reaches$meanreaches, reaches$distortion)
  data$distortion<- data$distortion*-1
  reach_model<-tworatemodel(par=reach_par, distortions = data$distortion)
  Plotmodel(data)
  lines(reach_model$output, col = c(rgb(.5,0.,.5)))
  lines(reach_model$slow, col = rgb(0.,.5,1.))
  lines(reach_model$fast, col = rgb(0.0,0.7,0.0))
  ncreaches<- getreachesformodel(ncdata)
  lines(x= 33:288,y=ncreaches$meanreaches*-1)
  return(reach_par)
}
Reachmodel<- function(data) {
  svglite(file='reach_models_passive_pause.svg', width=8, height=5, system_fonts=list(sans = "Arial"))
  layout(matrix(c(1,1,1,1,2,3,4,5), nrow=2, byrow=TRUE), heights=c(3,1))
  data$distortion<- data$distortion*-1
  reaches<- getreachesformodel(data)
  reach_par<- fittworatemodel(reaches = reaches$meanreaches, reaches$distortion)
  data$distortion<- data$distortion*-1
  reach_model<-tworatemodel(par=reach_par, distortions = data$distortion)
  Plotmodel(data)
  lines(reach_model$output, col = c(rgb(.5,0.,.5)))
  lines(reach_model$slow, col = rgb(0.,.5,1.))
  lines(reach_model$fast, col = rgb(0.0,0.7,0.0))
  return(reach_par)
}

Plotmodel<- function(dataset){
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[,2:ncol(dataset)], na.rm = TRUE)
  plot(dataset$Mean*-1, ylim = c(-35, 35), xlab = "Trial",lwd= 2, ylab = "Hand Direction [deg]",col = c(rgb(0.8,0.8,0.8)), axes = FALSE, main = "Two-Rate Model Applied to Reaches", type = 'l')
  lines(c(1,64,64,224,224,240,240),c(0,0,30,30,-30,-30,0),col=rgb(0.,0.,0.))
  lines(c(240,288),c(0,0),lty=2,col=rgb(0.,0.,0.))
  legend(-5, -3,legend=c('Reach data','No-Cursors', 'model','fast','slow'),col=c(rgb(0.44,0.51,0.57),col ='black', rgb(.5,0.,.5),rgb(0.0,0.7,0.0),rgb(0.,.5,1.)),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2),bty='n')
  axis(1, at=c(1,64,224,240,288), cex.axis=0.75)
  axis(2, at=c(-30,-15,0,15,30), cex.axis=0.75)
  lines(dataset$Mean*-1,col = c(rgb(0.44,0.51,0.57)))
}