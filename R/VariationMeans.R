
Getshiftsperrotation<- function() {
  VR_Data<- getreachesformodel(variation_reaches)
  VP_Data<- getreachesformodel(variation_localization)
g<- seq(from = 50, to = 480, by = 12)
h<- seq(from = 61, to = 481, by = 12)
h[36]<- h[36]-1
rotation<- c()
localizations<- c()


for (i in 1:length(g)) {
  
  localizations[i]<- mean(VP_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
  rotation[i]<- variation_reaches$distortion[g[i]]
}

return(variation_prop<- data.frame(rotation, localizations))
}



Getreachesperrotation<- function() {
  VR_Data<- getreachesformodel(variation_reaches)
  g<- seq(from = 50, to = 480, by = 12)
  h<- seq(from = 61, to = 481, by = 12)
  h[36]<- h[36]-1
  rotation<- c()
  reaches<- c()
  
  
  for (i in 1:length(g)) {
    
    
    reaches[i]<- mean(VR_Data$meanreaches[g[i]:h[i]], na.rm = TRUE)
    rotation[i]<- variation_reaches$distortion[g[i]]
  }
  rotation[rotation == 360]<- NA
  return(variation_reach<- data.frame(rotation, reaches))
}



variation_reaches$distortion[variation_reaches$distortion == 360]<- NA


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
      sprintf('Passive Y = %.2fx + %.2f, r2 = %.2f', slopes[1], intercepts[1], rsquareds[1]),
      sprintf('Active Y = %.2fx + %.2f, r2 = %.2f', slopes[2], intercepts[2], rsquareds[2])
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n', cex = 1.25
  )
  
}

PlotoutLine <- function(dataset) {
  labels <-
    list (
      'Reaches',
      'Localizations')
  label <- labels[1:2]
  colors <- colorNNC
  dataCIs <- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Location [°]",
    axes = F,
    main = "Variation Experiment",
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


##Creates the necessary lists of numbers from the perturbation lists to make the plot

makingschedule<- function (){
  
vprop<- Getshiftsperrotation()
vreac<- Getreachesperrotation()
localizations<-vprop$localizations
Variation_means<- cbind(vreac,localizations)


g<- seq(from = 50, to = 480, by = 12)
h<- seq(from = 61, to = 481, by = 12)
h[36]<- h[36]-2

z<-c(0,50)
for (i in 1:36) {
  
  z<<- c(z, g[i], h[i]+1)
}


sizes<- c(0,0)

for (i in 1:36){
  
  sizes<<- c(sizes,Variation_means$rotation[i],Variation_means$rotation[i]) 
  
}


}
##Finds out which places have the NAs so we can use that info for making the perturbation schedule
v<- NA
counter<- 1
g<- c()
for (i in 1:length(sizes)){

  
  if (is.na(sizes[i])){
    print(i)
    g[counter]<-i
    v<- c(v,z[i])
    counter<-counter+1
  }

}
v<- v[-1]



##This turns the NA's into zeros for plottign the perturbation.
plotvariation<- function (){
  g<- seq(from = 50, to = 480, by = 12)
  g<- c(1,g,480)
for (i in 1:length(sizes)){
  
  
  if (is.na(sizes[i])){
    sizes[i]<- 0
  }
  
}
plot(variation_reaches$distortion, col = 'white', axes = F,cex.lab = 1.5,
     cex.main = 1.5,    xlab = "Trial",
     ylab = "Hand Location [°]")

lines(x = z[1:25], y = sizes[1:25], type = 'l')
lines(x = z[25:26], y = c(0,0), lty = 2)
lines(x = z[26:33], y = sizes[26:33], type = 'l')
lines(x = z[33:36], y = c(0,0,0,0), lty = 2)
lines(x = z[36:51], y = sizes[36:51], type = 'l')
lines(x = z[51:52], y = c(0,0), lty = 2)
lines(x = z[52:61], y = sizes[52:61], type = 'l')
lines(x = z[61:62], y = c(0,0), lty = 2)
lines(x = z[62:71], y = sizes[62:71], type = 'l')
lines(x = z[71:72], y = c(0,0), lty = 2)

legend(
  -5,
  30,
  legend = c(
    'Reaches',
    'Localizations'),
  col = c('blue', 'red'),
  lty = c(1),
  lwd = c(2),
  bty = 'n', 
  cex = 1.25
)
axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
     las = 2)
axis(1, at = g, cex.axis = .75, las = 2)
reachdata<- getreachesformodel(variation_reaches)
lines(reachdata$meanreaches*-1, type = 'l', col = 'Blue')
locdata<- getreachesformodel(variation_localization)
lines(locdata$meanreaches, type = 'l', col = 'red')
dataCIs <- trialCI(data = variation_localization)
dataCIs <- dataCIs
x <-  c(c(1:480), rev(c(1:480)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = rgb(1,0,0,.2), border = NA)

dataCIs <- trialCI(data = variation_reaches)
dataCIs <- dataCIs*-1
x <-  c(c(1:480), rev(c(1:480)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = rgb(0,0,1,.2), border = NA)

}


