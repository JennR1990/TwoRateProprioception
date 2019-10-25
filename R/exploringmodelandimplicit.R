distortion<- rep(NA, 512)
train<-seq(from  = 1, to = 512, by = 2)
test<-seq(from  = 2, to = 512, by = 2)
distortion[train]<- nocursor_reaches$distortion[65:320]


reaches<- rep(NA,512)
ncreach<- getreachesformodel(nocursor_reaches)
reaches[train]<-ncreach$meanreaches[65:320]
nc<- getreachesformodel(nocursor_nocursors)
reaches[test]<- nc$meanreaches


Reachslowmodel <- function(data, name, grid = 'restricted', condition = 'Reach', ncdata = NA, loc_data = NA, color) {
  grid <- grid
  #reaches <- getreachesformodel(data)
  reach_par <-
    fitTwoRateReachModel(
      reaches = dataset$Mean,
      schedule = dataset$distortion,
      oneTwoRates = 2,
      grid = 'restricted',
      checkStability = TRUE
    )
  reach_model <-
    twoRateReachModel(par = reach_par, schedule = dataset$distortion)
  
  if (condition == 'nc'){
    reach_model <- reach_model[33:320, ]
    Plotncmodel(data[33:320, ], name, color)
    lines(reach_model$total * -1, col = color,lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    ncreaches <- getreachesformodel(ncdata)
    lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
    
  } else if (condition == 'loc') {
    Plotlocmodel(data, name, color)
    lines(reach_model$total * -1, col = color,lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    lines(rowMeans(loc_data[, 2:ncol(loc_data)], na.rm = TRUE), col = color)
  } else{
    Plotslowmodel(data, name, color)
    lines(reach_model$total * -1, col = color,lty = 4)
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



Plotslowmodel <- function(dataset, name, color) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  #dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 384, 384, 416, 416),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(416, 512),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    -5,
    legend = c('Reach data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
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
    at = c(1, 64, 384, 416, 512),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}