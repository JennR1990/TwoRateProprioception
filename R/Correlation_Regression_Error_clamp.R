#plotting localization ~ reaches in error clamp phase

# this to see if a larger rebound causes a larger shift in prop. 


APrm<- RepeatedMeasuresCombine1or5(Active_Prop)
ARrm<- RepeatedMeasuresCombine1or5(Active_Reach)
ARRm<-ARrm$EC_Late*-1
PPrm<- RepeatedMeasuresCombine1or5(Passive_Prop)
PPrm[15,]<- NA
PRrm<- RepeatedMeasuresCombine1or5(Passive_Reach)
PRRm<-PRrm$EC_Late*-1
PRRm[15]<- NA


PRrm<- TCombine(passive_reaches[33:320,])
PRrm<- PRrm[-13,]
PRRm<-PRrm$EC_Late*-1
PPec<- colMeans(Pause[1:32, 2:32], na.rm = TRUE)
plot(PPec~PRRm, col = rgb(0.7,0.0,0.7), xlab = 'Reaches', ylab = 'Localization', main = 'Localization ~ Reaches During Error Clamp', xlim = c(-12,25), ylim = c(-12,25))
plotRegressionWithCI(PRRm,PPec,colors=c(rgb(0.7,0.0,0.7,0.2), rgb(0.7,0.0,0.7)))
NCrm<- TCombine(nocursor_reaches[33:320,])
NCRm<-NCrm$EC_Late*-1
NCPec<- colMeans(NoCursor[1:32, 2:33], na.rm = TRUE)
points(NCPec~NCRm, col = rgb(1.0,0.4,0.0))
plotRegressionWithCI(NCRm,NCPec,colors=c(rgb(1.0,0.4,0.0,0.2), rgb(1.0,0.4,0.0)))


NCIrm<- TCombine(nocursorI_reaches[33:320,])
NCIRm<-NCIrm$EC_Late*-1
NCIPec<- colMeans(NewNoC[1:32, 2:11], na.rm = TRUE)
points(NCIPec~NCIRm, col = rgb(0.5,0.7,0.8))
plotRegressionWithCI(NCIRm,NCIPec,colors=c(rgb(0.5,0.7,0.8,0.2), rgb(0.5,0.7,0.8)))
legend(-14,23,legend=c('New No-Cursor', 'No-Cursor','No-Localization'),col=c(rgb(0.5,0.7,0.8), rgb(1.0,0.4,0.0), rgb(0.7,0.0,0.7)),lty=c(1,1,1),lwd=c(2,2,2),bty='n')




# APR<- RepeatedMeasuresCombine1or5(AP)
# ARR<- RepeatedMeasuresCombine1or5(AR)
# ARRm<-ARR$EC_Late*-1
# PPR<- RepeatedMeasuresCombine1or5(PP)
# PRR<- RepeatedMeasuresCombine1or5(PR)
# PRRm<-PRR$EC_Late*-1


##Passive = (0.7,0.0,0.7)
plot(APrm$EC_Late~ARRm, col = rgb(1.0,0.4,0.0), xlab = 'Reaches', ylab = 'Localization', main = 'Localization ~ Reaches During Error Clamp', xlim = c(-12,25), ylim = c(-12,25))
abline(h= 0, col= '#99999999', lty= 2)
abline(v=0, col= '#99999999', lty= 2)
points(PPrm$EC_Late~PRRm, col = rgb(0.7,0.0,0.7))
plotRegressionWithCI(PRRm,PPrm$EC_Late,colors=c(rgb(0.7,0.0,0.7,0.2), rgb(0.7,0.0,0.7)))
plotRegressionWithCI(ARRm,APrm$EC_Late,colors=c(rgb(1.0,0.4,0.0,0.2), rgb(1.0,0.4,0.0)))
legend(-14,23,legend=c('Active Localization; R2 = .545','Passive Localization; R2 = .347'),col=c(rgb(1.0,0.4,0.0), rgb(0.7,0.0,0.7)),lty=c(1,1),lwd=c(2,2),bty='n')



legend(-24,8,legend=c('R = .738, R2 = .545','R = .589, R2 = .347'),col=c('red', 'blue'),lty=c(1,1),lwd=c(2,2),bty='n')
A.lm<- lm(APrm$EC_Late~ARRm)
cooksdA<-cooks.distance(A.lm)
cutoffA <- 4/((31-length(A.lm$coefficients)-2))
which(cooksdA>cutoffA)
which(cooksdA==max(cooksdA))
p.lm<- lm(PPrm$EC_Late~PRRm)
cooksdP<-cooks.distance(P.lm)
cutoffP <- 4/((32-length(P.lm$coefficients)-2))
which(cooksdP>cutoffP)
which(cooksdP==max(cooksdP))



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

