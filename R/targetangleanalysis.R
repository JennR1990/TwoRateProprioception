x<- 1:8
i = 1
y<- c()
SEs<-
angles<- sort(unique(active_prop_angles[,2]))
for (angle in angles) {
  print(angle)
  
y[i]<- mean(active_localization[active_prop_angles == angle], na.rm = TRUE)
SEs[i]<- (sd(active_localization[active_prop_angles == angle], na.rm = TRUE)/ sqrt(length(active_localization[active_prop_angles == angle]))) * 3
i = i +1
}


plot(x, y, ylim = c(-2,10), axes = FALSE, ylab = "Error in Estimated Hand Location", xlab = "Target Angle", main = "Active Localizations", col = colorA)
arrows(x0 = x, y0=y - SEs,  x1 = x, y1 = y + SEs, code = 3, angle = 90, length = .1, col = colorA)
axis(1, at=c(1,2,3,4,5,6,7,8), labels = angles)
axis(2, at= -2:10, labels = -2:10, las = 1)

i = 1
y<- c()
SEs<-
  angles<- sort(unique(passive_prop_angles[,2]))
for (angle in angles) {
  print(angle)
  
  y[i]<- mean(passive_localization[passive_prop_angles == angle], na.rm = TRUE)
  SEs[i]<- (sd(passive_localization[passive_prop_angles == angle], na.rm = TRUE)/ sqrt(length(passive_localization[passive_prop_angles == angle]))) * 3
  i = i +1
}

plot(x, y, ylim = c(-2,10), axes = FALSE, ylab = "Error in Estimated Hand Location", xlab = "Target Angle", main = "Passive Localizations", col = colorPA)
arrows(x0 = x, y0=y - SEs,  x1 = x, y1 = y + SEs, code = 3, angle = 90, length = .1, col = colorPA)
axis(1, at=c(1,2,3,4,5,6,7,8), labels = angles)
axis(2, at= -2:10, labels = -2:10, las = 1)
