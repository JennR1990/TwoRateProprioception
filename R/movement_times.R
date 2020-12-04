
plotmovetimes<- function(dataset, color = 4, trans = 4, exp = c(7,8), colors = c(7,8), dataset1, color2 = 7, trans2 = 7){
  
  

dataset$Mean <-
  rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
labels <-
  list (
    'Active Localization (N=32)',
    'Passive Localization (N=32)',
    'Pause (N=32)',
    'No-Cursor (N=48)',
    'Active Localizations (N=32)',
    'Passive Localizations (N=32)',
    'No-Cursor Instructed (N=16)',
    'No-Cursor Uninstructed (N=32)'
  )
label <- labels[exp]


colorlist <- c(colorA, colorPA, colorNL, colorNC,colorA, colorPA, colorNNC, colorNC)
allcolors <- colorlist[colors]
translist <-
  c(colorA_trans,
    colorPA_trans,
    colorNL_trans,
    colorNC_trans,
    colorA_trans,
    colorPA_trans,
    colorNNC_trans,
    colorNC_trans)

plot( x = c(33:288),
  y = dataset$Mean,
  ylim = c(0, 2500),
  xlim = c(1,288),
  xlab = "Trial",
  ylab = "Movement Time [ms]",
  axes = F,
  main = "No-Cursor Movement Times",
  type = 'l',
  col = colorlist[color], 
  cex.lab = 1.5,
  cex.main = 1.5
)

abline( v = c(64, 224, 240))
text(x = 32, y = 2000, labels = "Aligned")
text(x = 144, y = 2000, labels = "First Rotation")
text(x = 232, y = 2000, labels = "Reversal", srt = 90)
text(x = 260, y = 2000, labels = "Clamped", srt = 90)
legend(
  0,
  500,
  legend = c(label),
  col = c(unlist(allcolors)),
  lty = c(1),
  lwd = c(2),
  bty = 'n', 
  cex = 1.5
)
axis(2, at = c( 0, 500, 1000, 1500, 2000, 2500), cex.axis = 1.5)
axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)


dataCIs <- trialCI(data = dataset)
dataset["distortion"][is.na(dataset["distortion"])] <- 0
x <- c(c(33:288), rev(c(33:288)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = translist[trans], border = NA)
lines(x[1:length(dataset$Mean)],dataset$Mean, col = colorlist[color])

dataCIs <- trialCI(data = dataset1)
dataset1["distortion"][is.na(dataset1["distortion"])] <- 0
x <- c(c(33:288), rev(c(33:288)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = translist[trans2], border = NA)
dataset1$Mean <-rowMeans(dataset1[, 2:length(dataset1)], na.rm = TRUE)
lines(x[1:length(dataset1$Mean)],dataset1$Mean, col = colorlist[color2])

}

movementT<- function(data, exp1, exp2, task) {
  print(sprintf('this is the between subjects comparison of movement time %s to %s %s Data', exp1, exp2, task))
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  
  
}

PairedMovementT<- function(data, exp1, task) {
  print(sprintf('this is the within subjects analysis of movement time for %s %s Data', exp1, task))
  print('Is there early learning?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE)) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], na.rm = TRUE))
  print('Did they return to baseline? (Should not)')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE))
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], na.rm = TRUE))
  print('Was there learning from beginning to end of 1st rotation?')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], na.rm = TRUE))
  print('Was there learning from 1st to 2nd block of 1st rotation?')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_second[data$Experiment == exp1], na.rm = TRUE))
  print('How much could they learn of the 60 degree change?')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # not sig A vs. NC
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], na.rm = TRUE)) # not sig A vs. NC
  print('Do their error clamp trials reflect the trajectories in 2nd rotation?')
  print(t.test(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE)) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # not sig  A vs. NC
  print(etaSquaredTtest(data$R2[data$Experiment == exp1],data$EC[data$Experiment == exp1], na.rm = TRUE)) # not sig  A vs. NC
  print('Is there a change in error clamps across time? (Decay)')
  print(t.test(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # p-value = 0.005945  A vs. NC
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE)) # p-value = 0.005945  A vs. NC
  print('Did they just revert back to aligned behaviour when dealing with 2nd rotation?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R2[data$Experiment == exp1], na.rm = TRUE)) 
  print('Are the 1st few error clamp trials the same as their aligned behaviour?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))  #p-value = 1.36e-07  A vs. NC
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], na.rm = TRUE))  #p-value = 1.36e-07  A vs. NC
  print('Did they eventually decay back to baseline?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE))
  print('Were the error clamp trials similar to the 1st rotation?')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE))
  
}
