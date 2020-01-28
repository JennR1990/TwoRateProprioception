##making table 1

##first have to get all the metrics for each of the four groups and the group parameters

Active_metrics<- GroupModelAICs(active_reaches, 'Active')
Passive_metrics<- GroupModelAICs(passive_reaches, 'Passive')
Pause_metrics<- GroupModelAICs(pause_reaches, 'Pause')
NC_metrics<- GroupModelAICs(newnocursor_reaches, 'No-Cursor', 'skewed')
metrics<- rbind(Active_metrics, Passive_metrics, Pause_metrics, NC_metrics)


ActivePars<-Reachmodel(active_reaches, 'Active', color = colorA)
PassivePars<-Reachmodel(passive_reaches, 'Passive', color = colorPA)
PausePars<-Reachmodel(pause_reaches[33:320,], 'Pause', color = colorNL)
AllNoCPars<-Reachmodel(newnocursor_reaches[33:320,], 'No-Cursor', color = colorNC)
pars<- rbind(ActivePars,PassivePars, PausePars, AllNoCPars)


Group<- c("Active Localization", "Passive Localization", "Pause", "No-Cursor")

metrics<- cbind(Group, pars, metrics)
rownames(metrics)<- c()
metrics<- metrics[,-7]
metrics<- metrics[,-9]
metrics[,2:9]<- round(metrics[,2:9], digits = 3)

formattable(metrics, align = c('r','c','c','c','c','c','c','c','r'), list(
  Group = formatter("span", style = ~ style(color = "red", font.weight = 'bold')),
  `oneRate<br>likelihood` = color_bar('green')))
