svglite(file='doc/Fig 5.svg', width=12, height=4, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(2))
Plotexp2CI(pause_reaches[33:320,],newnocursor_reaches[33:320,], pause_reaches[33:320,])
Reachmodel(newnocursor_reaches, 'No-Cursor', grid = 'skewed', condition = 'nc', ncdata = newnocursor_nocursors, color = colorNC, yaxis = 'Hand Direction [°]')
dev.off()


svglite(file='doc/Fig 4.svg', width=12, height=8, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,1,2,2,3,3,4,4,4,5,5,5), nrow=2, byrow=TRUE), heights=c(2,2))
RegressionPLot3P()
RegressionPLotchange()
RegressionPLotec()
plotfitPropModel(active_reaches, active_localization, colorA, 'Active Localizations')
plotfitPropModel(passive_reaches, passive_localization, colorPA, 'Passive Localizations')
dev.off()


svglite(file='doc/Fig 3.svg', width=12, height=8, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(2,2))
Plotexp1CI(active_reaches, passive_reaches, pause_reaches[33:320,])
PlotallTapCI(passive_localization, active_localization)
Reachmodel(active_reaches, 'Active Localization', condition = 'loc', loc_data = active_localization, color=colorA)
Reachmodel(passive_reaches, 'Passive Localization', condition = 'loc', loc_data = passive_localization, color=colorPA , yaxis = 'Hand Direction [°]')
dev.off()


svglite(file='doc/fig 1.svg', width=12, height=4, system_fonts=list(sans = "Arial"))
Plotschedule(active_reaches)
dev.off()
