##Figure 1
tiff(filename='Figure 1.tiff', res=600, width=12, height=4, units='in', compression='lzw')
layout(matrix(c(1,2), nrow=1, byrow=TRUE), heights=c(2))
Plotexp2CI(pause_reaches[33:320,],newnocursor_reaches[33:320,], pause_reaches[33:320,])
mtext('a', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
Reachmodel(newnocursor_reaches, 'No-Cursor', grid = 'skewed', condition = 'nc', ncdata = newnocursor_nocursors, color = colorNC, yaxis = 'Hand Direction [°]')
mtext('b', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
dev.off()



##Figure 2
tiff(filename='Figure 2.tiff', res=600, width=12, height=8, units='in', compression='lzw')
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(2,2))
Plotexp1CI(active_reaches, passive_reaches, pause_reaches[33:320,])
mtext('a', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
PlotallTapCI(passive_localization, active_localization)
mtext('b', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
Reachmodel(active_reaches, 'Active Localization', condition = 'loc', loc_data = active_localization, color=colorA)
mtext('c', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
Reachmodel(passive_reaches, 'Passive Localization', condition = 'loc', loc_data = passive_localization, color=colorPA , yaxis = 'Hand Direction [°]')
mtext('d', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
dev.off()

##Figure 3
tiff(filename='Figure 3.tiff', res=600, width=12, height=8, units='in', compression='lzw')
layout(matrix(c(1,1,2,2,3,3,4,4,4,5,5,5), nrow=2, byrow=TRUE), heights=c(2,2))
RegressionPLot3P()
mtext('a', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
RegressionPLotchange()
mtext('b', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
RegressionPLotec()
mtext('c', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
plotfitPropModel(active_reaches, active_localization, colorA, 'Active Localizations')
mtext('d', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
plotfitPropModel(passive_reaches, passive_localization, colorPA, 'Passive Localizations')
mtext('e', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1,cex = 2.5)
dev.off()


##Figure 6
tiff(filename='Figure 6.tiff', res=600, width=12, height=4, units='in', compression='lzw')
Plotschedule(active_reaches)
dev.off()
 