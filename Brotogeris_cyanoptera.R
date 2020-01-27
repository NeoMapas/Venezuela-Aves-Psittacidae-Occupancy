bro.cya <- grepl("Brotogeris cyan",aves@data$species)
shp.bc <- shapefile("~/gisdata/distribuciones/BirdLife/Brotogeris_cyanoptera_22685983.shp")

xys <- gb.Brotogeris.cyanoptera$data[,c("decimalLongitude","decimalLatitude")]
me.bc <- maxent(WC,xys)
pr.bc <- predict(me.bc,WC)


png(file="Brotogeris_cyanoptera_hist.png",width=900,height=440,pointsize=18)
layout(matrix(1:2,ncol=2))
par(mar=c(1,0,1,5),oma=c(0,0,3,0))
plot(vz0,ylim=c(0,13),border="darkgreen",main="IUCN, 2014")
plot(shp.bc,col=rgb(.1,.1,.9,.7),add=T)
points(xys,col=2,cex=.85)

plot(pr.bc,col=brewer.pal(9,"Greens"),
     breaks=round(seq(0,1,length=10),2),
     ylim=c(0,13),axes=F,main="GBIF + MaxEnt 3.3.3k")
plot(vz0,add=T)
points(xys,col=2,cex=.85)

mtext("Brotogeris cyanoptera",3,cex=2.7,outer=T,font=3)
dev.off()

png(file="Brotogeris_cyanoptera_hist.png",width=900,height=600,pointsize=14)
 plot(WC,col=brewer.pal(9,"RdYlGn"),nc=4,nr=3,maxnl=18)
dev.off()
