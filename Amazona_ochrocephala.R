amz.och <- grepl("Amazona ochr",aves@data$species)
amz.bod <- grepl("Amazona fest",aves@data$species)

ss <- !is.na(yr) & !is.na(mes) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(qrv) & !is.na(avdat$ndvi) & !is.na(avdat$pre) & !is.na(avdat$wpre) & !is.na(avdat$pendiente) & !is.na(avdat$arcilla) & !duplicated(data.frame(qrw,qrx,qry,qrz,qrv,amz.och,mes,col,yr,nyr))

y <- amz.och[ss]

sl1 <- !is.na(aves@data$year) & aves@data$year<1950
sl2 <- !is.na(aves@data$year) & aves@data$year> 1950 & aves@data$year < 1980  
sl3 <- !is.na(aves@data$year) & aves@data$year> 1980  

png(file="Amazona_ochrocephala_hist.png",width=1000,height=380,pointsize=28)
layout(matrix(1:3,ncol=3))
par(mar=c(1,1,2,1),cex.main=2)
plot(vz0,ylim=c(0,13),col="grey33",border="grey77",main="< 1950")
points(aves[sl1,],pch=3,cex=.37,col="yellow")
points(aves[sl1 & amz.och,],col=2,cex=.85)

plot(vz0,ylim=c(0,13),col="grey33",border="grey77",main="1950-1980")
points(aves[sl2,],pch=3,cex=.37,col="yellow")
points(aves[sl2 & amz.och,],col=2,cex=.85)

plot(vz0,ylim=c(0,13),col="grey33",border="grey77",main=">1980")
points(aves[sl3,],pch=3,cex=.37,col="yellow")
points(aves[sl3 & amz.och,],col=2,cex=.85)
dev.off()

shp.ao <- shapefile("~/gisdata/distribuciones/BirdLife/Amazona_ochrocephala_22686346.shp")
shp.ab <- shapefile("~/gisdata/distribuciones/BirdLife/Amazona_bodini_22727636.shp")

png(file="Amazona_bodini_hist.png",width=800,height=440,pointsize=18)
layout(matrix(1:2,ncol=2))
par(mar=c(1,1,2,1),cex.main=2,font.main=3)

plot(vz0,ylim=c(0,13),border="grey33",main="Amazona bodini")
plot(shp.ab,col=rgb(.1,.1,.9,.7),add=T)
points(aves[amz.bod,],col=2,cex=.85)

plot(vz0,ylim=c(0,13),border="grey33",main="Amazona ochrocephala")
plot(shp.ao,col=rgb(.1,.1,.9,.7),add=T)
points(aves[amz.och,],col=2,cex=.85)
dev.off()


## hipótesis
## variabilidad ambiental representada por promedios multianuales:
## tres var. principales: temp media anual, estacionalidad e isotermalidad
W1 <- paste(sprintf("%1$s + I(%1$s^2)",c("wbio01","wbio15","wbio03")),
            collapse=" + ")
## además: intervalo diurno, intervalo anual, prec trim. seco, prec anual 
W2 <- paste(sprintf("%1$s + I(%1$s^2)",c("wbio02","wbio07","wbio17","wbio12")),
            collapse=" + ")

## variabilidad ambiental representada por valores anuales:
## tres var. principales: temp media anual, estacionalidad e intervalo anual
M1 <- paste(sprintf("%1$s + I(%1$s^2)",c("mbio01","mbio15","mbio07")),
            collapse=" + ")
## además: 
M2 <- paste(sprintf("%1$s + I(%1$s^2)",c("mbio02","mbio18","mbio17","mbio12")),
            collapse=" + ")

## variabilidad ambiental representada por valores mensuales:
## temp y prec corregidos
T1 <- paste(sprintf("%1$s + I(%1$s^2)",c("ctmp","cpre")),
            collapse=" + ")
## temp y prec mensuales (promedio de varios años)
T2 <- paste(sprintf("%1$s + I(%1$s^2)",c("wtmp","wpre")),
            collapse=" + ")

##Tendencia anual lineal ## la puedo usar pero prob. indica dif. muestreo
Y <- paste(sprintf("%1$s ","year"),collapse=" + ")
##estacionalidad 
S <- paste(sprintf("%1$s ",c("cphi","sphi")),collapse=" + ")
##anomalias tmp y pre
D <- paste(sprintf("%1$s ",c("dtmp", "dpre")),collapse=" + ")

##NDVI
V <- paste(sprintf("%1$s + I(%1$s^2)",c("ndvi")),collapse=" + ")


fm1 <- expand.grid(c(T),c(T,F),c(T,F),c(T,F),c(T,F))
colnames(fm1) <- c("W1","M2","S","D","V")
##fm1 <- subset(fm1,!(D & Y) & !(D & S))
fm1 <- subset(fm1,!(D & S))

fm2 <- expand.grid(c(T),c(T,F),c(T,F),c(T,F),c(T,F))
colnames(fm2) <- c("M1","W2","S","D","V")
fm2 <- subset(fm2,!(D & S))

fm3 <- expand.grid(c(T))
colnames(fm3) <- c("T1")
fm4 <- expand.grid(c(T),c(T,F))
colnames(fm4) <- c("T2","D")



modnms <- c()
for (j in 1:4) {
    fms <- get(paste("fm",j,sep=""))
    for (k in 1:nrow(fms)) {
        nms <- sprintf("lm.%s",paste(colnames(fms)[unlist(fms[k,])],collapse=""))
        assign(nms,
               glm(as.formula(sprintf("y ~  %s",paste(unlist(mget(colnames(fms)[unlist(fms[k,])])),collapse="+"))),
                   avdat[ss,],family=binomial))
        modnms <- c(modnms,nms)
    }
}




AICt <- aictab(mget(modnms),modnms)

 modavg(mget(modnms), "wbio01", modnames = modnms,exclude="wbio01^2")

###
# Mejor modelo

lm.W1M2D

###
# predicción

## promedio mensual
r01 <- raster(WC,1)
r02 <- raster(WC,3)
r03 <- raster(WC,15)

vrs <- stack(r01,r02,r03)
rvz <- rasterize(vz0,vrs)>0
vrs <- vrs*rvz

names(vrs) <- c("wbio01","wbio03","wbio15")
mpsi <- r01
values(mpsi) <- 0
r00 <- mpsi
mpsi.01 <- mpsi.02 <- mpsi.03 <- mpsi.04 <- mpsi.05 <- mpsi.06 <- mpsi.07 <- 
mpsi.08 <- mpsi.09 <- mpsi.10 <- mpsi.11 <- mpsi.12 <- mpsi
j <- 0
cop <- 0.78##median(aggregate(predict(lm3,type="response"),list(y),median)$x)
prd <- 1940:2010
prd <- 1981:2010

rm(spsis)
##whr <- locator()
whr <- data.frame(x=c(-70.12681, -68.11605, -66.13825, -64.72083, -64.35823,
                      -63.86378),
                  y=c(7.640884,  8.525332,  9.114964,  9.639081,  9.704596,
                      10.130441))
whr.psi <- data.frame()
for (k in prd) {
    cc <- seq(along=names(bio02))[as.numeric(substr(names(bio02),2,5)) %in% k]
    ss2 <- (substr(yr,2,5) %in% k)
    mi.y <- amz.och[ss & !ss2] 
    tu.y <- amz.och[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ mbio02 + I(mbio02^2) + mbio18 + I(mbio18^2) + mbio17 + I(mbio17^2) +mbio12 + I(mbio12^2) + dtmp + dpre,avdat[ss & !ss2,],family=binomial)

    
    if (k %in% 1950) {
        mpsi.t1 <- mpsi/j
        mpsi <- r00
        values(mpsi) <- 0
        j <- 0
    }
    if (k %in% 1980) {
        mpsi.t2 <- mpsi/j
        mpsi <- r00
        values(mpsi) <- 0
        j <- 0
    }
    r04 <- raster(bio02,cc)
    r05 <- raster(bio12,cc)
    r06 <- raster(bio17,cc)
    r07 <- raster(bio18,cc)
    vrt <- vrs
    for (x in 4:7) {
        rr <- sprintf("r%02d",x)
        dd <- disaggregate(get(rr),round(res(get(rr))[1]/res(r01)[1]))
        vrt <- addLayer(vrt,resample(dd,r01))    
    }
    for (m in 1:12) {
        rr <- raster(rdtmp,grep(k,names(rdtmp))[m])
        dd <- disaggregate(rr,round(res(rr)[1]/res(r01)[1]))
        frt <- addLayer(vrt,resample(dd,r01))    
        rr <- raster(rdpre,grep(k,names(rdpre))[m])
        dd <- disaggregate(rr,round(res(rr)[1]/res(r01)[1]))
        frt <- addLayer(frt,resample(dd,r01))    
 
        psi <- raster(frt,1)
        nwdt <- data.frame(values(frt))
        colnames(nwdt) <- c(names(vrs),"mbio02","mbio12","mbio17","mbio18",
                            "dtmp","dpre")
        
        values(psi) <- boot::inv.logit(rowSums(predict(lm00,nwdt,type="terms"))) 
        whr.psi <- rbind(whr.psi,
                         data.frame(k=1:nrow(whr),
                                    fch=k+((m-1)/12),
                                    psi=extract(psi,whr)))

        if (j %% 7 == 0) {
            if (!exists("spsis")) {
                spsis <- stack(psi)
            } else {
                spsis <- addLayer(spsis,psi)
            }
            names(spsis)[nlayers(spsis)] <- sprintf("A%sM%s",k,m)

        }
        
        x01 <- extract(psi,aves[ss & ss2,])
        if (sum(tu.y)>0) {
            ev1 <- evaluate(x01[tu.y],x01[!tu.y])
            ##  boxplot(ev1)
            
            cop <- threshold(ev1,"no_omission")
        } 
        assign(sprintf("mpsi.%02d",m),get(sprintf("mpsi.%02d",m))+(psi>cop))
        mpsi <- mpsi+(psi>cop)
        j <- j+1
        plot(mpsi/j)
        points(aves[amz.och,])
        title(main=k,sub=m)
    }
}
mpsi.t3 <- mpsi/j

mpsi.01 <- mpsi.01/length(prd)
mpsi.02 <- mpsi.02/length(prd)
mpsi.03 <- mpsi.03/length(prd)
mpsi.04 <- mpsi.04/length(prd)
mpsi.05 <- mpsi.05/length(prd)
mpsi.06 <- mpsi.06/length(prd)
mpsi.07 <- mpsi.07/length(prd)
mpsi.08 <- mpsi.08/length(prd)
mpsi.09 <- mpsi.09/length(prd)
mpsi.10 <- mpsi.10/length(prd)
mpsi.11 <- mpsi.11/length(prd)
mpsi.12 <- mpsi.12/length(prd)

plot(mpsi.t3)


### ahora con vegetacion
lm.W1M2DV

## promedio mensual
r01 <- raster(WC,1)
r02 <- raster(WC,3)
r03 <- raster(WC,15)

vrs <- stack(r01,r02,r03)
rvz <- rasterize(vz0,vrs)>0
vrs <- vrs*rvz

names(vrs) <- c("wbio01","wbio03","wbio15")
mpdi <- r01
values(mpdi) <- 0
r00 <- mpdi
mpdi.01 <- mpdi.02 <- mpdi.03 <- mpdi.04 <- mpdi.05 <- mpdi.06 <- mpdi.07 <- 
mpdi.08 <- mpdi.09 <- mpdi.10 <- mpdi.11 <- mpdi.12 <- mpdi
j <- 0
cop <- 0.78##median(aggregate(predict(lm3,type="response"),list(y),median)$x)
prd <- 1940:2010
prd <- 1981:2010

rm(spdis)
##whr <- locator()
whr <- data.frame(x=c(-70.12681, -68.11605, -66.13825, -64.72083, -64.35823,
                      -63.86378),
                  y=c(7.640884,  8.525332,  9.114964,  9.639081,  9.704596,
                      10.130441))
whr.pdi <- data.frame()
for (k in prd) {
    cc <- seq(along=names(bio02))[as.numeric(substr(names(bio02),2,5)) %in% k]
    ss2 <- (substr(yr,2,5) %in% k)
    mi.y <- amz.och[ss & !ss2] 
    tu.y <- amz.och[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ mbio02 + I(mbio02^2) + mbio18 + I(mbio18^2) + mbio17 + I(mbio17^2) +mbio12 + I(mbio12^2) + dtmp + dpre +  ndvi + I(ndvi^2),avdat[ss & !ss2,],family=binomial)

   
    if (k %in% 1950) {
        mpdi.t1 <- mpdi/j
        mpdi <- r00
        values(mpdi) <- 0
        j <- 0
    }
    if (k %in% 1980) {
        mpdi.t2 <- mpdi/j
        mpdi <- r00
        values(mpdi) <- 0
        j <- 0
    }
    r04 <- raster(bio02,cc)
    r05 <- raster(bio12,cc)
    r06 <- raster(bio17,cc)
    r07 <- raster(bio18,cc)
    vrt <- vrs
    for (x in 4:7) {
        rr <- sprintf("r%02d",x)
        dd <- disaggregate(get(rr),round(res(get(rr))[1]/res(r01)[1]))
        vrt <- addLayer(vrt,resample(dd,r01))    
    }
    for (m in 1:12) {
        rr <- raster(rdtmp,grep(k,names(rdtmp))[m])
        dd <- disaggregate(rr,round(res(rr)[1]/res(r01)[1]))
        frt <- addLayer(vrt,resample(dd,r01))    
        rr <- raster(rdpre,grep(k,names(rdpre))[m])
        dd <- disaggregate(rr,round(res(rr)[1]/res(r01)[1]))
        frt <- addLayer(frt,resample(dd,r01))    

        if (!is.na(grep(sprintf("A%s%s",k,m),names(ndvis))[1])) {

            rr <- raster(ndvis,grep(sprintf("A%s%s",k,m),names(ndvis))[1])
            dd <- disaggregate(rr,round(res(rr)[1]/res(r01)[1]))
            frt <- addLayer(frt,resample(dd,r01))    
            
            pdi <- raster(frt,1)
            nwdt <- data.frame(values(frt))
            colnames(nwdt) <- c(names(vrs),"mbio02","mbio12","mbio17","mbio18",
                                "dtmp","dpre","ndvi")
            
            values(pdi) <- boot::inv.logit(rowSums(predict(lm00,nwdt,type="terms"))) 
            whr.pdi <- rbind(whr.pdi,
                         data.frame(k=1:nrow(whr),
                                    fch=k+((m-1)/12),
                                    pdi=extract(pdi,whr)))
            
            if (j %% 7 == 0) {
                if (!exists("spdis")) {
                    spdis <- stack(pdi)
                } else {
                    spdis <- addLayer(spdis,pdi)
                }
                names(spdis)[nlayers(spdis)] <- sprintf("A%sM%s",k,m)
                
            }
        
            x01 <- extract(pdi,aves[ss & ss2,])
            if (sum(tu.y)>0) {
                ev1 <- evaluate(x01[tu.y],x01[!tu.y])
                ##  boxplot(ev1)
                
                cop <- threshold(ev1,"no_omission")
            } 
            assign(sprintf("mpdi.%02d",m),get(sprintf("mpdi.%02d",m))+(pdi>cop))
            mpdi <- mpdi+(pdi>cop)
            j <- j+1
            plot(mpdi/j)
            points(aves[amz.och,])
            title(main=k,sub=m)
        }
    }
}
mpdi.t3 <- mpdi/j
lm00


plot(mpdi.t3*(1-mpsi.t3),col=brewer.pal(9,"PuBu"))
plotRGB(brick((1-mpsi.t3)*(1-mpdi.t3),mpsi.t3*mpdi.t3,mpdi.t3*(1-mpsi.t3)),scale=1)
plot((mpsi.t3-mpdi.t3)*((mpsi.t3+mpdi.t3)>1),col=brewer.pal(9,"PiYG"),breaks=round(seq(-.15,.15,length=10),2))



png(file="Amazona_ochrocephala_tiempo.png",width=800,height=700,pointsize=18)
plot(spsis,c(1,15,22,50),col=brewer.pal(9,"Greens"),breaks=round(seq(0,1,length=10),2))
dev.off()


png(file="Amazona_ochrocephala_hist.png",width=1000,height=400,pointsize=18)

layout(matrix(c(3,2,1,7,7,7,6,5,4),ncol=3,byrow=F))
clrs <- rev(brewer.pal(6,"Spectral"))

par(mar=c(3,3,1,1))
for(kk in 1:6) {
    if (kk %in% 4)
        par(mar=c(3,1,1,3))
    with(subset(whr.psi,k==kk),
         plot(fch,psi,ylim=c(0,1),col=clrs[kk],lwd=2,
              type="l",axes=F,xlab="",ylab="",bg="grey77"))
    if (kk %in% c(1,4))
        axis(1)
    if (kk %in% c(1,3))
        axis(2)
    if (kk %in% c(4,6))
        axis(4)
    abline(h=0.7,lty=3)
    box()
}
par(mar=c(2,2,0,0))
##plot((mpsi.t3+mpsi.t2+mpsi.t1)/3,breaks=c(0,.25,.5,.75,1),
plot(vz0)
image(mpsi.t3,add=T,
     ylim=c(0,13),axes=F,
     col=brewer.pal(9,"Spectral"))
##
points(whr,cex=2.4)
dev.off()


png(file="Amazona_ochrocephala_prd.png",width=700,height=550,pointsize=18)
plot(mpsi.t3,
     ylim=c(0,13),axes=F,
     col=brewer.pal(9,"Spectral"))
plot(vz0,add=T)
points(decimalLatitude~decimalLongitude,gb.Amazona.ochrocephala$data)
dev.off()

png(file="Amazona_ochrocephala_prdndvi.png",width=700,height=550,pointsize=18)
plot(mpdi.t3,
     ylim=c(0,13),axes=F,
     col=brewer.pal(9,"Spectral"))
plot(vz0,add=T)
points(decimalLatitude~decimalLongitude,gb.Amazona.ochrocephala$data)
dev.off()


png(file="SeriesTiempoCRU.png",width=500,height=400)
layout(matrix(1:2,ncol=1))
par(mar=c(5,4,1,1),oma=c(0,0,3,0))
plot(1901:2013,t(extract(bio01,data.frame(-68.95206,10.79257))),type="l",
     xlab="Año",ylab="Temperatura media anual [°C]")
plot(1901:2013,t(extract(bio12,data.frame(-68.95206,10.79257))),type="l",
     xlab="Año",ylab="Precipitación anual [mm]")
mtext("CRU TS 3.0",3,1,outer=T,cex=2)
dev.off()

ndvis <- stack(dir("~/mapas/Venezuela/NDVI3g/",full.names=T))
tt <- as.numeric(substr(names(ndvis),12,15)) + (cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))[as.numeric(substr(names(ndvis),16,17))] + as.numeric(substr(names(ndvis),18,19)))/365
qry <- t(extract(ndvis,data.frame(-68.95206,10.79257)))

png(file="SeriesTiempoNDVI.png",width=500,height=400)
par(mar=c(5,4,1,1),oma=c(0,0,3,0))
plot(tt,qry,type="l",
     xlab="Año",ylab="Indice de vegetación")
mtext("AVHRR GIMMS NDVI 3g",3,1,outer=T,cex=2)
dev.off()


mpsis <- stack(mpsi.01,mpsi.02,mpsi.03,mpsi.04,mpsi.05,mpsi.06,
               mpsi.07,mpsi.08,mpsi.09,mpsi.10,mpsi.11,mpsi.12)

mpsi.p <- mpsi.a <- mpsi.01
values(mpsi.p) <- apply(values(mpsis),1,max)
values(mpsi.a) <- apply(values(mpsis),1,min)
plot(mpsi.p-mpsi.a)
 


plot(mpsi.p,breaks=c(0,.25,.5,.75,1),
     ylim=c(0,13),
     col=brewer.pal(4,"Spectral"))
plot(mpsi.a,breaks=c(0,.25,.5,.75,1),
     ylim=c(0,13),
     col=brewer.pal(4,"Spectral"))


##tendencia en NDVI
ndvis.t[grep("\\.8356",as.character(ndvis.t))]
grep("\\.8356",as.character(ndvis.t))
plot(raster(ndvis,602)<raster(ndvis,33),col=brewer.pal(9,"PiYG"))
plot(raster(ndvis,578)<raster(ndvis,57),col=brewer.pal(9,"PiYG"))
