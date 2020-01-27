## 27 febrero 2018
##R --vanilla
require(raster)
require(vegan)
require(mgcv)
require(nlme)

source("~/Dropbox/Mapoteca/inc/inc00_funciones.R")
setwd("~/tmp/CEBA")

## datos de distribución
data.dir <- "~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos"
data.dir <- "~/Dropbox/ceba/data/720_SeriesTiempoPsitacidos"

## creo que estos son los datos de Jessica
rgs.Ab <- read.csv(sprintf("%s/rgs_abarbadensis.csv",data.dir),dec=".",sep=";")
rgs.Ab$lon <- -1*abs(rgs.Ab$lon)
rgs.Ab$fch <- rgs.Ab$fecha

## base de datos de Eliana, diciembre 2017
aves <- read.csv(sprintf("%s/DATOS_11_Diciembre2017II.csv",data.dir),dec=".",sep=",")
eli.Ab <- subset(aves,Genero %in% "Amazona" & Especie %in% "barbadensis")
eli.As <- subset(aves,Genero %in% "Amazona" & !(Especie %in% "barbadensis"))

eli.Ab$lon <- eli.Ab$X
eli.Ab$lat <- eli.Ab$Y
eli.Ab$fch <- eli.Ab$Ano
eli.Ab$fch[eli.Ab$Observador %in% "Steven Hilty"] <- NA

## datos NeoMapas,
(load("~/Dropbox/NeoMapas/Rdata/NMaves.rda"))

qry <- subset(spp.aves,Latname %in% "Amazona barbadensis")$N_ave
nms.Ab <- rbind(subset(NM.m1,Especieid %in% qry)[,c("lon","lat")],
                subset(NM.m2,Especieid %in% qry)[,c("lon","lat")])
nms.Ab$fch <- 2010
vzla <- shapefile("/opt/gisdata/vectorial/ecoSIG/division_pol.shp")

plot(vzla,ylim=c(10,13),col="grey53",border="grey87")

points(Y~X,eli.Ab)
points(lat~lon,nms.Ab,col=3)
points(lat~lon,rgs.Ab,col=2)
## solapamiento (punto en el Zulia, costa de Falcón y Anzoátegui)
## Ab en Macanao, A ochrocephala en el resto de la Isla 
##points(Y~X,eli.As,cex=2,pch=2) ##
## ¿y las otras especies de Amazona?

## A barbadensis parece estar ocupando las condiciones climáticas extremas donde otras especies de Amazona (concretamente A ochrocephala) no pueden sobrevivir.
## ¿que podemos decir de la alimentación?
## ¿interacciones importantes? El guayacán: importante para los nidos en Margarita, se solapan en su distribución; ¿otras especies?

Ab <- rbind(rgs.Ab[,c("lon","lat","fch")],
            eli.Ab[,c("lon","lat","fch")],
            nms.Ab[,c("lon","lat","fch")])

plot(vzla,ylim=c(10,13),col="grey53",border="grey87")

points(lat~lon,subset(Ab,!is.na(fch) & fch>1999))
points(lat~lon,subset(Ab,is.na(fch) | fch<2000),pch=4) ## overlap...





#########
## cargar funciones para calcular valores de Modis...
##########

archs <- dir("~/mapas/Venezuela/250m_16_days_NDVI","tif$",full.names=T)

mtz <- matrix(nrow=nrow(Ab),ncol=length(archs),dimnames=list(c(),sapply(archs,function(x) strsplit(x,"\\.")[[1]][2])))

for (aa in archs)
    mtz[,strsplit(aa,"\\.")[[1]][2]] <- as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",Ab$lon,Ab$lat),collapse="\n"),aa),intern=T))

##NDVI
Mdvi.mtz <- mSSt(mtz)


archs <- dir("~/mapas/Venezuela/LST_Day_1km/","tif$",full.names=T)

mtz <- matrix(nrow=nrow(Ab),ncol=length(archs),dimnames=list(c(),sapply(archs,function(x) strsplit(x,"\\.")[[1]][2])))

for (aa in archs)
    mtz[,strsplit(aa,"\\.")[[1]][2]] <- as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",Ab$lon,Ab$lat),collapse="\n"),aa),intern=T))

##LST d
Lstd.mtz <- mSSt(mtz,ll=7500,ul=32300,cf=0.02,os=-273.15)


archs <- dir("~/mapas/Venezuela/LST_Night_1km/","tif$",full.names=T)

mtz <- matrix(nrow=nrow(Ab),ncol=length(archs),dimnames=list(c(),sapply(archs,function(x) strsplit(x,"\\.")[[1]][2])))

for (aa in archs)
    mtz[,strsplit(aa,"\\.")[[1]][2]] <- as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",Ab$lon,Ab$lat),collapse="\n"),aa),intern=T))

##LST n
Lstn.mtz <- mSSt(mtz,ll=7500,ul=32300,cf=0.02,os=-273.15)

archs <- dir("~/mapas/Venezuela/PET_1km/","tif$",full.names=T)

mtz <- matrix(nrow=nrow(Ab),ncol=length(archs),dimnames=list(c(),sapply(archs,function(x) strsplit(x,"\\.")[[1]][2])))

for (aa in archs)
    mtz[,strsplit(aa,"\\.")[[1]][2]] <- as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",Ab$lon,Ab$lat),collapse="\n"),aa),intern=T))

##PET
PET.mtz <- mSSt(mtz,ll=-32767,ul=32760,cf=0.1,os=0,setNA=0)


archs <- dir("~/mapas/Venezuela/ET_1km/","tif$",full.names=T)

mtz <- matrix(nrow=nrow(Ab),ncol=length(archs),dimnames=list(c(),sapply(archs,function(x) strsplit(x,"\\.")[[1]][2])))

for (aa in archs)
    mtz[,strsplit(aa,"\\.")[[1]][2]] <- as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",Ab$lon,Ab$lat),collapse="\n"),aa),intern=T))

##ET
ET.mtz <- mSSt(mtz,ll=-32767,ul=32760,cf=0.1,os=0,setNA=0)



matplot(t(Mdvi.mtz),type="l")
matplot(t(Lstn.mtz),type="l")

matplot(t(PET.mtz),type="l",ylim=c(0,300),col="grey77")
matlines(t(ET.mtz),col="pink")



## registros climáticos históricos (resolución gruesa CRU)
mptc <- "/media/jferrer/Elements/gisdata"
rtmp <- stack(sprintf("%s/clima/CRU/cru_ts_3.22/data/tmp/cru_ts3.22.1901.2013.tmp.dat.nc",mptc))
rtmp <- crop(rtmp,extent(c(-74,-58,10, 13)))
rtmx <- stack(sprintf("%s/clima/CRU/cru_ts_3.22/data/tmx/cru_ts3.22.1901.2013.tmx.dat.nc",mptc))
rtmx <- crop(rtmx,extent(c(-74,-58,10, 13)))
rtmn <- stack(sprintf("%s/clima/CRU/cru_ts_3.22/data/tmn/cru_ts3.22.1901.2013.tmn.dat.nc",mptc))
rtmn <- crop(rtmn,extent(c(-74,-58,10, 13)))

rpre <- stack(sprintf("%s/clima/CRU/cru_ts_3.22/data/pre/cru_ts3.22.1901.2013.pre.dat.nc",mptc))
rpre <- crop(rpre,extent(c(-74,-58,10, 13)))

Ab$CRU.cld <- cellFromXY(rtmp,Ab[,c("lon","lat")])



## promedios climáticos recientes (WorldClim)
wpre <- stack(dir(sprintf("%s/clima/WC2.0/wc2.0_2.5m_prec",mptc),pattern=".tif$",full.names=T))
wpre <- crop(wpre,extent(c(-74,-58,10, 13)))

wtmp <- stack(dir(sprintf("%s/clima/WC2.0/wc2.0_2.5m_tavg",mptc),pattern=".tif$",full.names=T))
wtmp <- crop(wtmp,extent(c(-74,-58,10, 13)))

Ab$WC2.cld <- cellFromXY(wtmp,Ab[,c("lon","lat")])


## buscar tmin, tmean, tmax, prec

## registros climáticos satelitales (AVHRR, Modis, Chirps)
map.dir <- "~/mapas/Venezuela"
ndvis <- stack(dir(sprintf("%s/NDVI3g",map.dir),pattern=".tif$",full.names=T))
ndvis <- crop(ndvis,extent(c(-74,-58,10, 13)))
gc()

Ab$NDV.cld <- cellFromXY(ndvis,Ab[,c("lon","lat")])

chrps <- stack(dir(sprintf("%s/chirps",map.dir),pattern=".tif$",full.names=T))
chrps <- crop(chrps,extent(c(-74,-58,10, 13)))
gc()

Ab$CRP.cld <- cellFromXY(chrps,Ab[,c("lon","lat")])

chrp.mtz <- extract(chrps,Ab[,c("lon","lat")])



## Luego:
##¿han cambiado las condiciones ambientales históricas en las localidades donde actualmente hay registros de presencia?
##¿han cambiado las condiciones ambientales históricas en las localidades donde hubo registros de presencia históricos y actualmente está (aparentemente) ausente?


## Datos recientes de Anzoategui --- consultar Tony Crease and Andrew Spencer / Ebird
subset(eli.Ab,Estado %in% "Anzoategui")
## Datos recientes de Zulia --- Hilty (realmente viene de Literatura, sin fecha precisa) y observación de "Yores" en Hacienda los Aceitunos (¿en libertad o cautivo?)
subset(eli.Ab,Estado %in% "Zulia")

## Datos recientes de Falcon --
subset(eli.Ab,Estado %in% "Falcon" & Observador %in% "")
subset(eli.Ab,Estado %in% "Falcon" & Observador %in% c("John Drummond","R a y", "Carpentier","Jhonathan Miranda","David Stejskal","Kvarnback","Howard Laidlaw" ))
subset(eli.Ab,Estado %in% "Falcon" & Observador %in% "Jessica Ortega")
subset(eli.Ab,Estado %in% "Falcon" & Observador %in% "Steven Hilty")
subset(eli.Ab,Estado %in% "Falcon" & Observador %in% "Chris Sharpe" )
subset(eli.Ab,Estado %in% "Falcon" & Observador %in% "Hoffman Dos Santos")

## Datos recientes de Lara --
subset(eli.Ab,Estado %in% "Lara" & Observador %in% "")
subset(eli.Ab,Estado %in% "Lara" & Observador %in% c("Marcial Quiroga" ))
subset(eli.Ab,Estado %in% "Lara" & Observador %in% "Jessica Ortega")
subset(eli.Ab,Estado %in% "Lara" & Observador %in% "Steven Hilty")

## Datos recientes de Nueva Esparta --
subset(eli.Ab,Estado %in% "Nueva Esparta" & Observador %in% "")
subset(eli.Ab,Estado %in% "Nueva Esparta" & Observador %in% c("Platt","Dennis","Rodriguez"))
subset(eli.Ab,Estado %in% "Nueva Esparta" & Observador %in% "Franklin Rojas Suarez")


subset(eli.Ab,Estado %in% "Dependencias Federales")





table(Ab$CRU.cld,floor(Ab$fch/10))
table(Ab$WC2.cld,floor(Ab$fch/10))
table(Ab$NDV.cld,floor(Ab$fch/10))

CRU.dts <- with(Ab,aggregate(data.frame(fecha=fch),list(celda=CRU.cld),max,na.rm=T))
WC2.dts <- with(Ab,aggregate(data.frame(fecha=fch),list(celda=WC2.cld),max,na.rm=T))
NDV.dts <- with(Ab,aggregate(data.frame(fecha=fch),list(celda=NDV.cld),max,na.rm=T))
CRP.dts <- with(Ab,aggregate(data.frame(fecha=fch),list(celda=CRP.cld),max,na.rm=T))
MDV.dts <- with(Ab,aggregate(data.frame(fecha=fch),list(celda=MDV.cld),max,na.rm=T))

plot(vzla,ylim=c(10,13),col="grey53",border="grey87")
points(lat~lon,Ab,cex=.5,col="pink")

text(xyFromCell(rtmp,CRU.dts$celda)[,1],xyFromCell(rtmp,CRU.dts$celda)[,2],CRU.dts$fecha,cex=.6)

matplot(t(values(rtmp)[CRU.dts$celda,]),type="l")
matplot(t(values(ndvis)[NDV.dts$celda,]),type="l")
matplot(t(values(chrps)[CRP.dts$celda,]),type="l")

plot(CRU.dts$fecha,rowMeans(values(rtmp)[CRU.dts$celda,],na.rm=T))
plot(WC2.dts$fecha,rowMeans(values(wtmp)[WC2.dts$celda,],na.rm=T))
plot(NDV.dts$fecha,rowMeans(values(ndvis)[NDV.dts$celda,],na.rm=T))
plot(CRP.dts$fecha,rowMeans(values(chrps)[CRP.dts$celda,],na.rm=T))



plot(1:12,type="n",ylim=c(-50,50))
        mi.t1 <- ts(ET.mtz[j,], start = c(2000, 1), end = c(2014, 12), frequency = 12)
        mi.t2 <- ts(PET.mtz[j,], start = c(2000, 1), end = c(2014, 12), frequency = 12)
        ## vacios en 2001 y 2010... faltan capas?
        ##mi.t3 <- ts(Lstd.mtz[j,], start = c(2000, 9), end = c(2016, 19), frequency = 46)
        ##mi.t4 <- ts(Lstn.mtz[j,], start = c(2000, 9), end = c(2016, 19), frequency = 46)
        lines(decompose(mi.t1)$seasonal[1:12])
        lines(decompose(mi.t2)$seasonal[1:12],col=2)


nwdt <- data.frame(year=2000,doy=seq(1,365,by=31),mes=1:12)
rslts <- data.frame()

        layout(matrix(1:8,ncol=2))

##Time series
for (j in 1:nrow(PET.mtz)) {
    if (!j %in% rslts$j) {
        dtt <- data.frame(ET= ET.mtz[j,],PET= PET.mtz[j,],
                          mes= unname(as.numeric(substr(colnames(PET.mtz),7,8))),
                          year= unname(as.numeric(substr(colnames(PET.mtz),2,5))))
        dtt$TOS <- seq(along=dtt$year)

        dts <- data.frame(lstd= Lstd.mtz[j,],lstn= Lstn.mtz[j,],
                          doy= unname(as.numeric(substr(colnames(Lstn.mtz),6,8))),
                          year= unname(as.numeric(substr(colnames(Lstn.mtz),2,5))))
        dts$TOS <- seq(along=dts$year)

        dms <- data.frame(ndvi= Mdvi.mtz[j,],
                           doy= unname(as.numeric(substr(colnames(Mdvi.mtz),6,8))),
                           year= unname(as.numeric(substr(colnames(Mdvi.mtz),2,5))))
        dms$TOS <- seq(along=dms$year)

        dps <- data.frame(prec= chrp.mtz[j,],
                           mes= unname(as.numeric(substr(colnames(chrp.mtz),18,20))),
                           year= unname(as.numeric(substr(colnames(chrp.mtz),13,16))))
        dps$TOS <- seq(along=dps$year)

        
        ## code inspired by
        ##https://stackoverflow.com/questions/12623027/how-to-analyse-irregular-time-series-in-r
        ##https://www.fromthebottomoftheheap.net/2011/07/21/smoothing-temporally-correlated-data/
        ##https://www.fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
        if (mean(is.na(dtt$ET))<.75) {
            mod.ET <- gamm(ET ~ s(mes, bs = "cc") + s(year),
                           data = subset(dtt,!is.na(ET)),
                           correlation = corCAR1(form = ~ TOS))
            plot(mod.ET$gam)
            r.et <- t(predict(mod.ET$gam,
                               newdata=nwdt))
        } else {
            r.et <- t(rep(NA,12))
        }
        
        if (mean(is.na(dtt$PET))<.75) {
            
            mod.PET <- gamm(PET ~ s(mes, bs = "cc") + s(year),
                            data = subset(dtt,!is.na(PET)),
                            correlation = corCAR1(form = ~ TOS))
            plot(mod.PET$gam)
            r.pet <- t(predict(mod.PET$gam,
                                newdata=nwdt))
        } else {
            r.pet <- t(rep(NA,12))
        }
        if (mean(is.na(dps$prec))<.75) {
            mod.prec <- gamm(prec ~ s(mes, bs = "cc") + s(year),
                             data = subset(dps,!is.na(prec)),
                             correlation = corCAR1(form = ~ TOS))
            plot(mod.prec$gam)
            r.prec <- t(predict(mod.prec$gam,
                                newdata=nwdt))
        } else {
            r.prec <- t(rep(NA,12))
        }
        
        if (mean(is.na(dts$lstd))<.75) {

            
            mod.lstd <- gamm(lstd ~ s(doy, bs = "cc") + s(year),
                             data = subset(dts,!is.na(lstd)),
                             correlation = corCAR1(form = ~ TOS))
            plot(mod.lstd$gam)
            r.lstd <- t(predict(mod.lstd$gam,
                                newdata=nwdt))
        } else {
            r.lstd <- t(rep(NA,12))
        }
        
        if (mean(is.na(dts$lstn))<.75) {
        ##if (!all(is.na(dts$lstn))) {
            
            mod.lstn <- gamm(lstn ~ s(doy, bs = "cc") + s(year),
                             data = subset(dts,!is.na(lstn)),
                         correlation = corCAR1(form = ~ TOS))
            plot(mod.lstn$gam)
            r.lstn <- t(predict(mod.lstn$gam,
                                newdata=nwdt))
        } else {
            r.lstn <- t(rep(NA,12))
        }
        
        if (mean(is.na(dms$ndvi))<.75) {
            mod.ndvi <- gamm(ndvi ~ s(doy, bs = "cc") + s(year),
                             data = subset(dms,!is.na(ndvi)),
                             correlation = corCAR1(form = ~ TOS))
            plot(mod.ndvi$gam)
            r.ndvi <- t(predict(mod.ndvi$gam,
                                newdata=nwdt))
        } else {
            r.ndvi <- t(rep(NA,12))
        }

    
        rslts <- rbind(rslts,
                       data.frame(j,
                                  LSTd=r.lstd,
                                  LSTn=r.lstn,
                                  ET=r.et,
                                  PET=r.pet,
                                  NDVI=r.ndvi,
                                  PREC=r.prec
                                  ))

        if (nrow(rslts)>3) {
            plot(diana(daisy(rslts[,-1])))
            plot(vzla,ylim=c(10,13),col="grey53",border="grey87")
            Ab[rslts$j,"grp"] <- fanny(rslts[,-1],k=8)$cluster
            points(lat~lon,col=grp,data=Ab)

        }
    }
}

d0 <- diana(daisy(rslts[,-1]))
plot(d0,which.plots=2)

Ab[rslts$j,"grp"] <- cutree(d0,k=8)
plot(vzla,ylim=c(10,13),col="grey53",border="grey87")
points(lat~lon,col=grp,data=Ab)

##h0 <- as.hclust(d0)
plot(d0,which.plots=2,col=cutree(d0,k=8))

layout(matrix(1:(4*8),ncol=8,nrow=4,byrow=F))
for (gg in 1:8) {#grupos
    tt <- aggregate(t(Lstd.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(Lstd.mtz),6,8)),median,na.rm=T)
    matplot(tt[,1],tt[,-1],type="l",ylab="Temperature [°C]",ylim=c(0,50))
    tt <- aggregate(t(Lstn.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(Lstn.mtz),6,8)),median,na.rm=T)
    matlines(tt[,1],tt[,-1],col="pink")

    tt <- aggregate(t(chrp.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(chrp.mtz),18,22)),median,na.rm=T)
            matplot(tt[,1],tt[,-1],type="l",ylab="Precipitation [mm]",ylim=c(0,300))


    tt <- aggregate(t(PET.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(PET.mtz),7,8)),median,na.rm=T)
    matplot(tt[,1],tt[,-1],type="l",ylab="ET",ylim=c(0,300))
    tt <- aggregate(t(ET.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(ET.mtz),7,8)),median,na.rm=T)
   
    matlines(tt[,1],tt[,-1],col="pink")

    tt <- aggregate(t(Mdvi.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(Mdvi.mtz),6,8)),median,na.rm=T)
    matplot(tt[,1],tt[,-1],type="l",ylab="NDVI",ylim=c(0,1))
    
    
}

## see differences in group 1 (bimodal precipitation) and 5 (unimodal precipitation


## este le da más peso al NDVI, menos a la prec
d0 <- diana(daisy(rslts[,c(grep("NDVI",colnames(rslts)),grep("prec",colnames(rslts)))]))
layout(1)
plot(d0,which.plots=2)

mik <- 9

Ab[rslts$j,"grp"] <- cutree(d0,k=mik)
plot(vzla,ylim=c(10,13),col="grey53",border="grey87")
points(lat~lon,col=grp,data=Ab)


layout(matrix(1:(4*mik),ncol=mik,nrow=4,byrow=F))
for (gg in 1:mik) {#grupos
    tt <- aggregate(t(Lstd.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(Lstd.mtz),6,8)),median,na.rm=T)
    matplot(tt[,1],tt[,-1],type="l",ylab="Temperature [°C]",ylim=c(0,50))
    tt <- aggregate(t(Lstn.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(Lstn.mtz),6,8)),median,na.rm=T)
    matlines(tt[,1],tt[,-1],col="pink")

    tt <- aggregate(t(chrp.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(chrp.mtz),18,22)),median,na.rm=T)
            matplot(tt[,1],tt[,-1],type="l",ylab="Precipitation [mm]",ylim=c(0,300))


    tt <- aggregate(t(PET.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(PET.mtz),7,8)),median,na.rm=T)
    matplot(tt[,1],tt[,-1],type="l",ylab="ET",ylim=c(0,300))
    tt <- aggregate(t(ET.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(ET.mtz),7,8)),median,na.rm=T)
   
    matlines(tt[,1],tt[,-1],col="pink")

    tt <- aggregate(t(Mdvi.mtz[Ab$grp %in% gg,,drop=F]),
                    by=list(month=substr(colnames(Mdvi.mtz),6,8)),median,na.rm=T)
    matplot(tt[,1],tt[,-1],type="l",ylab="NDVI",ylim=c(0,1))
    
    
}

x11()
plot(vzla,ylim=c(10,13),col="grey53",border="grey87")
points(lat~lon,col=grp,data=Ab)

## separar localidades según evidencia de: anidamiento, alimentación, dormideros y avistamientos genéricos
## considerar si hay solapamiento entre los diferentes usos
## ver si las fechas de época reproductiva coinciden con pico en condiciones vegetación/clima



## too big...
##dts <- values(Mdvis)[MDV.dts$celda,]


dts <- values(ndvis)[NDV.dts$celda,]
dts <- values(chrps)[CRP.dts$celda,]
dts <- Mdvi.mtz
ss <- rowSums(is.na(dts))==0
pca1 <- rda(dts[ss,])
mtz <- vegdist(dts[ss,],"gower")
h0 <- hclust(mtz)
g0 <- cutree(h0,h=0.25) ## para NDVI
g0 <- cutree(h0,h=0.35) ## para CHIRPS
g0 <- cutree(h0,h=0.3) ## para Modis NDVI

plot(h0)

## NDVI -> interesante separación de Margarita y Blanquilla (menos vegetación) del resto (más vegetación)
## Chirps data -> divisiones claras costas secas, vs costa humeda e interior

layout(matrix(c(1,1,3,
                1,1,5,
                6,4,2),ncol=3,byrow=T))
plot(vzla,ylim=c(10,13),col="grey53",border="grey87")
for (k in 1:5)
points(lat~lon,subset(Ab,CRP.cld %in% CRP.dts$celda[g0 %in% k]),
       cex=1.75,col=k,pch=19)

for (k in 1:5) 
    matplot(t(dts[ss,][g0 %in% k,]),type="l",col=k,ylim=c(0,1))


text(xyFromCell(rtmp,CRU.dts$celda)[,1],xyFromCell(rtmp,CRU.dts$celda)[,2],CRU.dts$fecha,cex=.6)

matplot(t(dts[ss,][g0 %in% 1,]),type="l")


rowMeans(values(rtmp)[dts$celda,],na.rm=T)
         boxplot(,na.rm=T))



## predicción de cambio climático


## y estos?

xys <- aves@coords[amz.bar,]
colnames(xys) <- c("lon","lat")
xys <- unique(rbind(xys,
                    rgs.Ab[,c("lon","lat")]))


evs <- extract(WC,xys)
nds <- extract(ndvis,xys)

require(Hmisc)

v <- varclus(evs)
plot(v)

slc <- c("bio15","bio07","bio03","bio05","bio01","bio13","bio12")
D2.o <- mahalanobis(evs[,slc],center=colMeans(evs[,slc]),cov=cov(evs[,slc]))
##1-pchisq(D2,length(slc)-1)

D2 <- mahalanobis(values(WC)[,slc],center=colMeans(evs[,slc]),cov=cov(evs[,slc]))
psiAb <- raster(WC,1)
values(psiAb) <- D2< max(D2.o)
##values(psiAb) <- 1-pchisq(D2,length(slc)-1)

plot(psiAb)

rm(D2.M)

for (prd in 1981:2010) {
    kk <- grep(sprintf("X%s",prd),names(bio15))
    sbs <- stack(raster(bio15,kk),
                 raster(bio07,kk),
                 raster(bio03,kk),
                 raster(bio05,kk),
                 raster(bio01,kk),
                 raster(bio13,kk),
                 raster(bio14,kk))
    xbs <- extract(sbs,xys)
    D3 <- mahalanobis(values(sbs),
                      center=colMeans(xbs,na.rm=T),cov=cov(xbs,use="complete"))
    pp <- raster(sbs,1)
    values(pp) <- 1-pchisq(D3,length(slc)-1)
    if (exists("D2.M")) {
        D2.M <- addLayer(D2.M,pp)
    } else {
        D2.M <- pp
    }
}
psiAb2 <- raster(D2.M,1)
values(psiAb2) <- rowMeans(values(D2.M)>0.05)
psiAb2 <- resample(disaggregate(psiAb2,10),psiAb)



colnames(nds) <- sub(".Venezuela_NDVI","",sub("AVHRRVI3g.","",names(ndvis)))

v2 <- varclus(nds[,13:36])
plot(v2)
abline(h=.5)

ylc <- c("A19820615","A19821115","A19820315")

D2.n <- mahalanobis(nds[,ylc],center=colMeans(nds[,ylc]),cov=cov(nds[,ylc]))

rm(D2.V)
##xt <- values(ndvis)
for (prd in 1981:2010) {
    kk <-  sprintf("AVHRRVI3g.%s.Venezuela_NDVI",sub("1982",prd,ylc))
    if (all(kk %in%  names(ndvis))) {
        D3 <- mahalanobis(xt[,kk],
                  center=colMeans(nds[,ylc]),cov=cov(nds[,ylc]))
        pp <- raster(ndvis,1)
        values(pp) <- 1-pchisq(D3,length(slc)-1)
        if (exists("D2.V")) {
            D2.V <- addLayer(D2.V,pp)
        } else {
            D2.V <- pp
        }
    }
}
psiAb3 <- raster(D2.V,1)
values(psiAb3) <- rowMeans(values(D2.V)>0.75)
psiAb3 <- resample(disaggregate(psiAb3,10),psiAb)

plot(psiAb+(psiAb3>.5))

png(file="Amazona_barbadensis_tiempo.png",width=800,height=800,pointsize=20)
layout(matrix(1:2,ncol=1))
par(mar=c(1,1,4,1))
plot(psiAb*(psiAb+psiAb2)/2,col=brewer.pal(5,"YlOrBr"),axes=F,
     ylim=c(7,13),xlim=c(-73,-61),legend=T,main="Clima")
plot(vz0,add=T,border="maroon")
points(xys,pch=3,cex=.5)

plot(psiAb*(psiAb+(psiAb3>.5))/2,col=brewer.pal(5,"YlOrBr"),axes=F,
    ylim=c(7,13),xlim=c(-73,-61),legend=T,main="Vegetación")
plot(vz0,add=T,border="maroon")
points(xys,pch=3,cex=.5)
dev.off()


plot(lat~lon,rgs.Ab,col=(fecha %in% 2012) +1)
table(rgs.Ab$fecha)


##datos ebird y otras fuentes
