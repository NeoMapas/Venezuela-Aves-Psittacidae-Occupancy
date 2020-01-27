source("~/Dropbox/Mapoteca/inc/inc00_funciones.R")

##head(substr(trans.info$Fecha.Muestreo.1,4,5))
##head(substr(trans.info$Fecha.Muestreo.1,1,2))

trans.info$doy <- cumsum(c(0,31,28,31))[as.numeric(substr(trans.info$Fecha.Muestreo.1,1,2))]+as.numeric(substr(trans.info$Fecha.Muestreo.1,4,5))


trans.info$fPET <- sprintf("A2010M%s",substr(trans.info$Fecha.Muestreo.1,1,2))
trans.info$fLST <- as.character(cut(trans.info$doy,
                                    breaks=c(seq(1,365,by=8),365),
                       labels=sprintf("A2010%03d",seq(1,365,by=8))))
trans.info$fEVI <- as.character(cut(trans.info$doy,
                                    breaks=c(seq(1,365,by=16),365),
                       labels=sprintf("A2010%03d",seq(1,365,by=16))))

for (k in unique(trans.info$fLST)) {
    slc <- subset(trans.info,fLST %in% k)$IDTransecta
    ss <- muestreos$NM %in% slc
    for (vv in c("LST_Day_1km","LST_Night_1km","Fpar_1km","Lai_1km")) {
        rLST <- raster(dir(sprintf("~/mapas/Venezuela/%s/",vv),
                           pattern=k,full.names=T))
        qqq <- extract(rLST,muestreos[ss,c("V2","V1")])
        
        muestreos[ss,vv] <- switch(vv,LST_Day_1km=,
                                   LST_Night_1km={
                                       mSSt(qqq,ll=7500,ul=NA,cf=0.02,os=-273.15)},
                                   Lai_1km={mSSt(qqq,ll=0,ul=100,cf=0.1,os=0)},
                                   Fpar_1km={mSSt(qqq,ll=0,ul=100,cf=0.01,os=0)})
        
    }
}


for (k in unique(trans.info$fPET)) {
    slc <- subset(trans.info,fPET %in% k)$IDTransecta
    ss <- muestreos$NM %in% slc
    for (vv in c("PET_1km","ET_1km")) {
        rLST <- raster(dir(sprintf("~/mapas/Venezuela/%s/",vv),
                           pattern=k,full.names=T))
        qqq <- extract(rLST,muestreos[ss,c("V2","V1")])
        
        muestreos[ss,vv] <- mSSt(qqq,ll=-32767,ul=32760,cf=0.1,os=0,setNA=0)
    }
}

for (k in unique(trans.info$fEVI)) {
    for (vv in c("250m_16_days_NDVI","250m_16_days_EVI")) {
        pv <- paste("v",vv,sep="")
        ss <- trans.info$fEVI %in% k 
        qqq <- extract(raster(dir(sprintf("~/mapas/Venezuela/%s/",vv),
                                  pattern=k,full.names=T)),
                       muestreos[ss,c("V2","V1")])
        muestreos[ss,pv] <- 
            mSSt(qqq,ll=-2000,ul=10000,cf=0.0001,os=0,setNA=0)
    }
}

require(randomForest)

na.fix <- rfImpute(muestreos[,c(1,2,6:13)],factor(muestreos$NM))
for (vv in c("LST_Day_1km","LST_Night_1km","PET_1km","ET_1km",
             "Lai_1km","Fpar_1km","v250m_16_days_NDVI","v250m_16_days_EVI")) 
    muestreos[is.na(muestreos[,vv]),vv] <- 
        na.fix[is.na(muestreos[,vv]),vv]

for (vv in c("LST_Day_1km","LST_Night_1km","PET_1km","ET_1km",
             "Lai_1km","Fpar_1km","v250m_16_days_NDVI","v250m_16_days_EVI")) {
    nv <- sub("Night","n",sub("Day","d",sub("days","",gsub("[_v0-9km]+","",vv))))
    mu <- mean(muestreos[,vv])
    sg <- sd(muestreos[,vv])
    muestreos[,nv] <- (muestreos[,vv]-mu)/sg

}


trans.info$fEVI

rTs <- stack()
for (k in c("A2010065","A2010073","A2010081")) {
    rLST <- raster(dir(sprintf("~/mapas/Venezuela/%s/","LST_Day_1km"),
                       pattern=k,full.names=T))
    values(rLST) <- mSSt(values(rLST),ll=7500,ul=NA,cf=0.02,os=-273.15)
    rTs <- addLayer(rTs,rLST)
}

rPET <- raster(dir(sprintf("~/mapas/Venezuela/%s/","PET_1km"),
                   pattern="A2010M03",full.names=T))
values(rPET) <- mSSt(values(rPET),ll=-32767,ul=32760,cf=0.1,os=0,setNA=0)


rEVI <- raster(dir(sprintf("~/mapas/Venezuela/%s/","250m_16_days_EVI"),
                   pattern="A2010065",full.names=T))

values(rEVI) <- mSSt(values(rEVI),ll=-2000,ul=10000,cf=0.0001,os=0,setNA=0)

rE1 <- aggregate(rEVI,4)

mVars <- stack(raster(rTs,3),rPET,resample(rE1,rPET))
names(mVars) <- c("LSTd","PET","EVI")
