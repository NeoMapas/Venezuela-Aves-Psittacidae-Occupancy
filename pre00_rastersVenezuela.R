if (file.exists("/media/mapoteca/HD-E1")) {
    mptc <- "/media/mapoteca/HD-E1"
}
if (file.exists("/media/jferrer/HD-E1")) {
    mptc <- "/media/jferrer/HD-E1"
}

WC <- stack(dir("~/mapas/Venezuela/WC_2.5min/",pattern="bio",full.names=T))
    WC <- crop(WC,extent(c(-74,-58,0, 13)))
    wtmp <- stack(dir("~/mapas/Venezuela/WC_2.5min/",pattern="tmean",full.names=T))
    wtmp <- crop(wtmp,extent(c(-74,-58,0, 13)))
    wpre <- stack(dir("~/mapas/Venezuela/WC_2.5min/",pattern="prec",full.names=T))
    wpre <- crop(wpre,extent(c(-74,-58,0, 13)))
    ralt <- raster(dir("~/mapas/Venezuela/WC_2.5min/",pattern="alt",full.names=T))
    ralt <- crop(ralt,extent(c(-74,-58,0, 13)))
    
    pendiente <- terrain(ralt,"slope")
    suelo <- raster("~/mapas/Venezuela/HWSD_1s/hwsd.tif")
    txt <- read.table("~/suelos/HWSD/texturas.tab",header=T,sep="\t")
    vls <- txt[match(values(suelo),txt$ID),c("T_CLAY","T_SAND","T_SILT")]
    ##vls[,1] <- vls[,1]-0.00005
    ##vls[,2] <- vls[,2]-0.00005
    ##vls[,3] <- vls[,3]+0.0001
    vls <- alr(vls,2)
    arcilla <- suelo
    values(arcilla) <- NA
    values(arcilla) <- vls[,1]
    ##textura <- stack(arcilla,arena)
    ##names(textura) <- c("arcilla","arena")
    rtmp <- stack(sprintf("%s/Mapoteca/clima/CRU/cru_ts_3.22/data/tmp/cru_ts3.22.1901.2013.tmp.dat.nc",mptc))
    rtmp <- crop(rtmp,extent(c(-74,-58,0, 13)))
    rtmx <- stack(sprintf("%s/Mapoteca/clima/CRU/cru_ts_3.22/data/tmx/cru_ts3.22.1901.2013.tmx.dat.nc",mptc))
    rtmx <- crop(rtmx,extent(c(-74,-58,0, 13)))
    rtmn <- stack(sprintf("%s/Mapoteca/clima/CRU/cru_ts_3.22/data/tmp/cru_ts3.22.1901.2013.tmp.dat.nc",mptc))
    rtmn <- crop(rtmn,extent(c(-74,-58,0, 13)))
    
    rpre <- stack(sprintf("%s/Mapoteca/clima/CRU/cru_ts_3.22/data/pre/cru_ts3.22.1901.2013.pre.dat.nc",mptc))
    rpre <- crop(rpre,extent(c(-74,-58,0, 13)))

    ## periodo 1960 al 1990
    prd <- 60:90
    rdtmp <- rtmp
    values(rdtmp) <- NA
    rdpre <- rpre
    values(rdpre) <- NA

matplot(t(values(rtmp)[101:200,]),type="l")
matplot(log1p(t(values(rpre)[101:200,])),type="l")

    ## meses
    for (pat in sprintf("\\.%02d\\.",1:12)) {
        mu <- apply(values(rtmp)[,grep(pat,names(rtmp),value=T)[prd]],1,mean,na.rm=T)
        obs <- values(rtmp)[,grep(pat,names(rtmp),value=T)]
        delta <- obs-mu
        values(rdtmp)[,grep(pat,names(rtmp),value=T)] <- delta
        
        mu <- apply(values(rpre)[,grep(pat,names(rpre),value=T)[prd]],1,mean,na.rm=T)
        obs <- values(rpre)[,grep(pat,names(rpre),value=T)]
        delta <- obs-mu
        values(rdpre)[,grep(pat,names(rpre),value=T)] <- delta
    }

    bio01 <- bio02 <- bio03 <- bio04 <- bio05 <- bio06 <- bio07 <- bio08 <-
        bio09 <- bio10 <- bio11 <- bio12 <- bio13 <- bio14 <- bio15 <-
            bio16 <- bio17 <- bio18 <- bio19 <- stack()

    for (k in 1901:2013) {
        bv <- biovars(values(rpre)[,grep(k,names(  rtmx))],
                              values(rtmn)[,grep(k,names(  rtmx))],
                              values(rtmx)[,grep(k,names(  rtmx))])
        for (j in 1:19) {
            bb <- raster(rpre,1)
            values(bb) <- bv[,j]
            assign(sprintf("bio%02d",j),
                   addLayer(get(sprintf("bio%02d",j)),bb))
        }
    }
    names(bio01) <- names(bio02) <- names(bio03) <- names(bio04) <-
        names(bio05) <- names(bio06) <- names(bio07) <- names(bio08) <-
            names(bio09) <- names(bio10) <- names(bio11) <- names(bio12) <-
                names(bio13) <- names(bio14) <- names(bio15) <- names(bio16) <-
                    names(bio17) <- names(bio18) <- names(bio19) <-
                1901:2013


ndvis <- stack(dir("~/mapas/Venezuela/NDVI3g/",full.names=T))
ndvis.t <- as.numeric(substr(names(ndvis),12,15)) + (cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))[as.numeric(substr(names(ndvis),16,17))] + as.numeric(substr(names(ndvis),18,19)))/365
