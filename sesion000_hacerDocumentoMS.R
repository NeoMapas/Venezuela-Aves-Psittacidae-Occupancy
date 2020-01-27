##R --vanilla
setwd("~/tmp/CEBA")
require(raster)
require(maxlike)
require(compositions)
require(dismo)
require(Hmisc)
require(AICcmodavg)
source("~/Dropbox/ceba/doc/720_SerieTiempoPsittacidae/inc01_funciones.R")

mi.arch <- "Documento1_PsittacidaeVenezuelaNM"
mi.dir <- "720_SerieTiempoPsittacidae"
titulo <- "SupplementaryDocument_DetailedMethodsResults"
mi.path <- "~/Dropbox/ceba/doc/"

############
## preambulo
############

load("~/Dropbox/NeoMapas/Rdata/NMaves.rda")
load("~/Dropbox/NeoMapas/Rdata/SIG2.rda")
load("~/Dropbox/NeoMapas/Rdata/20181202_RS_avs.rda")



##v <- varclus(as.matrix(sitCV[,names(mus)]), similarity="spear")
##plot(v)
##abline(h=.5)

##nT01 + nT02 + dT01 + dT02 + pet01 + aet01 + Fpar01 + Fpar02 + pre01

mi.doc <- "~/Dropbox/CEBA/doc"
mi.doc <- "~/Dropbox/ceba/doc"

## estos son los datos de Eliana
(load("~/Dropbox/ceba/Rdata/Wagleri.RData"))
mi.rda <- "~/Dropbox/ceba/Rdata/RasterPsittacidae.rda"
if (file.exists(mi.rda)) {
    (load(mi.rda))
} else {
    source(sprintf("%s/720_SerieTiempoPsittacidae/pre00_rastersVenezuela.R",mi.doc))
    save(file=mi.rda,rtmp,rpre,arcilla,pendiente, suelo, ralt,wpre,wtmp,
         rtmn,rtmx,WC,ndvis,ndvis.t,
         rdpre,rdtmp,bio01,bio02,bio03,bio04,bio05,bio06,bio07,bio08,bio09,
         bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)
}


mi.rda <- "~/Dropbox/ceba/Rdata/20190113_RasterRSPsittacidae.rda"
if (file.exists(mi.rda)) {
    (load(mi.rda))
} else {

        tmp00 <- stack(c(sprintf("~/mapas/Venezuela/chirps/chirps-v2.0.2009.%02d.tif",4:12),
                     sprintf("~/mapas/Venezuela/chirps/chirps-v2.0.2010.%02d.tif",1:3)))
    pre01 <- raster(tmp00,1)
    values(pre01) <- rowSums(values(tmp00),na.rm=T)
    values(pre01)[values(pre01)==0] <- NA
    pre00 <- disaggregate(pre01,6)

        tmp00 <- stack(c(sprintf("~/mapas/Venezuela/PET_1km/MOD16A2.A2009M%02d.Venezuela_PET_1km.tif",4:12),
                     sprintf("~/mapas/Venezuela/PET_1km/MOD16A2.A2010M%02d.Venezuela_PET_1km.tif",1:3)))
    
    pet01 <- raster(tmp00,1)
    values(pet01) <- rowSums(mSSt(values(tmp00),ll=-32767,ul=32760,cf=0.1,os=0,setNA=0),na.rm=T)
    values(pet01)[values(pet01)==0] <- NA

        fls <- c(sprintf("~/mapas/Venezuela/LST_Day_1km/MOD11A2.A2009%03d.Venezuela_LST_Day_1km.tif",seq(97,365,by=8)),
                     sprintf("~/mapas/Venezuela/LST_Day_1km/MOD11A2.A2010%03d.Venezuela_LST_Day_1km.tif",seq(1,97,by=8)))
    tmp00 <- stack(fls[file.exists(fls)])
    
    dT01 <- raster(tmp00,1)
    values(dT01) <- rowMeans(mSSt(values(tmp00),ll=7500,ul=32300,cf=0.02,os=-273.15),na.rm=T)
    ##   values(pet01)[values(pet01)==0] <- NA

    fls <- c(sprintf("~/mapas/Venezuela/250m_16_days_EVI/MOD13Q1.A2009%03d.Venezuela_250m_16_days_EVI.tif",seq(97,365,by=16)),
             sprintf("~/mapas/Venezuela/250m_16_days_EVI/MOD13Q1.A2009%03d.Venezuela_250m_16_days_EVI.tif",seq(1,97,by=16)))
    for (ff in fls) {
        tmp00 <- raster(ff)
        values(tmp00) <- mSSt(values(tmp00),ll=-2000,ul=10000,cf=0.0001,os=0,setNA=0)
        tmp01 <- aggregate(tmp00,4)
        rm(tmp00)
        gc()
        if(!exists("tmp000")) {
            tmp000 <- tmp01
        } else {
            tmp000 <- stack(tmp000,tmp01)
        }
        rm(tmp01)
        gc()
       ## plot(tmp000)
    }
        evi00 <- raster(tmp000,1)
        values(evi00) <- rowMeans(values(tmp000))
        ##   values(pet01)[values(pet01)==0] <- NA
        pre01 <- resample(pre00,dT01)
        evi01 <- resample(evi00,dT01)
    save(file=mi.rda,pet01,pre01,dT01,evi01)
}


system(sprintf("rm %s.*",mi.arch))
Sweave(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""),eps=F)
Stangle(file=paste(mi.path,mi.dir,"/",mi.arch,".Rnw",sep=""))
tools::texi2dvi(paste(mi.arch,".tex",sep=""), pdf=TRUE)
system(sprintf("atril %s.pdf &",mi.arch))

hoy <- format(Sys.time(), "%Y%m%d")

system(paste("mv ",mi.arch,".pdf ~/Dropbox/ceba/doc/",mi.dir,"/",hoy,"_",titulo,".pdf",sep=""))
system(paste("mv ",mi.arch,".R ~/Dropbox/ceba/doc/",mi.dir,"/",hoy,"_",titulo,".R",sep=""))








































##############
## codigos viejos...
###############

###
## datos de NeoMapas
###
mi.rda4 <- "~/Dropbox/ceba/Rdata/NMPsittacidae.rda"
if (file.exists(mi.rda4)) {
    (load(mi.rda4))
} else {
    source("~/Dropbox/CEBA/doc/720_SerieTiempoPsittacidae/pre02_NMPsittacidae.R")
    source("~/Dropbox/CEBA/doc/720_SerieTiempoPsittacidae/pre03_Modis.R")
    ## 19/10/2015
    ## corregir orden de los puntos 7d-051 y 3a-054
   muestreos <- muestreos[match(obs1$Row.names,muestreos$V3),]


    save(file=mi.rda4,list=c("nmdat","stdat","kry","krz","krx","krw","obs1",ls(pattern="mtz$"),"trans.info","muestreos","mVars"))
}






## estos son los datos de Eliana
(load("~/Dropbox/ceba/Rdata/Wagleri.RData"))
mi.rda <- "~/Dropbox/ceba/Rdata/RasterPsittacidae.rda"
if (file.exists(mi.rda)) {
    (load(mi.rda))
} else {
    source(sprintf("%s/720_SerieTiempoPsittacidae/pre00_rastersVenezuela.R",mi.doc))
    save(file=mi.rda,rtmp,rpre,arcilla,pendiente, suelo, ralt,wpre,wtmp,
         rtmn,rtmx,WC,ndvis,ndvis.t,
         rdpre,rdtmp,bio01,bio02,bio03,bio04,bio05,bio06,bio07,bio08,bio09,
         bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)
}

mi.rda2 <- "~/Dropbox/ceba/Rdata/DFPsittacidae.rda"
if (file.exists(mi.rda2)) {
    (load(mi.rda2))
} else {
    source("~/Dropbox/CEBA/doc/720_SerieTiempoPsittacidae/pre01_datosAves.R")

    save(file=mi.rda2,avdat,qry,qrz,qrx,qrw,qrv,wa,col,mes,yr,nyr)
}



###########
## estos no estoy seguro de necesitarlos:


mi.rda5 <- "~/Dropbox/ceba/Rdata/GBIFPsittacidae.rda"
if (file.exists(mi.rda5)) {
    (load(mi.rda5))
} else {
    
    e <- extent(rpre)
    for (k in c(todos.loros,alt.loros)) {
        nm <- sprintf("gb.%s",gsub(" ",".",k))
        if (!exists(nm)) {
            prb <- occ_search(scientificName=k,
                              geometry=sprintf('POLYGON((%1$s %3$s, %1$s %4$s, %2$s %4$s, %2$s %3$s, %1$s %3$s))',e[1], e[2],e[3],e[4]),
                              limit=200000)
            assign(nm,prb)
            if (prb$meta$count>0) {
                plot(rpre,1,main=k)
                points(decimalLatitude~decimalLongitude,prb$data)
            }
        }
    }
    save(file=mi.rda5,list=ls(pattern="gb."))
}
