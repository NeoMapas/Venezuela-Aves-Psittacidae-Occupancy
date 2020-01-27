##########
## esto se paso a sesion000_hacerDocumento y Documento1...
###########

##filogenia Psittacidae
##schodde et al 2013
##monofiletico, cuatro tribus
##Kirchman et al 2012, Remsen et al 2013
##R --vanilla
setwd("~/tmp/CEBA")
require(raster)
require(maxlike)
require(compositions)
require(dismo)
require(Hmisc)
require(AICcmodavg)
source("~/Dropbox/CEBA/doc/720_SerieTiempoPsittacidae/inc01_funciones.R")
source("~/Dropbox/ceba/doc/720_SerieTiempoPsittacidae/inc01_funciones.R")



(load("~/Dropbox/ceba/Rdata/Wagleri.RData"))
mi.doc <- "~/Dropbox/CEBA/doc"
mi.doc <- "~/Dropbox/ceba/doc"
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

ls(pattern=".mtz")

##bastantes
colSums(Arat_pert.mtz[,-1],na.rm=T)
colSums(Amaz_ochr.mtz[,-1],na.rm=T)
colSums(Amaz_amaz.mtz[,-1],na.rm=T)
colSums(Ara_seve.mtz[,-1],na.rm=T)

## algunos
colSums(Forp_pass.mtz[,-1],na.rm=T)
colSums(Pion_mens.mtz[,-1],na.rm=T)
colSums(Amaz_fari.mtz[,-1],na.rm=T)
colSums(Ara_chlo.mtz[,-1],na.rm=T)
colSums(Brot_jugu.mtz[,-1],na.rm=T)

##pocos
colSums(Ara_maca.mtz[,-1],na.rm=T)
colSums(Amaz_barb.mtz[,-1],na.rm=T)
colSums(Ara_mili.mtz[,-1],na.rm=T)
colSums(Arat_leuc.mtz[,-1],na.rm=T)

colSums(Arat_wagl.mtz[,-1],na.rm=T)
colSums(Arat_acut.mtz[,-1],na.rm=T)
colSums(Brot_chry.mtz[,-1],na.rm=T)
colSums(Orth_mani.mtz[,-1],na.rm=T)
colSums(Diop_nobi.mtz[,-1],na.rm=T)
colSums(Pion_mens.mtz[,-1],na.rm=T)
colSums(Pyrr_pict.mtz[,-1],na.rm=T)

colSums(Nann_pany.mtz[,-1],na.rm=T)
colSums(Pion_barr.mtz[,-1],na.rm=T)
colSums(Pyrr_egre.mtz[,-1],na.rm=T)
colSums(Pyrr_hoem.mtz[,-1],na.rm=T)
colSums(Pyrr_mela.mtz[,-1],na.rm=T)
colSums(Pyrr_rhod.mtz[,-1],na.rm=T)


todos.loros <- c("Amazona amazonica","Amazona autumnalis",
                 "Amazona barbadensis","Amazona festiva",
                 "Amazona dufresniana","Amazona farinosa",
                 "Amazona mercenaria","Amazona ochrocephala",
                 "Ara ararauna","Ara chloroptera","Ara macao",
                 "Ara militaris","Ara severa",
                 "Aratinga solstitialis",
                 "Bolborhynchus lineola",
                 "Brotogeris chrysopterus","Brotogeris cyanoptera",
                 "Brotogeris jugularis","Deroptyus accipitrinus",
                 "Ara nobilis","Aratinga pertinax","Forpus conspicillatus",
                 "Forpus sclateri","Forpus passerinus",
                 "Hapalopsittaca amazonina","Nannopsittaca panychlora",
                 "Ara manilata","Pionites melanocephala","Pionus chalcopterus",
                 "Pionus fuscus","Pionus menstruus","Pionus seniloides",
                 "Pionus sordidus","Aratinga acuticaudata",
                 "Aratinga leocophthalma","Aratinga wagleri",
                 "Pionopsitta barrabandi","Pionopsitta caica",
                 "Pionopsitta pyrilia",
                 "Pyrrhura leucotis",
                 "Pyrrhura egregia","Pyrrhura leucotis","Pyrrhura hoematotis",
                 "Pyrrhura melanura","Pyrrhura picta","Pyrrhura rhodocephala",
                 "Touit batavica","Touit dilectissima","Touit huetii",
                 "Touit purpurata")

alt.loros <- c("Amazona amazonica","Amazona autumnalis","Amazona barbadensis",
               "Amazona bodini","Amazona dufresniana","Amazona farinosa",
               "Amazona mercenarius","Amazona ochrocephala",
               "Ara ararauna","Ara chloropterus","Ara macao","Ara militaris",
               "Ara severus","Aratinga solstitialis","Bolborhynchus lineola",
               "Brotogeris chrysoptera","Brotogeris cyanoptera",
               "Brotogeris jugularis",
               "Deroptyus accipitrinus","Diopsittaca nobilis",
               "Eupsittula pertinax",
               "Forpus conspicillatus",
               "Forpus modestus","Forpus passerinus",
               "Hapalopsittaca amazonina","Nannopsittaca panychlora",
               "Orthopsittaca manilatus",
               "Pionites melanocephala",
               "Pionus chalcopterus","Pionus fuscus","Pionus menstruus",
               "Pionus seniloides","Pionus sordidus",
               "Thectocercus acuticaudatus",
               "Psittacara leucophthalmus","Psittacara wagleri",
               "Pyrilia barrabandi","Pyrilia caica","Pyrilia pyrilia",
               "Pyrrhura caeruleiceps","Pyrrhura egregia","Pyrrhura emma",
               "Pyrrhura hoematotis","Pyrrhura melanura","Pyrrhura picta",
               "Pyrrhura rhodocephala","Touit batavica","Touit dilectissima",
               "Touit huetii","Touit purpurata","Aratinga leucophthalmus",
               "Forpus passerinus viridissimus","Amazona aestiva",
               "Pionites melanocephalus","Amazona leucocephala",
               "Orthopsittaca manilata","Psittacara acuticaudatus",
               "Aratinga jandaya","Aratinga leucophtalmus",
               "Aratinga mitrata","Aratinga wagleri transilis",
               "Touit batavicus","Touit dilectissimus","Touit purpuratus")

caz0 <- read.csv("~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos/psita_caceria",as.is=T)
caz1 <- read.csv("~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos/ama_caceria",as.is=T)
caz <- read.csv("~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos/iwu_bird.csv",as.is=T)
##caz$lon <- as.numeric(caz$lon)
##caz$lat <- as.numeric(caz$lat)
caz$lon[!is.na(caz$lon) & caz$lon < -100] <- subset(caz,lon < -100)$lon/10e5
vial <- shapefile("~/gisdata/vectorial/roads/VEN_roads.shp") 

vz0 <- shapefile("~/gisdata/vectorial/ecoSIG/division_pol.shp")

plot(vz0)
plot(vial,add=T,col="grey77")
points(lat~lon,caz,col=4,cex=.7,pch=19)
##points(y_ge~x_ge,caz0,col=2)
points(lat~lon,subset(caz,val %in% c(alt.loros,todos.loros)),col=2,cex=.7)
points(lat~lon,subset(caz,grepl("wagleri",val)),col=3,pch=19)



ttl <- ttl.2010 <- c()
for (k in 1:length(todos.loros)) {
    ttl <- c(ttl,sum(grepl(todos.loros[k],aves@data$species) |
                         grepl(alt.loros[k],aves@data$species) |
                         grepl(todos.loros[k],aves@data$scientificName) |
                         grepl(alt.loros[k],aves@data$scientificName)))
    ss <- aves@data$year %in% 2010 & !(aves@data$collectionCode %in% "doi:10.1594/PANGAEA.803430")
    ttl.2010 <- c(ttl.2010,sum(grepl(todos.loros[k],aves@data$species[ss]) |
                         grepl(alt.loros[k],aves@data$species[ss]) |
                         grepl(todos.loros[k],aves@data$scientificName[ss]) |
                         grepl(alt.loros[k],aves@data$scientificName[ss])))

}

data.frame(todos.loros,alt.loros[1:k],ttl,ttl.2010)

require(rgbif)

mi.rda5 <- "~/Rdata/GBIFPsittacidae.rda"
if (file.exists(mi.rda5)) {
    load(mi.rda5)
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

e <- extent(rpre)
mexp <- data.frame()
for (shp in dir("~/gisdata/distribuciones/BirdLife",pattern="shp",full.names=F)) {
    mi.spp <- sub("_"," ",sub("_[0-9]+.shp","",shp))
    if (mi.spp %in% c(todos.loros,alt.loros)) {
        tmp <- crop(shapefile(sprintf("~/gisdata/distribuciones/BirdLife/%s",shp)),e)
        if (!is.null(tmp)) {
            nms <- table(muestreos$NM[extract(tmp,muestreos[,c("V2","V1")])$PRESENCE %in% 1])
            if(length(nms)>0)
                mexp <- rbind(mexp,
                              data.frame(spp=mi.spp,
                                         NM=nms))
            print(tail(mexp))
        }
    }

}
names(mexp) <- c("spp","NM","freq")
mexp$spp %in% c(alt.loros,todos.loros)
table(unique(mexp$spp) %in% c(alt.loros,todos.loros))
## Aratinga solsticialis y Forpus conspicillatus no se solapan

alt.loros[!alt.loros %in% mexp$spp]



table(mexp$exp>0)

table(sub("_"," ",subset(mexp,exp>0)$spp) %in% alt.loros)





















##no
mexp <- data.frame()
SEDAC.mptc <- "~/distribuciones/SEDAC/Psittacidae"
for (bls in dir(SEDAC.mptc,pattern="bil",full.names=F)) {
    tmp <- crop(raster(sprintf("%s/%s",SEDAC.mptc,bls)),e)
    mexp <- rbind(mexp,data.frame(spp=sub("_gdg.bil","",bls),
                                  exp=sum(extract(tmp,muestreos[,c("V2","V1")]),na.rm=T)))
    print(tail(mexp))
}
##no
mexp <- data.frame()
for (bls in dir(SEDAC.mptc,pattern="shp$",full.names=F)) {
    tmp <- shapefile(sprintf("%s/%s",SEDAC.mptc,bls))
    nms <- unique(muestreos$NM[extract(tmp,muestreos[,c("V2","V1")])$PRESENCE %in% 1])
    if (length(nms)>0)
        mexp <- rbind(mexp,data.frame(spp=sub("_pl.shp","",bls),
                                      NM=nms))
    ##exp=sum(extract(tmp,muestreos[,c("V2","V1")])$PRESENCE %in% 1,na.rm=T)))
    print(tail(mexp))
}




atg.leu <- grepl("Aratinga leucoph",aves@data$species)

ara.sev <- grepl("Ara sever",aves@data$species)
plot(aves)
points(aves[ara.sev,],col=2)

## registros dudosos: en caracas y los andes 
points(aves[atg.leu,],col=3)


grep(gg,spp.aves$Latname)

acdg <- subset(spp.aves,grepl("wagleri",Latname))$N_ave
plot(aves[wa,])
points(lat~lon,subset(NM.m1,Especieid %in% acdg),col=2)
points(lat~lon,NM.m1)


## NDVI
## rNDVI <- raster("~/sensores/AVHRR/GIMMS/geo81aug15a.n07-VI3")

if (!exists("v"))
    v <- varclus(as.matrix(avdat))
head(avdat)
plot(v)
abline(h=.5)

vz0 <- shapefile("~/vectorial/ecoSIG/division_pol.shp")

##
   source("~/Dropbox/CEBA/doc/720_SerieTiempoPsittacidae/Psittacara_wagleri.R")




boxplot(avdat$arcilla~wa)
##DSMW <- shapefile("~/suelos/FAO/DSMW.shp")
##vDSMW <- crop(DSMW,WC)
identical(names(rpre),names(rtmp))

## considerar como observaciones únicas aquellas con diferentes combinaciones
## de celdas de todos los mapas considerados, además del periodo temporal
## luego contar el total de observaciones (n) por combinación única para estimar
## el peso (log10(n+1))
dim(unique(data.frame(qrw,qrx,qry,qrz,wa)))

##t0 <- read.csv2("~/data/Aves_Vzla.csv",dec=".")
##subset(grepl("wagleri",t0$scientificname)
##t0 <- subset(t0,!is.na(decimallatitude))


##Awag <- rbind(aves,t2,t3)
##e0 <- data.frame(extract(rtmp,aves))
##d0 <- data.frame(extract(rdtmp,aves))
##p0 <- data.frame(extract(rpre,aves))
##q0 <- data.frame(extract(rdpre,aves))

##ms <- as.numeric(factor(aves@data$Mes,levels=c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")))

dim(unique(data.frame(qrw,qrx,qry,qrz,mes,yr,wa)))
dim(unique(data.frame(qrw,qrx,qry,qrz,col,wa)))
dim(unique(data.frame(qrw,qrx,qry,qrz,wa)))
table(!duplicated(data.frame(qrw,qrx,qry,qrz,wa,yr,mes,col)))
##aves@data$year>1900 & aves@data$year<2011 &

##sort(abs(cor(cbind(x1,wa),use="complete")[1:19,20]))

slc <- cutree(v$hclust,h=.5)
vtp <- c(rep("P",19),
         rep("S",2),
         rep("TS",4),
         rep("Y",19),
         rep("M",2),
         rep("TS",2),
         rep("M",2))

table(vtp,slc)

##static (1)
~arcilla
## time series slc[vtp %in% "TS"] (2,5)
~ctmp + cpre
## year (2,6,7,8,10,9,11)
~mbio01 + mbio2 + mbio07 + mbio12 + mbio15 + mbio17 + mbio18
## mensual (2,4,12,13)
~wtmp + wpre + cphi + sphi 
## promedio (2,14,16,17,15,18,19)
~wbio01 + wbio02 + wbio07 + wbio12 + wbio03 + wbio17 + wbio18

##full
~arcilla + ctmp + cpre +  mbio2 + mbio07 + mbio12 + mbio15 + mbio17 + mbio18



table(ss)

require(AICcmodavg)



###
# predicción

plot(wtmp,1)
points(aves[ss,])

###
## por periodos de tiempo, específicamente lm.W1D 
###

##prd <- 1950:1980
##prd <- 1981:2010


mi.rda3 <- "~/Rdata/ResultadosAwagleri.rda"
save(file=mi.rda3,
     mpsi.01,mpsi.02,mpsi.03,mpsi.04,mpsi.05,mpsi.06,
     mpsi.07,mpsi.08,mpsi.09,mpsi.10,mpsi.11,mpsi.12,
     mpsi.t1,mpsi.t2,mpsi.t3)

load(mi.rda3)

plot(mpsi.t3)
points(lat~lon,NM.m1)

prd.NM <- extract(mpsi.t3,NM.m1[,c("lon","lat")])
hist(extract(mpsi.t3,NM.m1[,c("lon","lat")]))
table((NM.m1$Especieid %in% acdg))
table(prd.NM[(NM.m1$Especieid %in% acdg)])




## aparentemente un cop >0.75
mean(extract(psi,aves[wa,])>0.77,na.rm=T)

require(RColorBrewer)

plot(stack(mpsi.t1,mpsi.t2,mpsi.t3),breaks=seq(0,1,length=11),
     col=c("grey77",brewer.pal(9,"YlGnBu")))

perdidas <- stack(mpsi.t1 * (1-mpsi.t2),
                 mpsi.t2 * (1-mpsi.t3))
names(perdidas) <- c("PerdidasT2","PerdidasT3")
ganancias <- stack(mpsi.t2 * (1-mpsi.t1),
                   mpsi.t3 * (1-mpsi.t2))
names(ganancias) <- c("GananciasT2","GananciasT3")
plot(perdidas,breaks=seq(0,.54,length=9),
     col=brewer.pal(9,"RdPu"))
plot(ganancias,breaks=seq(0,.81,length=9),
     col=brewer.pal(9,"Blues"))

p0 <- (1-mpsi.t1)*(1-mpsi.t2)*(1-mpsi.t3)
p1 <- mpsi.t1*mpsi.t2*mpsi.t3
p2 <- mpsi.t1*(1-mpsi.t2)*(1-mpsi.t3)
p3 <- mpsi.t1*(1-mpsi.t2)*mpsi.t3
ps <- stack(p0,p1,p2,p3)
vps <- values(ps)
idx <- unlist(apply(vps[rowSums(is.na(vps))==0,],1,which.max))

rslt <- p0
values(rslt) <- NA

values(rslt)[rowSums(is.na(vps))==0] <- idx
plot(rslt,col=c("white","green","red","yellow"))

plot((mpsi.01/71)*(mpsi.05/71))





## construir modelos con variables derivadas promedio:
## compararlos con modelos con variables derivadas por año
##    (calcular biovars para cada año!
## luego con modelo con variables mensuales promedio
## y con modelo que tome en cuenta diferencia entre años

yp <-avdat[ss & wa,]
ya <-avdat[ss & !wa,]

ml1 <- maxlike.df( ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) +
                      wbio03 + I(wbio03^2) + dtmp + dpre,
                  yp,ya,start=coef(lm.W1D),fixed=coef(lm.W1D)*c(1,rep(NA,8)))
ml1 <- maxlike.df( ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) +
                      wbio03 + I(wbio03^2) + dtmp + dpre,
                  yp,ya,start=coef(ml1),fixed=coef(lm.W1D)*c(1,rep(NA,8)))

ml2 <- maxlike.df( ~ arcilla + I(arcilla^2) +
                      wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) +
                      wbio03 + I(wbio03^2) + dtmp + dpre,
                  yp,ya,start=coef(lm.A1W1D))
ml2 <- maxlike.df( ~ arcilla + I(arcilla^2) +
                      wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) +
                      wbio03 + I(wbio03^2) + dtmp + dpre,
                  yp,ya,start=coef(ml2),fixed=round(coef(ml2),1)*c(0.1,rep(NA,10)))
ml2 <- maxlike.df( ~ arcilla + I(arcilla^2) +
                      wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) +
                      wbio03 + I(wbio03^2) + dtmp + dpre,
                  yp,ya,start=coef(ml2),fixed=coef(ml2)*c(1,rep(NA,10)))















## bio01,bio12,bio17
lm.sm <- glm(y ~ mbio01 + mbio15 + mbio07 + I(mbio01^2) + I(mbio15^2) + I(mbio07^2) ,aves@data[ss,],family=binomial)


lm.fp <- glm(y ~ wtmp + wpre + I(wtmp^2) + I(wpre^2) + cphi + sphi ,aves@data[ss,],family=binomial)
lm.sp <- glm(y ~ wtmp + I(wtmp^2) + cphi + sphi ,aves@data[ss,],family=binomial)


## bio01,bio12,bio17
lm.dw <- glm(y ~ wbio01 + wbio12 + wbio03 + I(wbio01^2) + I(wbio12^2) + I(wbio03^2) +dtmp + dpre,aves@data[ss,],family=binomial)
lm.dm <- glm(y ~ mbio01 + mbio15 + mbio07 + I(mbio01^2) + I(mbio15^2) + I(mbio07^2) +dtmp + dpre,aves@data[ss,],family=binomial)
lm.dp <- glm(y ~ wtmp + I(wtmp^2) + cphi + sphi +dtmp + dpre,aves@data[ss,],family=binomial)
lm13 <- glm(y~arcilla + ctmp + cpre +  mbio02 + mbio07 + mbio12 + mbio15 + mbio17 + mbio18+I(arcilla^2) + I(ctmp^2) + I(cpre^2) + I( mbio02^2) + I(mbio07^2) + I(mbio12^2) + I(mbio15^2) + I(mbio17^2) + I(mbio18^2),aves@data[ss,],family=binomial)

##bio7 = Temperature annual range (bio5-bio6)
##bio18 = Precipitation of warmest quarter

lm14 <- glm(y~bio01+bio07+bio18+I(bio01^2)+I(bio07^2)+I(bio18^2),aves@data[ss,],family=binomial)
lm15 <- glm(y~ctmp+bio07+bio18+I(ctmp^2)+I(bio07^2)+I(bio18^2),aves@data[ss,],family=binomial)
lm16 <- glm(y~wtmp+bio07+bio18+I(wtmp^2)+I(bio07^2)+I(bio18^2),aves@data[ss,],family=binomial)
lm17 <- glm(y~wtmp+bio07+bio18+I(wtmp^2)+I(bio07^2)+I(bio18^2)+dtmp,aves@data[ss,],family=binomial)



layout(matrix(1:6,ncol=2))
boxplot(aves@data$pre~wa)
boxplot(aves@data$wpre~wa)
boxplot(aves@data$wtmp~wa)
boxplot(aves@data$tmp~wa)
boxplot(aves@data$dtmp~wa)
boxplot(aves@data$dpre~wa)
plot(dtmp~tmp,aves@data,col=wa+1,cex=1+2*wa)

plot(aves)
points(aves[is.na(aves@data$pendiente),],col=2)

ss <- !is.na(aves@data$year) & !is.na(aves@data$month) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(aves@data$pre) & !is.na(aves@data$wpre) & !is.na(aves@data$pendiente) & !is.na(aves@data$arcilla) & !duplicated(data.frame(qrw,qrx,qry,qrz,wa,mes,yr,col))


##table(duplicated(qry,qrx,qrw,qrz,col,mes,wa))



boxplot(tmp~month,yp)

##boxplot(tmp~year,yp)
##boxplot(tmp~year,ya)

for (k in c("tmp","pre")) {
    mu <- mean(ya[,k],na.rm=T)
    sg <- sd(ya[,k],na.rm=T)
    yp[,k] <- (yp[,k]-mu)/sg
    ya[,k] <- (ya[,k]-mu)/sg
}

yp$phi <- yp$month/12
ya$phi <- ya$month/12


ml1 <- maxlike.df(~tmp+I(tmp^2)+pre+I(pre^2),yp,ya,fixed=c(-3,NA,NA,NA,NA))
fx <- c(boot::logit(nrow(yp)/nrow(ya))/2,1.0,-0.1,1.0,-.10)
ml1 <- maxlike.df(~tmp+I(tmp^2)+pre+I(pre^2),yp,ya,
                  fixed=fx*c(1,NA,NA,NA,NA),start=fx)

fx <- c(-3.3023771,  0.8937817, -0.1226999,  0.2429410, -0.6097604)
-3.3023771  0.1558636 -0.9710315 -0.7265190 -0.1542083
fx <- c(boot::logit(nrow(yp)/nrow(ya))/2,runif(4,-1.1,1.2))##2.0,-0.2,.10,3.0)

fx <- coef(ml2)/max(coef(ml2))*c(1,runif(4,-1.1,1.2))
(ml2 <- maxlike.df(~tmp+I(tmp^2)+sin(phi)+cos(phi),yp,ya,
                  fixed=fx*c(1,NA,NA,NA,NA),start=fx))


boxplot(tmp~month,yp)
layout(matrix(1:6,ncol=2))

for (k in 1:6) {
    x <- 10:32
    nwdt <- data.frame(1,tmp=x,tmp2=x^2,phi=sin(k/12),phi2=cos(k/12))
    y <- boot::inv.logit(coef(ml2) %*% t(nwdt))
    boxplot(subset(yp,month==k)$tmp,horizontal=T,ylim=range(x),xlim=c(0,1.5))
    boxplot(subset(ya,month==k)$tmp,at=0.5,horizontal=T,add=T,col=2)
    lines(x,y)
}

wm <- c()
for (k in 1:12) {
    x <- 10:32
    nwdt <- data.frame(1,tmp=x,tmp2=x^2,phi=k/12,phi2=k/12)
    y <- boot::inv.logit(coef(ml2) %*% t(nwdt))
    wm <- c(wm,weighted.mean(nwdt$tmp,y))
}
plot(yp$month,yp$tmp)
lines(1:12,wm)



lines(1:12,boot::inv.logit(coef(ml2) %*% t(nwdt)))
lines(1:12,coef(ml2) %*% t(nwdt))



slc <- c("arcilla",
         "ctmp", ## o "bio01", "tmp", "wtmp"...
         "wpre","cphi","sphi","bio02","bio04","bio07","bio19","bio15","bio18")


y <- wa[ss]
lm1 <- glm(y~tmp+I(tmp^2)+pre+I(pre^2),aves@data[ss,],family=binomial)
lm2 <- glm(y~tmp+I(tmp^2)+sphi+cphi,aves@data[ss,],family=binomial)
lm3 <- glm(y~wtmp+I(wtmp^2),aves@data[ss,],family=binomial)
lm4 <- glm(y~wtmp+I(wtmp^2)+sphi+cphi,aves@data[ss,],family=binomial)
lm5 <- glm(y~ctmp+I(ctmp^2),aves@data[ss,],family=binomial)
lm6 <- glm(y~ctmp+I(ctmp^2)+sphi+cphi,aves@data[ss,],family=binomial)
lm7 <- glm(y~bio09+bio19+bio15+bio02+bio18+bio03,aves@data[ss,],family=binomial)
lm7 <- glm(y~bio09+bio19+bio15+bio02+bio18+bio03,aves@data[ss,],family=binomial)
lm8 <- glm(y~poly(bio09,bio19,bio15,bio02,bio18,bio03,degree=2),aves@data[ss,],family=binomial)
lm9 <- glm(y~bio03+I(bio03^2)+bio15+I(bio15^2),aves@data[ss,],family=binomial)
lm10 <- glm(y~wtmp+I(wtmp^2)+bio15+I(bio15^2),aves@data[ss,],family=binomial)
lm11 <- glm(y~bio01+I(bio01^2)+bio15+I(bio15^2),aves@data[ss,],family=binomial)

lm12 <- glm(y~arcilla+ctmp+wpre+cphi+sphi+bio02+bio04+bio07+bio19+bio15+bio18

               ,aves@data[ss,],family=binomial)
lm13 <- glm(y~arcilla+ctmp+wpre+cphi+sphi+bio02+bio04+bio07+bio19+bio15+bio18+I(arcilla^2)+I(ctmp^2)+I(wpre^2)+I(bio02^2)+I(bio04^2)+I(bio07^2)+I(bio19^2)+I(bio15^2)+I(bio18^2),aves@data[ss,],family=binomial)

##bio7 = Temperature annual range (bio5-bio6)
##bio18 = Precipitation of warmest quarter

lm14 <- glm(y~bio01+bio07+bio18+I(bio01^2)+I(bio07^2)+I(bio18^2),aves@data[ss,],family=binomial)
lm15 <- glm(y~ctmp+bio07+bio18+I(ctmp^2)+I(bio07^2)+I(bio18^2),aves@data[ss,],family=binomial)
lm16 <- glm(y~wtmp+bio07+bio18+I(wtmp^2)+I(bio07^2)+I(bio18^2),aves@data[ss,],family=binomial)
lm17 <- glm(y~wtmp+bio07+bio18+I(wtmp^2)+I(bio07^2)+I(bio18^2)+dtmp,aves@data[ss,],family=binomial)

## construir modelos con variables derivadas promedio (lm14)
## compararlos con modelos con variables derivadas por año
##    (calcular biovars para cada año!)
## luego con modelo con variables mensuales promedio
## y con modelo que tome en cuenta diferencia entre años


boot::inv.logit(rowSums(predict(lm3,data.frame(wtmp=(100:300)/10),type="terms")))
boot::inv.logit(rowSums(predict(lm5,data.frame(ctmp=(10:30)),type="terms")))


AIC(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12,lm13)

AICc(lm7)
AICc(lm7)
AICc(lm8)

plot(10:30,boot::inv.logit(rowSums(predict(lm5,data.frame(ctmp=(10:30)),type="terms"))))
lines((100:300/10),boot::inv.logit(rowSums(predict(lm3,data.frame(wtmp=(100:300)),type="terms"))),col=2)


plot(tmp~ctmp,aves@data)
abline(a=0,b=1,col=2)


wm <- c()
for (k in 1:12) {
    x <- 10:32
    nwdt <- data.frame(1,tmp=x,sphi=sin(2*pi*k/12),cphi=cos(2*pi*k/12))
    y <- boot::inv.logit(rowSums(predict(lm2,nwdt,"terms")))
    wm <- c(wm,weighted.mean(nwdt$tmp,y))
}
plot(yp$month,yp$tmp)
lines(1:12,wm)

weighted.mean(nwdt$tmp,boot::inv.logit(rowSums(predict(lm2,nwdt,"terms"))))

predict(lm2,data.frame(tmp=20,phi=.1),"terms")


mean(predict(lm1,type="response")[y])/mean(predict(lm1,type="response")[!y])

lm2 <- glm(y~pendiente+arcilla+tmp+I(tmp^2)+pre+I(pre^2),aves@data[ss,],family=binomial)
lm3 <- glm(y~pendiente+arcilla+wtmp+I(wtmp^2)+dtmp+I(dtmp^2),aves@data[ss,],family=binomial)
lm3 <- glm(y~wtmp+I(wtmp^2)+dtmp+I(dtmp^2),aves@data[ss,],family=binomial)

##predecir todas las vrs, ejemplo lm3

  
psi <- raster(vrs,1)
values(psi) <- NA
nwdt <- data.frame(values(vrs))
colnames(nwdt) <- c("arcilla","tmp","wtmp","pendiente","pre","dtmp",
                    "bio02","bio03","bio09","bio15","bio18","bio19")
                   ##pre=values(rpre)[,1200],
                   ##tmp=values(rtmp)[,1200])
##predict(lm1,nwdt)
sn <- rowSums(is.na(nwdt))==0

values(psi) <- predict(lm1,nwdt,type="response")
values(psi) <- predict(lm2,nwdt,type="response")

values(psi) <- predict(lm3,nwdt,type="response")
values(psi)[sn] <- boot::inv.logit(rowSums(predict(lm8,nwdt[sn,],type="terms")))
plot(psi)
points(aves[wa,])

values(psi) <- predict(lm3,nwdt,type="response")
plot(psi)
points(aves[wa,])
nlayers(rpre)





###
## por meses
###
cop <- median(aggregate(predict(lm3,type="response"),list(y),median)$x)
for (prd in 1:12) {
    r2 <- raster(wtmp,prd)
    mpsi <- raster(wtmp,1)*0
    j <- 0
    for (k in seq(along=names(rpre))[as.numeric(substr(names(rpre),7,8)) %in% prd]) {
        r8 <- disaggregate(raster(rdtmp,k),10)
        r9 <- resample(r8,r2)
        
        vrs <- stack(r2,r9)
        
        psi <- raster(vrs,1)
        nwdt <- data.frame(values(vrs))
        colnames(nwdt) <- c("wtmp","dtmp")
        values(psi) <- predict(lm3,nwdt,type="response")
        mpsi <- mpsi+(psi>cop)
        j <- j+1
        plot(mpsi/j)
    }
    assign(sprintf("mpsi.%02d",prd), mpsi/j)
    
}

plot(mpsi.01 * (1-mpsi.06))
plot(mpsi.12 * (1-mpsi.09))




psi <- raster(rpre,1)
mpsi <- psi*0
for (k in 1:nlayers(rpre)) {
nwdt <- data.frame(pre=values(rpre)[,k],
                   tmp=values(rtmp)[,k])
##predict(lm1,nwdt)
values(psi) <- predict(lm1,nwdt,type="response")
mpsi <- mpsi+(psi>0.001)
plot(mpsi/k)

}

###
## por periodos de tiempo
###
mpsi <- psi*0
j <- 0
for (k in seq(along=names(rpre))[as.numeric(substr(names(rpre),2,5)) %in% 1940:1950]) {
nwdt <- data.frame(pre=values(rpre)[,k],
                   tmp=values(rtmp)[,k])
##predict(lm1,nwdt)
values(psi) <- predict(lm1,nwdt,type="response")
mpsi <- mpsi+(psi>0.001)
j <- j+1
plot(mpsi/j)

}
mpsi.t1 <- mpsi/j
plot(mpsi.t1)

mpsi <- psi*0
j <- 0
for (k in seq(along=names(rpre))[as.numeric(substr(names(rpre),2,5)) %in% 1960:1990]) {
nwdt <- data.frame(pre=values(rpre)[,k],
                   tmp=values(rtmp)[,k])
##predict(lm1,nwdt)
values(psi) <- predict(lm1,nwdt,type="response")
mpsi <- mpsi+(psi>0.001)
j <- j+1
plot(mpsi/j)

}
mpsi.t2 <- mpsi/j
plot(mpsi.t1*(1-mpsi.t2))


for (ms in 1:12) {
    mpsi <- psi*0
    for (k in grep(sprintf("\\.%02d\\.",ms),names(rpre))) {
        nwdt <- data.frame(pre=values(rpre)[,k],
                           tmp=values(rtmp)[,k])
        ##predict(lm1,nwdt)
        values(psi) <- predict(lm1,nwdt,type="response")
        mpsi <- mpsi+(psi>0.001)
        plot(mpsi/k)
    }
    assign(sprintf("mpsi.%02d",ms), mpsi)
}

length(unique(aves@data$eventDate))
plot(mpsi.01)
plot(mpsi.02)
plot(mpsi.03)
plot(mpsi.04)
plot(mpsi.05)
plot(mpsi.06)
plot(mpsi.07)
plot(mpsi.08)
plot(mpsi.09)
plot(mpsi.10)
plot(mpsi.11)
plot(mpsi.12)


## diferentes opiniones sobre versiones de R2 para reg. log.:
## y http://www.ats.ucla.edu/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm
## http://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
##http://stats.stackexchange.com/questions/3559/which-pseudo-r2-measure-is-the-one-to-report-for-logistic-regression-cox-s

### ver recomendación en http://statisticalhorizons.com/r2logistic
##Tjur, T. (2009) “Coefficients of determination in logistic regression models—A new proposal: The coefficient of discrimination.” The American Statistician 63: 366-372.




###
## se puede explorar...
require(ppmlasso)
##Renner, I.W. & Warton, D.I. (2013). Equivalence of MAXENT and Poisson point process models for species distribution modeling in ecology. _Biometrics_ *69*, 274-281.
##ppm.form = ~ bio09+bio19+bio15+bio02+bio18+bio03
##species.ppm = ppmdat(sp.xy = aves@data[ss & wa,], back.xy = aves@data[ss & !wa,])
##species.ppm = ppmdat(sp.xy =aves@data[ss & wa,],
##back.xy = head(aves@data[ss & !wa,],100))
##ppm.fit  = ppmlasso(ppm.form, sp.xy = aves@data[ss & wa,],
##    data=species.ppm)
   

##lm8 <- glm(y~bio09+bio19+bio15+bio02+bio18+bio03,aves@data[ss,],family=binomial)

##10+(23+(52/60))/60 -1 * (67+(0+(22/60))/60)


tmp <- stack(dir("~/mapas/Venezuela/ET_1km",full.names=T)[-1])
www <- extract(tmp,data.frame(-1 * (67+(0+(22/60))/60),10+(23+(52/60))/60))

tmp <- stack(dir("~/mapas/Venezuela/PET_1km",full.names=T)[-1])
uuu <- extract(tmp,data.frame(-1 * (67+(0+(22/60))/60),10+(23+(52/60))/60))

ET.IVIC <- mSSt(www,ll=-32767,ul=32760,cf=0.1,os=0,setNA=0)
PET.IVIC <- mSSt(uuu,ll=-32767,ul=32760,cf=0.1,os=0,setNA=0)

pet.ts <- ts(t(PET.IVIC),start=c(2000,2),end=2014,frequency=12)
et.ts <- ts(t(ET.IVIC),start=c(2000,2),end=2014,frequency=12)

plot(decompose(pet.ts))
plot(pet.ts,ylim=c(0,250),lty=3,col="slateblue",ylab="Evapotranspiración (según satélite MODIS)",xlab="Año",main="Altos de Pipe")
lines(decompose(pet.ts)$trend,col="blue",lwd=2)
lines(et.ts,col="darkgreen",lty=3)
lines(decompose(et.ts)$trend,col="green",lwd=2)
plot(decompose(pet.ts)$trend-decompose(et.ts)$trend)


##stack(dir("~/mapas/Venezuela/LST_Day_1km"))
##stack(dir("~/mapas/Venezuela/ET_1km"))

sy <- aves@data$species %in% c(alt.loros,todos.loros)
rsm.dts <- table(aves@data$species[sy],
                 cut(aves@data$year[sy],
                     breaks=c(0,1950,1980,1990,2000,2010,2016),
                     labels=c("<1950","50-80","80-90","90-2000","2000-2010",">2010")))

png(file="Registros_tiempo.png",width=1000,height=600,pointsize=18)
par(mar=c(13,5,1,1))

barplot(t(rsm.dts[rowSums(rsm.dts)>0,]),las=2,col=brewer.pal(6,"Accent"),border=brewer.pal(6,"Accent"),font.axis=3,legend=T,ylab="Registros\nColecciones, Museos y observaciones",cex.axis=.7)
dev.off()




Desarrollar esta tabla...
Valores de las variables
Relación con las variables

Nuevas variables

NO
NO
NO
Distribución estable
NO
NO
SI
Perturbación externa
NO
SI
NO
Adaptación o perturbación indirecta
SI
NO
NO
Perturbación observable
Desarrollar esta tabla...
Valores de las variables
Relación con las variables
Nuevas variables

NO
NO
NO
Distribución estable
NO
NO
SI
Perturbación externa
NO
SI
NO
Adaptación o perturbación indirecta
SI
NO
NO
Perturbación observable


caz <- read.csv("~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos/iwu_bird.csv",as.is=T)
load("~/Dropbox/NeoMapas/Rdata/NMaves.rda")

for (k in unique(subset(caz,val %in% spp.aves$Latname)$val)[1:10]) {
    cat(with(subset(caz,val %in% k),
         sprintf("%s of ''%s'' are used in Venezuela in the %s for %s.\n",
                 paste(unique(recurso),collapse=","), k, paste(unique(int),collapse=","), paste(unique(venta),collapse=","))))
}
