### R code from vignette source '~/Dropbox/ceba/doc/720_SerieTiempoPsittacidae/Documento1_PsittacidaeVenezuelaNM.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: packages and GIS
###################################################
require(rgbif)
require(unmarked)
require(AICcmodavg)
require(RColorBrewer)
require(randomForest)
source("~/Dropbox/Mapoteca/inc/inc00_funciones.R")


Mm <- function(x) {
	y <- tolower(x)
	z <- paste(toupper(substr(y,0,1)),substr(y,2,nchar(y)),sep="")
	z
}

if (!exists("vz0"))
    vz0 <- shapefile("/opt/gisdata/vectorial/ecoSIG/division_pol.shp")
if (!exists("TMWB"))
    tmwb <- shapefile("/opt/gisdata/vectorial/TMworldborders/TM_WORLD_BORDERS-0.3.shp")



###################################################
### code chunk number 2: CNEB
###################################################
##para 82 milimetros (una columna): 82*0.03937*600
##para 2/3 de columan es 2800...
##tiff(filename = "FerrerSanchez_Figure1.tiff", width = 2800, height = 2800*.84, units = "px", pointsize = 64, compression = c("jpeg"), bg = "white")
 ## usar lwd=4
par(mar=c(3,3,1,1),xaxs="i",yaxs="i",lwd=1)
plot(crop(tmwb,extent(vz0)),col="aliceblue",border="grey",ylim=c(0.6526045,13))
plot(vz0,border="maroon",col="white",ylim=c(0,12),add=T)
plot(subset(CNEB,cdg %in% subset(CNEB.nm,UM %in% 1)$cdg),add=T,border="grey77")
plot(subset(CNEB,cdg %in%  trans.info$CNEB),col=1,add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="white",cex=.7)
text(-70,4.8,"Colombia")
text(-61,2,"Brazil")
text(-66,12.5,"Caribbean Sea")
axis(2,seq(0,12,by=2),sprintf("%02s°N",seq(0,12,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
scalebar(200,xy=c(-72,3),type="bar",divs=2,below="km",lonlat=T)
##dev.off()


###################################################
### code chunk number 3: ListaLoros
###################################################

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



###################################################
### code chunk number 4: MapasBirdLife
###################################################

if (!exists("mexp")) {
    e <- extent(rpre)
    mexp <- data.frame()
    gis.data <- "/opt/gisdata/distribuciones/BirdLife/Psittacidae"
    
    for (shp in dir(gis.data,pattern="shp",full.names=F)) {
        mi.spp <- sub("_"," ",sub("_[0-9]+.shp","",shp))
        if (mi.spp %in% c(todos.loros,alt.loros)) {
            tmp <- crop(shapefile(sprintf("%s/%s",gis.data,shp)),e)
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
    ##mexp$spp %in% c(alt.loros,todos.loros)
    ##table(unique(mexp$spp) %in% c(alt.loros,todos.loros))
    ## Aratinga solsticialis y Forpus conspicillatus no se solapan
    mexp$aspp <- sapply(as.character(mexp$spp),function(x) {y <- strsplit(x," ")[[1]]; paste(substr(y[1],1,4),substr(y[2],1,4),sep="_")})
    
    mexp$aspp <- sub("Psit_acut","Arat_acut",mexp$aspp)
    mexp$aspp <- sub("Psit_leuc","Arat_leuc",mexp$aspp)
    mexp$aspp <- sub("Eups_pert","Arat_pert",mexp$aspp)
    mexp$aspp <- sub("Psit_wagl","Arat_wagl",mexp$aspp)
    mexp$aspp <- sub("Pyri_barr","Pion_barr",mexp$aspp)
}


###################################################
### code chunk number 5: ttl2010
###################################################
if (!exists("ttl.2010")) {
    ttl <- ttl.2010 <- c()
    for (k in 1:length(todos.loros)) {
        ttl <- c(ttl,sum(grepl(todos.loros[k],aves@data$species) |
                             grepl(alt.loros[k],aves@data$species) |
                                 grepl(todos.loros[k],aves@data$scientificName) |
                                     grepl(alt.loros[k],aves@data$scientificName)))
        ss <- aves@data$year %in% 2008:2012 & !(aves@data$collectionCode %in% "doi:10.1594/PANGAEA.803430")
        ttl.2010 <- c(ttl.2010,sum(grepl(todos.loros[k],aves@data$species[ss]) |
                                       grepl(alt.loros[k],aves@data$species[ss]) |
                                           grepl(todos.loros[k],aves@data$scientificName[ss]) |
                         grepl(alt.loros[k],aves@data$scientificName[ss])))
        
    }
}

##aggregate(mexp$freq,list(mexp$spp),sum)

rsm.loros <- data.frame(old.spp=todos.loros,
                        ##spp=alt.loros[1:50],
                        aspp=sapply(alt.loros[1:50],function(x) {
                            y <- strsplit(x," ")[[1]]
                            paste(substr(y[1],1,4),substr(y[2],1,4),sep="_")
                        }),
                        GBIF=ttl,GBIF.2010=ttl.2010,stringsAsFactors=F)

rsm.loros$aspp <- sub("Thec_acut","Arat_acut",rsm.loros$aspp)
rsm.loros$aspp <- sub("Psit_leuc","Arat_leuc",rsm.loros$aspp)
rsm.loros$aspp <- sub("Eups_pert","Arat_pert",rsm.loros$aspp)
rsm.loros$aspp <- sub("Psit_wagl","Arat_wagl",rsm.loros$aspp)
rsm.loros$aspp <- sub("Pyri_barr","Pion_barr",rsm.loros$aspp)




###################################################
### code chunk number 6: Documento1_PsittacidaeVenezuelaNM.Rnw:269-272 (eval = FALSE)
###################################################
## alt.loros[!alt.loros %in% mexp$spp]
## table(mexp$exp>0)
## table(sub("_"," ",subset(mexp,exp>0)$spp) %in% alt.loros)


###################################################
### code chunk number 7: obsC
###################################################
if (!exists("obsC")) {
    ## importante, fecha corregida el 19/10/2015
    mt1 <- tapply(NM.m1$hrs,list(factor(NM.m1$idpunto,levels=muestreos$V3)),max)
    mt2 <- tapply(NM.m2$hrs,list(NM.m2$idpunto,NM.m2$Lapso),max)
    mt2[,1] <- rowMeans(mt2,na.rm=T)
    mt2[,2] <- mt2[,1]+3/(60*24)
    mt2[,3] <- mt2[,2]+3/(60*24)
    
    obsC <- merge(mt1,mt2,by="row.names",all=T)
    
    obsC <- obsC[match(avs.ll$V3,obsC$Row.names),]
}


###################################################
### code chunk number 8: Precipitation
###################################################
plot(pre01,col=brewer.pal(9,"Spectral"))
plot(vz0,add=T)



###################################################
### code chunk number 9: PET
###################################################
plot(pet01,col=brewer.pal(9,"Spectral"))
plot(vz0,add=T)



###################################################
### code chunk number 10: Temperature
###################################################

plot(dT01,col=rev(brewer.pal(9,"Spectral")))
plot(vz0,add=T)



###################################################
### code chunk number 11: EVI
###################################################

plot(evi01,col=brewer.pal(9,"Spectral"))
plot(vz0,add=T)



###################################################
### code chunk number 12: sitC
###################################################
if (!exists("sitC")) {
    ##all(avs.ll$V3==obs1$Row.names)
    sitC <- data.frame(idpunto=avs.ll$V3)
    for (k in unique(avs.ll$fPET)) {
        wch <- grep(gsub("M",".",k),colnames(avs.chirps))
        sitC[avs.ll$fPET %in% k,"pre01"] <- apply(avs.chirps[avs.ll$fPET %in% k,(wch-11):wch],1,sum,na.rm=T)
    sitC[avs.ll$fPET %in% k,"pre02"] <- apply(avs.chirps[avs.ll$fPET %in% k,(wch-11):wch],1,mad,na.rm=T)
    wch <- grep(k,colnames(avs.PET))
    sitC[avs.ll$fPET %in% k,"pet01"] <- apply(avs.PET[avs.ll$fPET %in% k,(wch-11):wch],1,sum,na.rm=T)
    sitC[avs.ll$fPET %in% k,"pet02"] <- apply(avs.PET[avs.ll$fPET %in% k,(wch-11):wch],1,mad,na.rm=T)

    wch <- grep(k,colnames(avs.ET))
    sitC[avs.ll$fPET %in% k,"aet01"] <- apply(avs.ET[avs.ll$fPET %in% k,(wch-11):wch],1,sum,na.rm=T)
    sitC[avs.ll$fPET %in% k,"aet02"] <- apply(avs.ET[avs.ll$fPET %in% k,(wch-11):wch],1,mad,na.rm=T)
}
sitC$pet01 <- ifelse(sitC$pet01 %in% 0,NA,sitC$pet01)
sitC$pet02 <- ifelse(sitC$pet01 %in% 0,NA,sitC$pet02)
sitC$aet01 <- ifelse(sitC$aet01 %in% 0,NA,sitC$aet01)
sitC$aet02 <- ifelse(sitC$aet01 %in% 0,NA,sitC$aet02)

for (k in unique(avs.ll$fEVI)) {
    wch <- grep(k,colnames(avs.EVI))
    sitC[avs.ll$fEVI %in% k,"evi01"] <- apply(avs.EVI[avs.ll$fEVI %in% k,(wch-22):wch],1,median)
    sitC[avs.ll$fEVI %in% k,"evi02"] <- apply(avs.EVI[avs.ll$fEVI %in% k,(wch-22):wch],1,mad)
    wch <- grep(k,colnames(avs.NDVI))
    sitC[avs.ll$fEVI %in% k,"ndvi01"] <- apply(avs.NDVI[avs.ll$fEVI %in% k,(wch-22):wch],1,median)
    sitC[avs.ll$fEVI %in% k,"ndvi02"] <- apply(avs.NDVI[avs.ll$fEVI %in% k,(wch-22):wch],1,mad)
}


for (k in unique(avs.ll$fLST)) {
    wch <- grep(k,colnames(avs.LSTn))
    sitC[avs.ll$fLST %in% k,"nT01"] <- apply(avs.LSTn[avs.ll$fLST %in% k,(wch-47):wch],1,median,na.rm=T)
    sitC[avs.ll$fLST %in% k,"nT02"] <- apply(avs.LSTn[avs.ll$fLST %in% k,(wch-47):wch],1,mad,na.rm=T)

    wch <- grep(k,colnames(avs.LSTd))
    sitC[avs.ll$fLST %in% k,"dT01"] <- apply(avs.LSTd[avs.ll$fLST %in% k,(wch-47):wch],1,median,na.rm=T)
    sitC[avs.ll$fLST %in% k,"dT02"] <- apply(avs.LSTd[avs.ll$fLST %in% k,(wch-47):wch],1,mad,na.rm=T)

    wch <- grep(k,colnames(avs.Lai))
    sitC[avs.ll$fLST %in% k,"Lai01"] <- apply(avs.Lai[avs.ll$fLST %in% k,(wch-47):wch],1,median,na.rm=T)
    sitC[avs.ll$fLST %in% k,"Lai02"] <- apply(avs.Lai[avs.ll$fLST %in% k,(wch-47):wch],1,mad,na.rm=T)

    wch <- grep(k,colnames(avs.Fpar))
    sitC[avs.ll$fLST %in% k,"Fpar01"] <- apply(avs.Fpar[avs.ll$fLST %in% k,(wch-47):wch],1,median,na.rm=T)
    sitC[avs.ll$fLST %in% k,"Fpar02"] <- apply(avs.Fpar[avs.ll$fLST %in% k,(wch-47):wch],1,mad,na.rm=T)

}
}


###################################################
### code chunk number 13: sitCV
###################################################

if (!exists("sitCV")) {
    
    colSums(is.na(sitC))
    stNM <- sitC
    fiX <- avs.ll$NM
    na.fix <- rfImpute(sitC[,-1],factor(fiX))
    mus <- apply(na.fix[,-1],2,mean,na.rm=T)
    sgs <- apply(na.fix[,-1],2,sd,na.rm=T)
    sitCV <- sitC
    for (nn in names(mus)) {
        sitCV[,nn] <-     (na.fix[,nn]-mus[nn])/sgs[nn]
    }
}

if (!exists("sVars")) {
    mVars <- stack(pre01,dT01,pet01,evi01)
    names(mVars) <- c("pre01","dT01","pet01","evi01")
    sVars <- mVars
    for (nn in names(sVars)) {
        values(sVars)[,nn] <-     (values(mVars)[,nn]-mus[nn])/sgs[nn]
    }
}



###################################################
### code chunk number 14: deteccionMatrix
###################################################
if(!exists("Pion_barr.mtz")) {

generos <- c("Amazona","Ara","Aratinga","Bolborhynchus","Brotogeris","Deroptyus","Diopsittaca","Eupsittula","Forpus","Hapalopsittaca","Nannopsittaca","Orthopsittaca","Pionites","Pionus","Psilopsiagon","Psittacara","Pionopsitta","Pyrrhura","Touit")
lst.spp <- c()
for (gg in generos)
    lst.spp <- unique(c(lst.spp,grep (gg,spp.aves$Latname)))

lst.spp <- lst.spp[sapply(spp.aves[lst.spp,"Latname"],function(x) strsplit(x," ")[[1]][1]) %in% generos]

acdgs <- unique(c(NM.m1$Especieid[NM.m1$Especieid %in% spp.aves[lst.spp,"N_ave"]],NM.m2$Especieid[NM.m2$Especieid %in% spp.aves[lst.spp,"N_ave"]]))

nmbrs <- c()
for (acdg in acdgs) {
    mtc <- NM.m1$Especieid %in% acdg
    mt1 <- tapply(mtc,list(factor(NM.m1$idpunto,levels=muestreos$V3)),max)##[,2,drop=F]
    mtc <- NM.m2$Especieid %in% acdg
    mt2 <- tapply(mtc,list(NM.m2$idpunto,NM.m2$Lapso),max)
    mt2[is.na(mt2)] <- 0
    mtz <- merge(mt1,mt2,by="row.names",all=T)

    
    colnames(mtz) <- c("idpunto","M1","L1","L2","L3")
    mtz$M1[is.na(mtz$M1)] <- 0
    mtz$L2[mtz$L1 %in% 1] <- NA
    mtz$L3[mtz$L1 %in% 1 | mtz$L2 %in% 1] <- NA

    mtz <- mtz[match(avs.ll$V3,mtz$idpunto),]

    assign(sprintf("%s.mtz",
                   sapply(subset(spp.aves,N_ave %in% acdg)$Latname,function(x) {
                       y <- strsplit(x," ")[[1]]
                       paste(substr(y[1],1,4),substr(y[2],1,4),sep="_")
                   })),
           ##abbreviate(subset(spp.aves,N_ave %in% acdg)$Latname)),
           mtz)
    nmbrs <- c(nmbrs,subset(spp.aves,N_ave %in% acdg)$Latname)
}
}


###################################################
### code chunk number 15: ListMatrices
###################################################

for (k in ls(pattern="\\.mtz")) {

    rsm.loros[match(gsub(".mtz","",k),rsm.loros$aspp),"NM.M1"] <- sum(get(k)[,2],na.rm=T)
    rsm.loros[match(gsub(".mtz","",k),rsm.loros$aspp),"NM.L1"] <- sum(get(k)[,3],na.rm=T)
    rsm.loros[match(gsub(".mtz","",k),rsm.loros$aspp),"NM.L2"] <- sum(get(k)[,4],na.rm=T)
    rsm.loros[match(gsub(".mtz","",k),rsm.loros$aspp),"NM.L3"] <- sum(get(k)[,5],na.rm=T)
}
##rsm.loros[,-1]



###################################################
### code chunk number 16: FitModels
###################################################
lst.spp <- c("Amaz_amaz", "Amaz_barb", "Amaz_fari", "Amaz_ochr", "Ara_chlo",
             "Ara_maca", "Ara_mili", "Ara_seve", "Arat_acut", "Arat_leuc",
             "Arat_pert", "Arat_wagl", "Brot_chry", "Brot_jugu", "Diop_nobi",
             "Forp_pass", "Nann_pany", "Orth_mani", "Pion_barr", "Pion_mela",
             "Pion_mens", "Pyrr_egre", "Pyrr_hoem", "Pyrr_mela", "Pyrr_pict",
             "Pyrr_rhod")


##lst.spp[!lst.spp %in% mexp$aspp]

no.removal <- F
for (spp in lst.spp) {
    
    if(!exists(sprintf("%s.fL",spp))) {
        mtz <- get(sprintf("%s.mtz",spp))
        if (no.removal) {
            ## alternativa sin removal design
            mtz <- cbind(mtz[,2],rowSums(mtz[,3:5],na.rm=T))
            ##obs <- cbind(obs1[,2:3])
            obs <- cbind(obsC[,2],rowMeans(obsC[,3:5],na.rm=F)) ## ya deben tener el mismo orden segun avs.ll
            mtz[is.na(obs)] <- NA
            UMF <- unmarkedFrameOccu(mtz,
                                     siteCovs=sitCV,
                                     ##siteCovs=muestreos,
                                     obsCovs=list(hora=obs,
                                         dur=col(obs)))
            UMF@obsCovs$dur <- c(3,9)[UMF@obsCovs$dur]
        ## seleccionar solo los puntos dentro del área de distribución esperada
            os <- seq(along=avs.ll$NM)[avs.ll$NM %in% unique(c(avs.ll[rowSums(mtz,na.rm=T)>0,"NM"], as.character(mexp[mexp$aspp %in% tolower(spp),"NM"]))) & !is.na(mtz[,1])]
            ##os <- seq(along=muestreos$NM)[muestreos$NM %in% unique(c(muestreos[rowSums(mtz,na.rm=T)>0,"NM"], as.character(mexp[mexp$aspp %in% tolower(spp),"NM"]))) & !is.na(mtz[,1])]

        } else {

            mtz[is.na(obsC)] <- NA
            UMF <- unmarkedFrameOccu(mtz[,-1],
                                     siteCovs=sitCV,
                                     obsCovs=list(hora=obsC[,-1]))
        ## seleccionar solo los puntos dentro del área de distribución esperada
            os <- seq(along=avs.ll$NM)[
                avs.ll$NM %in% unique(c(avs.ll@data[rowSums(mtz[,-1],na.rm=T)>0,"NM"], as.character(mexp[tolower(mexp$aspp) %in% tolower(spp),"NM"]))) & !is.na(mtz$M1)]

        }

        
        ##        fm <- occu(~ hora ~ LSTd, UMF[ssO,])
        ##        os <- fm@sitesRemoved
        
        fL <- list("nulo"=try(occu(~ 1 ~ 1, UMF[os,])),
                   "p(h)Psi(.)"=try(occu(~ hora ~ 1, UMF[os,])),
                   "p(.)Psi(V)"=try(occu(~ 1 ~ evi01+I(evi01^2), UMF[os,])),
                   "p(h)Psi(V)"=try(occu(~ hora ~ evi01+I(evi01^2) , UMF[os,])),
                   "p(.)Psi(C)"=try(occu(~ 1 ~ pet01+I(pet01^2) + dT01+I(dT01^2) + pre01+I(pre01^2), UMF[os,])),
                   "p(h)Psi(C)"=try(occu(~ hora ~ pet01+I(pet01^2) + dT01+I(dT01^2) + pre01+I(pre01^2), UMF[os,])),

                   "p(.)Psi(VC)"=try(occu(~ 1 ~ evi01+I(evi01^2) + pet01+I(pet01^2) + dT01+I(dT01^2) + pre01+I(pre01^2), UMF[os,])),
                   "p(h)Psi(VC)"=try(occu(~ hora ~ evi01+I(evi01^2) + pet01+I(pet01^2) + dT01+I(dT01^2) + pre01+I(pre01^2), UMF[os,]))
)
        
        qq <- sapply(fL,function(x) {
                         if (any(class(x) %in% "try-error")) {
                             return(FALSE)
                         } else {
                             return(x@opt$convergence==0 & sum(abs(coef(x))>8)<2)
                         }
                     })
        
        assign(sprintf("%s.fL",spp),fL[qq])
        if (any(qq)) {
            ms.tab <- aictab(fL[qq],names(fL)[qq])
            assign(sprintf("%s.ms",spp),ms.tab)
        }
     }
}




###################################################
### code chunk number 17: rsltsMS
###################################################

## ls(pattern="\\.ms$")
if (!exists("rslts")) {
    rslts <- data.frame()
    for (gg in ls(pattern="\\.ms$")) {
        ms.tab <- get(gg)
        rslts <- rbind(rslts,
                       data.frame(spp=sub(".ms","",gg),mod=ms.tab$Modnames,
                                  n=nrow(get(sub(".ms",".fL",gg))[[1]]@data@y),
                                  dtt= sum(rowSums(get(sub(".ms",".fL",gg))[[1]]@data@y,na.rm=T)>0),
                                  AICc=ms.tab$AICc,
                                  Delta.AICc=round(ms.tab$Delta_AICc,2),
                                  AICw=round(ms.tab$AICcWt,3),
                                  LL=round(ms.tab$LL,2)))
    }
}
##with(rslts,aggregate(AICw,list(mod),sum))
##with(rslts,aggregate(AICw,list(mod),function(x) sum(!is.na(x))))
##with(rslts,aggregate(AICw,list(spp),function(x) sum(!is.na(x))))

##
##lst.spp[!lst.spp %in% rslts$spp]




###################################################
### code chunk number 18: tablaAIC
###################################################
if (!exists("tabla.AIC")) {
    ##round(with(rslts,tapply(AICw,list(spp,mod),sum)),3)
    tabla.AIC <- round(with(rslts,
                            tapply(AICw,
                                   list(spp,
                                        factor(mod,
                                           levels=c("nulo","p(h)Psi(.)",
                                               "p(.)Psi(C)","p(h)Psi(C)",
                                               "p(.)Psi(V)","p(h)Psi(V)",
                                               "p(.)Psi(VC)","p(h)Psi(VC)")))
                                  ,sum)),3)
    
    
    for (gg in ls(pattern="\\.ms$")) {
        fL <- get(sub(".ms",".fL",gg))
        nwdt <- data.frame(hora=seq(0.25,1,length=60),dur=3)
        prds <- modavgPred(fL,names(fL),newdata=nwdt,
                           type = "link", parm.type = "detect")
        
        pmtz <- data.frame(lci=boot::inv.logit(prds$mod.avg.pred-(prds$uncond.se*1.96)),
                           mu=boot::inv.logit(prds$mod.avg.pred),
                           uci=boot::inv.logit(prds$mod.avg.pred+(prds$uncond.se*1.96)))
        assign(sub(".ms",".pmtz",gg),pmtz)
    }
}




###################################################
### code chunk number 19: Grafico1 (eval = FALSE)
###################################################
## 
## layout(matrix(1:10,ncol=2))
## par(mar=c(5,4,1,1))
## 
## ## rapid decay
## matplot(nwdt$hora,Amaz_amaz.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Pion_mens.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Arat_leuc.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Ara_mili.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## 
## ## nearly constant
## matplot(nwdt$hora,Arat_pert.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Forp_pass.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Ara_maca.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Brot_jugu.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## ## increasing
## matplot(nwdt$hora,Pyrr_pict.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## 
## 


###################################################
### code chunk number 20: Grafico2 (eval = FALSE)
###################################################
## 
## layout(matrix(1:10,ncol=2))
## matplot(nwdt$hora,Diop_nobi.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## 
## matplot(nwdt$hora,Orth_mani.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Pion_barr.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Pyrr_mela.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## 
## matplot(nwdt$hora,Arat_wagl.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Pyrr_egre.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Pyrr_rhod.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## 
## ## very low
## matplot(nwdt$hora,Amaz_fari.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## matplot(nwdt$hora,Ara_chlo.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## 


###################################################
### code chunk number 21: Documento1_PsittacidaeVenezuelaNM.Rnw:695-734
###################################################

if (!exists("dprob")) {
    rowSums(tabla.AIC[,grep("h",colnames(tabla.AIC))],na.rm=T)
    
    
    ##
    tt <- subset(rslts,Delta.AICc==0)
table(tt$mod)
    
    fq <- matrix(nrow=nrow(tt),ncol=4)
for (k in 1:nrow(tt)) {
    fL <- get(sprintf("%s.fL",tt[k,"spp"]))
    re <- ranef(fL[[as.character(tt[k,"mod"])]])
    fq[k,] <-  table(cut(bup(re, stat="mean"),c(-1,.05,.5,.95,2)))
    
}

dprob <- data.frame()
for (k in 1:nrow(tt)) {
    fL <- get(sprintf("%s.fL",tt[k,"spp"]))
    mtz <- fL[[as.character(tt[k,"mod"])]]@data@y
    miP <- getP(fL[[as.character(tt[k,"mod"])]])
    miP[is.na(mtz)] <- NA
    p.aus <- apply(1-miP,1,prod,na.rm=T)
    y <- rowSums(mtz,na.rm=T)
    pre.Psi <- predict(fL[[as.character(tt[k,"mod"])]],"state")[,1]
    post.Psi <- (pre.Psi*p.aus)/((pre.Psi*p.aus) + (1-pre.Psi))
    dprob <- rbind(dprob,data.frame(spp=tt[k,"spp"],
                                    pre.Psi=pre.Psi[y==0],
                                    p.aus=p.aus[y==0],
                                    post.Psi=post.Psi[y==0]))
    
}

mprob <- with(dprob,aggregate(post.Psi,by=list(spp),median,na.rm=T))
mprob$o <- order(mprob$x)
dprob <- dprob[order(dprob$spp),]
dprob$spp <- factor(dprob$spp,levels=levels(dprob$spp)[mprob$o])
}


###################################################
### code chunk number 22: Figuras3 (eval = FALSE)
###################################################
## ##png(file="Psi_previo_y_post.png",bg="aliceblue",width=700,height=500,pointsize=14)
## ##para 82 milimetros (una columna): 82*0.03937*600
## ##para 2/3 de columan es 2800...
## tiff(filename = "FerrerSanchez_Figure2.tiff", width = 2800, height = 2800*.84, units = "px", pointsize = 64, compression = c("jpeg"), bg = "white")
## 
## layout(1:2)
## par(mar=c(0.5,5,0.5,0.5),oma=c(10,0,0,0),lwd=4)
## boxplot((1-p.aus)~spp,dprob,las=2,ylim=c(0,1),axes=F,ylab="p*",xlim=c(5,17))##ylab=expression(p==(1-Pi[j=1]^K (1-p_j))))
## box()
## abline(h=c(0.10),lty=3)
## axis(2)
## mtext("(a)",2,line=3,las=2,at=c(3,.95),cex=1.8)
## 
## ##boxplot(pre.Psi~spp,dprob,las=2,ylim=c(0,1),axes=F,ylab=expression(hat(Psi)))
## ##axis(2)
## ##abline(h=c(0.2),lty=3)
## ##box()
## boxplot(post.Psi~spp,dprob,las=2,ylim=c(0,1),ylab=expression(Psi[condl]),axes=F,xlim=c(5,17))
## axis(1,1:25,as.character(mexp$spp[match(levels(dprob$spp),mexp$aspp)]),font=3,las=2,cex.axis=.85)
## abline(h=c(0.10),lty=3)
## axis(2)
## box()
## mtext("(b)",2,line=3,las=2,at=c(3,.95),cex=1.8)
## 
## dev.off()


###################################################
### code chunk number 23: Documento1_PsittacidaeVenezuelaNM.Rnw:767-797 (eval = FALSE)
###################################################
## 
## sim.prob <- data.frame()
## for (k in 1:nrow(tt)) {
##     fL <- get(sprintf("%s.fL",tt[k,"spp"]))
##     y <- rowSums(fL[[as.character(tt[k,"mod"])]]@data@y,na.rm=T)
##     pre.Psi <- predict(fL[[as.character(tt[k,"mod"])]],"state")[,1]
## 
##     maxcol <- 8
##     for (maxhr in c(9:12)) {
##         nwdt <- data.frame(hora=runif(length(y)*maxcol,5.5/24,maxhr/24))
##         m0 <-matrix(predict(fL[[as.character(tt[k,"mod"])]],"det",
##                             newdata=nwdt)$Predicted,ncol=maxcol,byrow=T)
##         for (ncol in 2:maxcol) {
##             p.new <- apply(1-m0[,1:ncol],1,prod)
##             new.Psi <- (pre.Psi*p.new)/(pre.Psi*p.new + (1-pre.Psi))
##             sim.prob <- rbind(sim.prob,data.frame(spp=tt[k,"spp"],
##                                                   hora.max=maxhr,
##                                                   n.mst=ncol,
##                                                   post.Psi=new.Psi[y==0]))
##         }
##     }
## }
## sim.prob <- rbind(sim.prob,data.frame(spp=dprob$spp,
##                        hora.max=0,
##                        n.mst=0,
##                        post.Psi=dprob$post.Psi))
## boxplot(post.Psi~sprintf("%02d %02d",hora.max,n.mst),sim.prob,las=2)
## abline(h=0.05,lty=3)
## abline(h=0.1425,lty=3)
## 


###################################################
### code chunk number 24: Documento1_PsittacidaeVenezuelaNM.Rnw:801-834 (eval = FALSE)
###################################################
## 
## ##
## mm <- table(mexp$aspp)
## mm[!names(mm) %in% tt$spp]
## 
## with(subset(mexp,!aspp %in% tt$spp),aggregate(freq,list(spp=spp),sum))
## 
## 
## 
## ## el muestreo actual representa 1.6 dias de muestreo
## 
## 
## 
## 
## ##    matrix(,ncol=8,byrow=T)
## ##                       fL[[as.character(tt[k,"mod"])]]@data@obsCovs)
##     s1 <- seq(1,nrow(nwdt),by=4)
##     s2 <- seq(2,nrow(nwdt),by=4)
##     s3 <- seq(3,nrow(nwdt),by=4)
##     s4 <- seq(4,nrow(nwdt),by=4)
##     h1 <- nwdt$hora[s1]
##     h2 <- nwdt$hora[-seq(1,nrow(nwdt),by=4)]
##     nwdt$hora <- NA
##     nwdt$hora[s1] <- h1
##     nwdt$hora[s2] <- rev(h1)
##     nwdt$hora[s3] <- h1
##     nwdt$hora[s4] <- rev(h1)
## 
## 
##         post.Psi <- (pre.Psi*p.aus)/(pre.Psi*p.aus + (1-pre.Psi))
## boxplot(post.Psi~spp,dprob,las=2,ylim=c(0,1))
## abline(h=0.05,lty=3)
## 


###################################################
### code chunk number 25: Documento1_PsittacidaeVenezuelaNM.Rnw:838-867 (eval = FALSE)
###################################################
## 
##  sum(dprob$post.Psi < 0.05,na.rm=T)
## sum(dprob$new.Psi[!is.na(dprob$new.Psi)] < 0.05,na.rm=T)
## 
## 
## 
## cbind(tt[,1:4],fq)
## boxplot(bup(re, stat="mean")~Amaz_barb.fL[[1]]@data@siteCovs$NM)
## abline(h=0.05)
## 
## table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] ) ##95% CI
## 
## subset(rslts,Delta.AICc==0 & grepl("V",mod))
## 
## vWC <- data.frame(values(mVars))
## ##colnames(vWC) <- sprintf("wbio%02d",1:19)
## ss <- rowSums(is.na(vWC))==0
## 
## mu <- mean(muestreos[,"LST_Day_1km"],na.rm=T)
## sg <- sd(muestreos[,"LST_Day_1km"],na.rm=T)
## vWC[,"LSTd"] <- (vWC[,"LSTd"]-mu)/sg
## mu <- mean(muestreos[,"PET_1km"],na.rm=T)
## sg <- sd(muestreos[,"PET_1km"],na.rm=T)
## vWC[,"PET"] <- (vWC[,"PET"]-mu)/sg
## mu <- mean(muestreos[,"v250m_16_days_EVI"],na.rm=T)
## sg <- sd(muestreos[,"v250m_16_days_EVI"],na.rm=T)
## vWC[,"EVI"] <- (vWC[,"EVI"]-mu)/sg
## 
## 


###################################################
### code chunk number 26: Documento1_PsittacidaeVenezuelaNM.Rnw:870-895 (eval = FALSE)
###################################################
## 
## for (mdl in c("p(.)Psi(VC)","p(h)Psi(VC)")) {
##     ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
##     for (k in 1:nrow(ssr)) {
##         psi <- raster(mVars,1)*NA
##         ##values(psi)[ss] <- prdPsi$Predicted
##         fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
##         values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1,EVI,EVI^2,PET,PET^2,LSTd,LSTd^2))))
##         assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
##     }
## }
## 
## 
## for (mdl in c("p(.)Psi(V)","p(h)Psi(V)")) {
##     ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
##     for (k in 1:nrow(ssr)) {
##         psi <- raster(mVars,1)*NA
##         ##values(psi)[ss] <- prdPsi$Predicted
##         fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
##         values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1,EVI,EVI^2))))
##         assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
##     }
## }
## 
## 


###################################################
### code chunk number 27: Documento1_PsittacidaeVenezuelaNM.Rnw:899-913 (eval = FALSE)
###################################################
## 
## for (mdl in c("nulo","p(h)Psi(.)")) {
##     ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
##     for (k in 1:nrow(ssr)) {
##         psi <- raster(mVars,1)*NA
##         ##values(psi)[ss] <- prdPsi$Predicted
##         fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
##         values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1))))
##         assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
##     }
## }
## 
## ls(pattern="Psi")
## 


###################################################
### code chunk number 28: Documento1_PsittacidaeVenezuelaNM.Rnw:919-927 (eval = FALSE)
###################################################
## 
## re <- ranef(Amaz_amaz.fL[["p(h)Psi(T)"]])
##  ## Extract all values in convenient formats
## post.df <- as(re, "data.frame")
## head(post.df)
## post.arr <- as(re, "array")
##      
## 


###################################################
### code chunk number 29: Documento1_PsittacidaeVenezuelaNM.Rnw:931-956 (eval = FALSE)
###################################################
## 
## ## Best Unbiased Predictors
## ##bup(re, stat="mean")           # Posterior mean
## table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] ) ##95% CI
## 
## (re <- ranef(Amaz_barb.fL[["p(h)Psi(T)"]]))
##      # Best Unbiased Predictors
##      bup(re, stat="mean")           # Posterior mean
## 
## 
##  re <- ranef(Amaz_ochr.fL[["p(.)Psi(VT)"]])
## ## Best Unbiased Predictors
## ##   bup(re, stat="mean")           # Posterior mean
## 
## table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] ) ##95% CI
## 
##   (re <- ranef(Amaz_barb.fL[["p(h)Psi(T)"]]))
##      # Best Unbiased Predictors
##      bup(re, stat="mean")           # Posterior mean
## 
## table(bup(re, stat="mean")> 0.95)
## table(bup(re, stat="mean")< 0.05)
## boxplot(bup(re, stat="mean")~Amaz_barb.fL[[1]]@data@siteCovs$NM)
## abline(h=0.05)
## 


###################################################
### code chunk number 30: Documento1_PsittacidaeVenezuelaNM.Rnw:959-986 (eval = FALSE)
###################################################
## 
## table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] )## 95% CI
## 
##     
## for (spps in c("Amaz_amaz","Amaz_ochr","Ara_seve","Arat_pert","Forp_pass","Pion_mens","Brot_jugu","Amaz_fari","Pyrr_pict"))
##     print(sprintf("%s.Psi <- %s",spps,with(subset(rslts,spp %in% spps & AICw>0.001),paste(sprintf("(%s.%s * %s)",spp,gsub("[()]","",mod),AICw),collapse="+"))))
## 
## Amaz_amaz.Psi <- (Amaz_amaz.phPsiT * 0.592)+(Amaz_amaz.phPsiVT * 0.408)
## Amaz_ochr.Psi <- (Amaz_ochr.p.PsiVT * 0.968)+(Amaz_ochr.p.PsiT * 0.031)
## Ara_seve.Psi <- (Ara_seve.nulo * 0.724)+(Ara_seve.p.PsiV * 0.276)
## Arat_pert.Psi <- (Arat_pert.p.PsiVT * 0.632)+(Arat_pert.phPsiVT * 0.251)+(Arat_pert.p.PsiT * 0.084)+(Arat_pert.phPsiT * 0.032)
## Forp_pass.Psi <- (Forp_pass.p.PsiT * 0.417)+(Forp_pass.phPsiT * 0.279)+(Forp_pass.p.PsiVT * 0.087)+(Forp_pass.nulo * 0.069)+(Forp_pass.phPsiVT * 0.057)+(Forp_pass.phPsi. * 0.051)+(Forp_pass.p.PsiV * 0.025)+(Forp_pass.phPsiV * 0.015)
## Pion_mens.Psi <- (Pion_mens.phPsi. * 0.801)+(Pion_mens.phPsiV * 0.125)+(Pion_mens.phPsiT * 0.075)
## Brot_jugu.Psi <- (Brot_jugu.p.PsiV * 0.377)+(Brot_jugu.p.PsiVT * 0.175)+(Brot_jugu.p.PsiT * 0.166)+(Brot_jugu.phPsiV * 0.14)+(Brot_jugu.phPsiVT * 0.061)+(Brot_jugu.phPsiT * 0.059)+(Brot_jugu.nulo * 0.016)+(Brot_jugu.phPsi. * 0.006)
## Amaz_fari.Psi <- (Amaz_fari.p.PsiT * 0.774)+(Amaz_fari.nulo * 0.133)+(Amaz_fari.p.PsiVT * 0.093)
## Pyrr_pict.Psi <- (Pyrr_pict.p.PsiV * 0.668)+(Pyrr_pict.phPsiV * 0.332)
## 
## 
## subset(rslts,spp %in% "Arat_pert" & AICw>0.001)
## 
## 
## png(file="Aratinga_pertinax_psi.png")
## plot(Arat_pert.Psi,main="Eupsittula pertinax")
## points(muestreos[rowSums(Arat_pert.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
## points(muestreos[rowSums(Arat_pert.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## dev.off()
## 


###################################################
### code chunk number 31: Documento1_PsittacidaeVenezuelaNM.Rnw:990-1011 (eval = FALSE)
###################################################
## 
## png(file="Ara_severus_psi.png")
## plot(Ara_seve.Psi,main="Ara severus")
## points(muestreos[rowSums(Ara_seve.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
## points(muestreos[rowSums(Ara_seve.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## shp <- shapefile("~/gisdata/distribuciones/BirdLife/Ara_severus_22685577.shp")
## plot(shp,add=T)
## dev.off()
## 
## png(file="Amazona_amazonica_psi.png")
## plot(Amaz_amaz.Psi,main="Amazona amazonica",col=brewer.pal(9,"Greens"))
## points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
## points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## dev.off()
## 
## png(file="Amazona_farinosa_psi.png")
## plot(Amaz_fari.Psi,main="Amazona farinosa",col=brewer.pal(9,"Greens"))
## points(muestreos[rowSums(Amaz_fari.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
## points(muestreos[rowSums(Amaz_fari.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## dev.off()
## 


###################################################
### code chunk number 32: Documento1_PsittacidaeVenezuelaNM.Rnw:1014-1069 (eval = FALSE)
###################################################
## shp <- shapefile("~/gisdata/distribuciones/BirdLife/Amazona_ochrocephala_22686346.shp")
## 
## png(file="Amazona_ochrocephala_psi.png",width=500,height=400)
## plot(Amaz_ochr.Psi,main="Amazona ochrocephala",col=brewer.pal(9,"Spectral"),ylim=c(0,13))
## plot(vz0,add=T,border="grey33")
## points(muestreos[rowSums(Amaz_ochr.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=1,pch=19,cex=.5)
## points(muestreos[rowSums(Amaz_ochr.mtz[,-1],na.rm=T)>0,c("V2","V1")],col="purple",pch=3)
## ##plot(shp,add=T)
## 
## dev.off()
## 
## shp <- shapefile("~/gisdata/distribuciones/BirdLife/Pionus_menstruus_45429607.shp")
## 
## png(file="Pionus_menstruus_psi.png",width=900,height=440,pointsize=18)
## layout(matrix(1:2,ncol=2))
## par(mar=c(1,0,1,5),oma=c(0,0,3,0))
## plot(vz0,ylim=c(0,13),border="darkgreen",main="IUCN, 2014")
## plot(shp,col=rgb(.1,.1,.9,.7),add=T)
## points(aves[aves@data$species %in% "Pionus menstruus",],col=2,pch=.7)
## 
## plot(Pion_mens.Psi,col=brewer.pal(9,"Greens"),
##      ##breaks=round(seq(0,1,length=10),2),
##      ylim=c(0,13),axes=F,main="NeoMapas + Occupancy model")
## plot(vz0,add=T)
## points(muestreos[rowSums(Pion_mens.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
## points(muestreos[rowSums(Pion_mens.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## 
## 
## mtext("Pionus menstruus",3,cex=2.7,outer=T,font=3)
## dev.off()
## 
## 
## dev.off()
## 
## png(file="Brotogeris_jugularis_psi.png")
## 
## plot(Brot_jugu.Psi,main="Brotogeris jugularis",col=brewer.pal(9,"Greens"))
## points(muestreos[rowSums(Brot_jugu.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
## points(muestreos[rowSums(Brot_jugu.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## shp <- shapefile(sprintf("%s/%s",SEDAC.mptc,"brot_jugu_pl.shp"))
## plot(shp,add=T)
## 
## dev.off()
## 
## 
## 
## rowSums(tabla.AIC[,grep("h",colnames(tabla.AIC))],na.rm=T)
## rowSums(tabla.AIC[,grep("V",colnames(tabla.AIC))],na.rm=T)
## rowSums(tabla.AIC[,grep("T",colnames(tabla.AIC))],na.rm=T)
## 
## 
## ### para ver si la falta de detección provee información confiable sobre su ausencia hay que calcular para cada punto (según hora etc...) estos valores
## 
## 
## 


###################################################
### code chunk number 33: Documento1_PsittacidaeVenezuelaNM.Rnw:1073-1131 (eval = FALSE)
###################################################
## 
## ## lista
## ## no-detectado vs detectado
## ## no-detectado: no esperado (escaso o distribución restringida) vs esperado
## ## esperado: baja detectabilidad o ausente
## ## detectado: abundante/común vs. escaso o restringido 
## ## restringido por clima o vegetación
## 
## 
## ## probabilidad de detección después de varias visitas a un sitio:
## ##Probabilidad de no detección
## ##gP <- getP(Arat_pert.fL[[3]])
## gP <- obs1[,-1]^0 * 0.3
## gP[is.na(obs1[,-1])] <- NA
## ##gP <- getP(Arat_pert.fL[["p(.)Psi(VT)"]])
## ##gP[is.na(obs1[os,-1])] <- NA
## preF <- apply(gP,1,function(x) prod(1-x,na.rm=T))
## 
## ## si estamos 25% seguros de la presencia...
## prePsi <- 0.5
## 
## postPsi <- prePsi*preF/(prePsi*preF + (1-prePsi))
## boxplot(postPsi)
## 
## 
## tapply(mexp$NM,mexp$spp,luq)
## 
## boxplot(with(subset(rslts,mod %in% "nulo"),dtt/n))
## 
## shp <- shapefile("~/gisdata/distribuciones/BirdLife/Pyrilia_caica_22686136.shp")
## plot(vz0,border="grey33")
## plot(shp,add=T)
## shp <- shapefile("~/gisdata/distribuciones/BirdLife/Pionus_fuscus_22686198.shp")
## plot(shp,add=T)
## shp <- shapefile("~/gisdata/distribuciones/BirdLife/Deroptyus_accipitrinus_22686416.shp")
## plot(shp,add=T)
## 
## res <- data.frame()
## for (j in 1:nrow(rslts)) {
##     fL <- get(sprintf("%s.fL",rslts[j,"spp"]))
##     mdl <- fL[[as.character(rslts[j,"mod"])]]
##     re <- ranef(mdl)
##     ci <- confint(re, level=0.95)
##     res <- rbind(res,data.frame(
##                          spp=rslts[j,"spp"],
##                          mod=rslts[j,"mod"],
##                          aus=sum(ci[,1]==0 & ci[,2]==0 ),
##                          q.aus=sum(ci[,1]==0 & ci[,2]==1 ),
##                          pres=sum(ci[,1]==1 & ci[,2]==1 )))
## 
## 
## }
## bst.res <- cbind(res[rslts$Delta.AICc==0,],rslts[rslts$Delta.AICc==0,c("n","dtt")])
## write.csv(file="TablaDeteccionesPrediccionPsittacidae.csv",bst.res)
## write.csv(file="TablaModelosPsittacidae.csv",rslts)
## sum(bst.res$aus)/sum(bst.res$n-bst.res$dtt)
## 
## 


###################################################
### code chunk number 34: FigNotDetected (eval = FALSE)
###################################################
## qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
## if(grepl("Touit",k))
##     qry <- gsub(" ","_",gsub("a$","us",k))
## slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
##                         qry),
##            full.names=T)
## if (length(slc)==0) {
##     slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
## }
##     
## 
## s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
## s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
## 
## shp <- crop(shapefile(slc),e)
## par(mar=c(2,2,0,0))
## plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
## axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
## axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
## plot(shp,add=T,lwd=2,col="pink")
## plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
## text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)
## 
## plot(vz0,add=T,border="grey13")
## 
## points(s10,col=2,pch=19,cex=.5)
## points(s11,col=4,pch=19,cex=.5)
## 


###################################################
### code chunk number 35: FigNotDetectedRestricted (eval = FALSE)
###################################################
## slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
## if (length(slc)==0) {
##     slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
## }
##     
## 
## s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
## s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
## 
## #s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
## #s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))
## 
## shp <- crop(shapefile(slc),e)
## par(mar=c(2,2,0,0))
## plot(shp,pty="n")
## axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
## axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
## plot(shp,add=T,lwd=2,col="pink")
## plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
## text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)
## 
## plot(vz0,add=T,border="grey13")
## 
## points(s10,col=2,pch=19,cex=.75)
## points(s11,col=4,pch=19,cex=.75)
## 


###################################################
### code chunk number 36: rsltsSS (eval = FALSE)
###################################################
## if (any(rslts$spp %in% ak))
##     subset(rslts,spp %in% ak)


###################################################
### code chunk number 37: ProbDetect (eval = FALSE)
###################################################
## arch <- sprintf("%s.pmtz",ak)
## ##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
## ##        width=400,height=400,pointsize=16)
## par(mar=c(5,4,1,1))
## matplot(nwdt$hora,get(arch),axes=F,
##         xlab="Hour of the day",ylab="Probability of detection",
##         lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## axis(2)
## axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
## box()
## ##dev.off()
## 


###################################################
### code chunk number 38: SummaryMdl (eval = FALSE)
###################################################
## fL <- get(sprintf("%s.fL",ak))
## mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
## summary(mdl)  


###################################################
### code chunk number 39: phPsiC (eval = FALSE)
###################################################
## mdl <- "p(h)Psi(C)"
## if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
##     psi <- raster(sVars,1)*NA
##     
##     ss <- rowSums(is.na(values(sVars)))==0
##     ##values(psi)[ss] <- prdPsi$Predicted
##     fL <- get(sprintf("%s.fL",ak))
##     values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
##                                                    rbind(1,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
##     assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
## }


###################################################
### code chunk number 40: phPsiVC (eval = FALSE)
###################################################
## mdl <- "p(h)Psi(VC)"
## if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
##     psi <- raster(sVars,1)*NA
##     ss <- rowSums(is.na(values(sVars)))==0
##     ##values(psi)[ss] <- prdPsi$Predicted
##     fL <- get(sprintf("%s.fL",ak))
##     values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
##                                                    rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
##     assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
## }
## 


###################################################
### code chunk number 41: plotPsi (eval = FALSE)
###################################################
## slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
## s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
## s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)
## 
## psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
## shp <- crop(shapefile(slc),e)
## 
## par(mar=c(2,2,0,0))
## plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
## image(psi,breaks=seq(0,1,length=11),
## ##      col=c(NA,grey(seq(.80,0.2,length=9))),
##       col=c(NA,brewer.pal(9,"Oranges")),
##       ylab="",xlab="",axes=F,add=T)
## axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
## axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
## ##plot(shp,add=T,lwd=2,border="white")
## plot(vz0,add=T,border="grey13")
## points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
## points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
## ##points(s10,col=2,pch=19,cex=.5)
## ##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 42: plotPsiCrop (eval = FALSE)
###################################################
## slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
## s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
## s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)
## 
## psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
## shp <- crop(shapefile(slc),e)
## 
## par(mar=c(2,2,0,0))
## plot(crop(vz0,extent(shp)),pty="n")
## image(psi,##breaks=seq(0,1,length=11),
## ##      col=c(NA,grey(seq(.80,0.2,length=9))),
##       col=c(NA,brewer.pal(9,"Oranges")),
##       ylab="",xlab="",axes=F,add=T)
## axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
## axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
## ##plot(shp,add=T,lwd=2,border="white")
## plot(vz0,add=T,border="grey13")
## points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
## points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
## ##points(s10,col=2,pch=19,cex=.5)
## ##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 43: PostPsiNM (eval = FALSE)
###################################################
## fL <- get(sprintf("%s.fL",ak))
## mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
## re <- ranef(mdl)
## prd <- predict(mdl,"state")
## psi.cond <- re@post[,2,1]
## nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]
## 


###################################################
### code chunk number 44: p.PsiVC (eval = FALSE)
###################################################
## mdl <- "p(.)Psi(VC)"
## if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
##     psi <- raster(sVars,1)*NA
##     ss <- rowSums(is.na(values(sVars)))==0
##     ##values(psi)[ss] <- prdPsi$Predicted
##     fL <- get(sprintf("%s.fL",ak))
##     values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
##                                                    rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
##     assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
## }


###################################################
### code chunk number 45: p.PsiV (eval = FALSE)
###################################################
## mdl <- "p(.)Psi(V)"
## if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
##     psi <- raster(sVars,1)*NA
##     ss <- rowSums(is.na(values(sVars)))==0
##     ##values(psi)[ss] <- prdPsi$Predicted
##     fL <- get(sprintf("%s.fL",ak))
##     values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
##                                                    rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2)))
##     assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
## }


###################################################
### code chunk number 46: phPsiV (eval = FALSE)
###################################################
## mdl <- "p(h)Psi(V)"
## if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
##     psi <- raster(sVars,1)*NA
##     ss <- rowSums(is.na(values(sVars)))==0
##     ##values(psi)[ss] <- prdPsi$Predicted
##     fL <- get(sprintf("%s.fL",ak))
##     values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
##                                                    rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2)))
##     assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
## }


###################################################
### code chunk number 47: ExpectedFrom (eval = FALSE)
###################################################
## cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 48: inc100_Amazona.Rnw:7-8
###################################################
subset(rsm.loros,grepl("Amazona ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 49: inc100_Amazona.Rnw:16-18
###################################################
ak <- "Amaz_amaz"
k = "Amazona amazonica"


###################################################
### code chunk number 50: inc100_Amazona.Rnw:23-24
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 51: inc100_Amazona.Rnw:31-32
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 52: inc100_Amazona.Rnw:37-38
###################################################
arch <- sprintf("%s.pmtz",ak)
##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
##        width=400,height=400,pointsize=16)
par(mar=c(5,4,1,1))
matplot(nwdt$hora,get(arch),axes=F,
        xlab="Hour of the day",ylab="Probability of detection",
        lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
axis(2)
axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
box()
##dev.off()



###################################################
### code chunk number 53: inc100_Amazona.Rnw:43-45
###################################################
mdl <- "p(h)Psi(VC)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}

slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
image(psi,breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 54: inc100_Amazona.Rnw:50-54
###################################################
ak <- "Amaz_autu"
k ="Amazona autumnalis"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 55: inc100_Amazona.Rnw:58-59
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 56: inc100_Amazona.Rnw:64-65
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 57: inc100_Amazona.Rnw:72-74
###################################################
ak <- "Amaz_barb"
k = "Amazona barbadensis"


###################################################
### code chunk number 58: inc100_Amazona.Rnw:79-80
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 59: inc100_Amazona.Rnw:86-87
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 60: pAb
###################################################
arch <- sprintf("%s.pmtz",ak)
##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
##        width=400,height=400,pointsize=16)
par(mar=c(5,4,1,1))
matplot(nwdt$hora,get(arch),axes=F,
        xlab="Hour of the day",ylab="Probability of detection",
        lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
axis(2)
axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
box()
##dev.off()



###################################################
### code chunk number 61: inc100_Amazona.Rnw:98-100
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
re <- ranef(mdl)
prd <- predict(mdl,"state")
psi.cond <- re@post[,2,1]
nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]

print(histogram(~psi.cond|nms,type="count",xlab="Posterior probability of presence"))


###################################################
### code chunk number 62: inc100_Amazona.Rnw:104-108
###################################################
ak <- "Amaz_bodi"
k ="Amazona bodini"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 63: inc100_Amazona.Rnw:114-115
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 64: inc100_Amazona.Rnw:119-121
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)




###################################################
### code chunk number 65: inc100_Amazona.Rnw:125-129
###################################################
ak <- "Amaz_dufr"
k ="Amazona dufresniana"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 66: inc100_Amazona.Rnw:135-136
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 67: inc100_Amazona.Rnw:140-142
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 68: inc100_Amazona.Rnw:146-148
###################################################
ak <- "Amaz_fari"
k = "Amazona farinosa"


###################################################
### code chunk number 69: inc100_Amazona.Rnw:152-153
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 70: inc100_Amazona.Rnw:157-158
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 71: inc100_Amazona.Rnw:163-164
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 72: inc100_Amazona.Rnw:170-184
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
re <- ranef(mdl)
prd <- predict(mdl,"state")
psi.cond <- re@post[,2,1]
nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]


layout(matrix(1:4,ncol=2))
for (nn in c("05","18","75","93")) {
    matplot(prd[nms %in% nn,c(1,3,4)],type="l",lty=c(1,3,3),col=1,ylim=c(0,1),main=sprintf("NM%s",nn),ylab="Probability of presence")
    y <- psi.cond[nms %in% nn]
    x <- seq(along=y)
    points(x[y<0.125], y[y<0.125],pch=19,col="red")
    points(x[y>0.125], y[y>0.125],pch=19,col="grey47")
    points(x[y>0.95], y[y>0.95],pch=19,col="blue")

}




###################################################
### code chunk number 73: PsiFigureAfarinosa
###################################################
mdl <- "p(.)Psi(VC)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
image(psi,breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 74: inc100_Amazona.Rnw:198-202
###################################################
ak <- "Amaz_merc"
k ="Amazona mercenarius"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 75: inc100_Amazona.Rnw:206-207
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 76: inc100_Amazona.Rnw:212-214
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 77: inc100_Amazona.Rnw:219-221
###################################################
ak <- "Amaz_ochr"
k = "Amazona ochrocephala"


###################################################
### code chunk number 78: inc100_Amazona.Rnw:226-227
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 79: inc100_Amazona.Rnw:232-233
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 80: inc100_Amazona.Rnw:238-240
###################################################
mdl <- "p(.)Psi(VC)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
image(psi,breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 81: inc101_Ara.Rnw:7-8
###################################################
subset(rsm.loros,grepl("Ara ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 82: inc101_Ara.Rnw:13-17
###################################################
ak <- "Ara_arar"
k ="Ara ararauna"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 83: inc101_Ara.Rnw:21-22
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 84: inc101_Ara.Rnw:26-28
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)




###################################################
### code chunk number 85: inc101_Ara.Rnw:32-34
###################################################
ak <- "Ara_chlo"
k ="Ara chloropterus"


###################################################
### code chunk number 86: inc101_Ara.Rnw:38-39
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 87: inc101_Ara.Rnw:44-45
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 88: inc101_Ara.Rnw:49-50
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 89: inc101_Ara.Rnw:55-57
###################################################
ak <- "Ara_maca"
k ="Ara macao"


###################################################
### code chunk number 90: inc101_Ara.Rnw:62-63
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 91: inc101_Ara.Rnw:68-69
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 92: inc101_Ara.Rnw:74-75
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 93: inc101_Ara.Rnw:80-82
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
re <- ranef(mdl)
prd <- predict(mdl,"state")
psi.cond <- re@post[,2,1]
nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]

hist(psi.cond,xlab=expression(Psi[cond]),freq=T)


###################################################
### code chunk number 94: inc101_Ara.Rnw:88-91
###################################################
ak <- "Ara_mili"
k ="Ara militaris"



###################################################
### code chunk number 95: inc101_Ara.Rnw:95-96
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 96: inc101_Ara.Rnw:100-101
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 97: inc101_Ara.Rnw:106-107
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 98: inc101_Ara.Rnw:112-113
###################################################
arch <- sprintf("%s.pmtz",ak)
##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
##        width=400,height=400,pointsize=16)
par(mar=c(5,4,1,1))
matplot(nwdt$hora,get(arch),axes=F,
        xlab="Hour of the day",ylab="Probability of detection",
        lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
axis(2)
axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
box()
##dev.off()



###################################################
### code chunk number 99: inc101_Ara.Rnw:118-119
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 100: inc101_Ara.Rnw:124-126
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
re <- ranef(mdl)
prd <- predict(mdl,"state")
psi.cond <- re@post[,2,1]
nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]

hist(psi.cond,xlab=expression(Psi[cond]),freq=T)


###################################################
### code chunk number 101: inc101_Ara.Rnw:131-133
###################################################
ak <- "Ara_seve"
k ="Ara severus"


###################################################
### code chunk number 102: inc101_Ara.Rnw:138-139
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 103: inc101_Ara.Rnw:144-145
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 104: inc101_Ara.Rnw:150-151
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 105: inc101_Ara.Rnw:156-158
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
re <- ranef(mdl)
prd <- predict(mdl,"state")
psi.cond <- re@post[,2,1]
nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]

hist(psi.cond,xlab=expression(Psi[cond]),freq=T)


###################################################
### code chunk number 106: inc102_Aratinga.Rnw:6-7
###################################################
subset(rsm.loros,grepl("Aratinga ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 107: inc102_Aratinga.Rnw:11-15
###################################################
ak <- "Arat_sols"
k ="Aratinga solstitialis"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 108: inc102_Aratinga.Rnw:20-21
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 109: inc103_Bolborhynchus.Rnw:6-7
###################################################
subset(rsm.loros,grepl("Bolborhynchus ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 110: inc103_Bolborhynchus.Rnw:12-16
###################################################
ak <- "Bolb_line"
k ="Bolborhynchus lineola"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 111: inc103_Bolborhynchus.Rnw:21-22
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 112: inc103_Bolborhynchus.Rnw:26-27
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 113: inc104_Brotogeris.Rnw:6-7
###################################################
subset(rsm.loros,grepl("Brotogeris ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 114: inc104_Brotogeris.Rnw:11-13
###################################################
ak <- "Brot_chry"
k ="Brotogeris chrysoptera"


###################################################
### code chunk number 115: inc104_Brotogeris.Rnw:16-17
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 116: inc104_Brotogeris.Rnw:21-22
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 117: inc104_Brotogeris.Rnw:27-29
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 118: inc104_Brotogeris.Rnw:35-37
###################################################
ak <- "Brot_cyan"
k ="Brotogeris cyanoptera"


###################################################
### code chunk number 119: inc104_Brotogeris.Rnw:40-41
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 120: inc104_Brotogeris.Rnw:45-46
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 121: Bj
###################################################
ak <- "Brot_jugu"
k ="Brotogeris jugularis"


###################################################
### code chunk number 122: inc104_Brotogeris.Rnw:59-60
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 123: inc104_Brotogeris.Rnw:64-65
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 124: inc104_Brotogeris.Rnw:70-71
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 125: inc104_Brotogeris.Rnw:76-77
###################################################
arch <- sprintf("%s.pmtz",ak)
##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
##        width=400,height=400,pointsize=16)
par(mar=c(5,4,1,1))
matplot(nwdt$hora,get(arch),axes=F,
        xlab="Hour of the day",ylab="Probability of detection",
        lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
axis(2)
axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
box()
##dev.off()



###################################################
### code chunk number 126: inc104_Brotogeris.Rnw:80-81
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 127: inc104_Brotogeris.Rnw:87-89
###################################################
mdl <- "p(h)Psi(C)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(crop(vz0,extent(shp)),pty="n")
image(psi,##breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 128: Dr
###################################################
subset(rsm.loros,grepl("Deroptyus ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 129: inc105_Deroptyus.Rnw:10-14
###################################################
ak <- "Dero_acci"
k ="Deroptyus accipitrinus"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 130: inc105_Deroptyus.Rnw:18-19
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 131: inc105_Deroptyus.Rnw:23-24
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 132: D
###################################################
subset(rsm.loros,grepl("Diopsittaca ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 133: inc106_Diopsittaca.Rnw:10-12
###################################################
ak <- "Diop_nobi"
k ="Diopsittaca nobilis"


###################################################
### code chunk number 134: inc106_Diopsittaca.Rnw:16-17
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 135: inc106_Diopsittaca.Rnw:21-23
###################################################

slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 136: inc106_Diopsittaca.Rnw:30-32
###################################################
##<<rsltsSS>>
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 137: E
###################################################
subset(rsm.loros,grepl("Eupsittula ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 138: inc107_Eupsittula.Rnw:10-12
###################################################
ak <- "Arat_pert"
k ="Eupsittula pertinax"


###################################################
### code chunk number 139: inc107_Eupsittula.Rnw:17-18
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 140: inc107_Eupsittula.Rnw:23-24
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 141: inc107_Eupsittula.Rnw:29-30
###################################################
arch <- sprintf("%s.pmtz",ak)
##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
##        width=400,height=400,pointsize=16)
par(mar=c(5,4,1,1))
matplot(nwdt$hora,get(arch),axes=F,
        xlab="Hour of the day",ylab="Probability of detection",
        lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
axis(2)
axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
box()
##dev.off()



###################################################
### code chunk number 142: inc107_Eupsittula.Rnw:35-36
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 143: inc107_Eupsittula.Rnw:42-44
###################################################
mdl <- "p(.)Psi(VC)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
image(psi,breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 144: F
###################################################
subset(rsm.loros,grepl("Forpus ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 145: Fc
###################################################
ak <- "Forp_cons"
k ="Forpus conspicillatus"


###################################################
### code chunk number 146: inc108_Forpus.Rnw:18-19
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 147: Fm
###################################################
ak <- "Forp_mode"
k ="Forpus modestus"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 148: inc108_Forpus.Rnw:33-34
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 149: inc108_Forpus.Rnw:39-41
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 150: Fp
###################################################
ak <- "Forp_pass"
k ="Forpus passerinus"


###################################################
### code chunk number 151: inc108_Forpus.Rnw:53-54
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 152: inc108_Forpus.Rnw:59-60
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 153: inc108_Forpus.Rnw:65-66
###################################################
arch <- sprintf("%s.pmtz",ak)
##png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
##        width=400,height=400,pointsize=16)
par(mar=c(5,4,1,1))
matplot(nwdt$hora,get(arch),axes=F,
        xlab="Hour of the day",ylab="Probability of detection",
        lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
axis(2)
axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
box()
##dev.off()



###################################################
### code chunk number 154: inc108_Forpus.Rnw:71-72
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 155: inc108_Forpus.Rnw:78-80
###################################################
mdl <- "p(h)Psi(C)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(crop(vz0,extent(shp)),pty="n")
image(psi,##breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 156: H
###################################################
subset(rsm.loros,grepl("Hapalopsittaca ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 157: inc109_Hapalopsittaca.Rnw:10-12
###################################################
ak <- "Hapa_amaz"
k ="Hapalopsittaca amazonina"


###################################################
### code chunk number 158: inc109_Hapalopsittaca.Rnw:16-17
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 159: inc109_Hapalopsittaca.Rnw:21-22
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 160: N
###################################################
subset(rsm.loros,grepl("Nannopsittaca ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 161: inc110_Nanopsittaca.Rnw:10-12
###################################################
ak <- "Nann_pany"
k ="Nannopsittaca panychlora"


###################################################
### code chunk number 162: inc110_Nanopsittaca.Rnw:16-17
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 163: inc110_Nanopsittaca.Rnw:21-22
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 164: inc110_Nanopsittaca.Rnw:27-28
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 165: inc111_Orthopsittaca.Rnw:6-7
###################################################
subset(rsm.loros,grepl("Orthopsittaca ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 166: Om
###################################################
ak <- "Orth_mani"
k ="Orthopsittaca manilatus"


###################################################
### code chunk number 167: inc111_Orthopsittaca.Rnw:17-18
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 168: inc111_Orthopsittaca.Rnw:22-23
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 169: inc111_Orthopsittaca.Rnw:29-30
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 170: inc111_Orthopsittaca.Rnw:35-36
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 171: inc112_Pionites.Rnw:5-6
###################################################
subset(rsm.loros,grepl("Pionites ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 172: Pm
###################################################
ak <- "Pion_mela"
k ="Pionites melanocephala"


###################################################
### code chunk number 173: inc112_Pionites.Rnw:17-18
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 174: inc112_Pionites.Rnw:22-23
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 175: inc112_Pionites.Rnw:28-29
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 176: P
###################################################
subset(rsm.loros,grepl("Pionus ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 177: Ph
###################################################
ak <- "Pion_chal"
k ="Pionus chalcopterus"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 178: inc113_Pionus.Rnw:20-21
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 179: inc113_Pionus.Rnw:26-27
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 180: Pf
###################################################
ak <- "Pion_fusc"
k ="Pionus fuscus"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 181: inc113_Pionus.Rnw:40-41
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 182: inc113_Pionus.Rnw:46-48
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 183: Pm
###################################################
ak <- "Pion_mens"
k ="Pionus menstruus"


###################################################
### code chunk number 184: inc113_Pionus.Rnw:62-63
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 185: inc113_Pionus.Rnw:69-70
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 186: inc113_Pionus.Rnw:75-76
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 187: inc113_Pionus.Rnw:81-83
###################################################
mdl <- "p(h)Psi(VC)"
if (!exists(sprintf("%s.%s",ak,gsub("[()]","",mdl)))) {
    psi <- raster(sVars,1)*NA
    ss <- rowSums(is.na(values(sVars)))==0
    ##values(psi)[ss] <- prdPsi$Predicted
    fL <- get(sprintf("%s.fL",ak))
    values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*
                                                   rbind(1,values(sVars)[ss,"evi01"],values(sVars)[ss,"evi01"]^2,values(sVars)[ss,"pet01"],values(sVars)[ss,"pet01"]^2,values(sVars)[ss,"dT01"],values(sVars)[ss,"dT01"]^2,values(sVars)[ss,"pre01"],values(sVars)[ss,"pre01"]^2)))
    assign(sprintf("%s.%s",ak,gsub("[()]","",mdl)),psi)
}

slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k) & year %in% 2008:2012)

psi <- get(sprintf("%s.%s",ak,gsub("[()]","",mdl)))
shp <- crop(shapefile(slc),e)

par(mar=c(2,2,0,0))
plot(crop(vz0,extent(shp)),pty="n")
image(psi,##breaks=seq(0,1,length=11),
##      col=c(NA,grey(seq(.80,0.2,length=9))),
      col=c(NA,brewer.pal(9,"Oranges")),
      ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
##plot(shp,add=T,lwd=2,border="white")
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)==0,c("V2","V1")],col="red",pch=1,cex=.5)
points(muestreos[rowSums(get(sprintf("%s.mtz",ak))[,-1],na.rm=T)>0,c("V2","V1")],col="blue",pch=19)
##points(s10,col=2,pch=19,cex=.5)
##points(s11,col=4,pch=19,cex=.5)


###################################################
### code chunk number 188: Pl
###################################################
ak <- "Pion_seni"
k ="Pionus seniloides"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 189: inc113_Pionus.Rnw:98-99
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 190: inc113_Pionus.Rnw:104-106
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 191: Ps
###################################################
ak <- "Pion_sord"
k ="Pionus sordidus"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 192: inc113_Pionus.Rnw:119-120
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 193: inc113_Pionus.Rnw:125-127
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 194: inc114_Thectocercus.Rnw:6-7
###################################################
subset(rsm.loros,grepl("Thectocercus ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 195: Ta
###################################################
ak <- "Arat_acut"
k ="Thectocercus acuticaudatus"


###################################################
### code chunk number 196: inc114_Thectocercus.Rnw:19-20
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 197: inc115_Psittacara.Rnw:4-5
###################################################
subset(rsm.loros,grepl("Psittacara ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 198: inc115_Psittacara.Rnw:9-11
###################################################
ak <- "Arat_leuc"
k ="Psittacara leucophthalmus"


###################################################
### code chunk number 199: inc115_Psittacara.Rnw:15-16
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 200: inc115_Psittacara.Rnw:20-21
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)



###################################################
### code chunk number 201: inc115_Psittacara.Rnw:26-28
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 202: Aw
###################################################
ak <- "Arat_wagl"
k ="Psittacara wagleri"


###################################################
### code chunk number 203: inc115_Psittacara.Rnw:42-43
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 204: inc115_Psittacara.Rnw:47-48
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 205: inc115_Psittacara.Rnw:53-55
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 206: P
###################################################
subset(rsm.loros,grepl("Pyrilia ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 207: Pb
###################################################
ak <- "Pion_barr"
k ="Pyrilia barrabandi"


###################################################
### code chunk number 208: inc116_Pyrilia.Rnw:15-16
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 209: inc116_Pyrilia.Rnw:20-21
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)



###################################################
### code chunk number 210: inc116_Pyrilia.Rnw:26-28
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 211: Pa
###################################################
ak <- "Pyri_caic"
k ="Pyrilia caica"


###################################################
### code chunk number 212: inc116_Pyrilia.Rnw:40-41
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 213: inc116_Pyrilia.Rnw:46-48
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 214: Pp
###################################################
ak <- "Pyri_pyri"
k ="Pyrilia pyrilia"


###################################################
### code chunk number 215: inc116_Pyrilia.Rnw:60-61
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 216: inc116_Pyrilia.Rnw:66-68
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 217: inc117_Pyrrhura.Rnw:7-8
###################################################
subset(rsm.loros,grepl("Pyrrhura ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 218: Pc
###################################################
ak <- "Pyrr_caer"
k ="Pyrrhura caeruleiceps"


###################################################
### code chunk number 219: inc117_Pyrrhura.Rnw:18-19
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 220: inc117_Pyrrhura.Rnw:24-26
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 221: Pg
###################################################
ak <- "Pyrr_egre"
k ="Pyrrhura egregia"


###################################################
### code chunk number 222: inc117_Pyrrhura.Rnw:37-38
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 223: inc117_Pyrrhura.Rnw:43-45
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 224: inc117_Pyrrhura.Rnw:49-50
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 225: inc117_Pyrrhura.Rnw:53-54
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 226: Pe
###################################################
ak <- "Pyrr_emma"
k ="Pyrrhura emma"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 227: inc117_Pyrrhura.Rnw:69-70
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 228: inc117_Pyrrhura.Rnw:75-77
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 229: Ph
###################################################
ak <- "Pyrr_hoem"
k ="Pyrrhura hoematotis"


###################################################
### code chunk number 230: inc117_Pyrrhura.Rnw:87-88
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 231: inc117_Pyrrhura.Rnw:93-95
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 232: inc117_Pyrrhura.Rnw:99-100
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 233: inc117_Pyrrhura.Rnw:103-104
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 234: Pm
###################################################
ak <- "Pyrr_mela"
k ="Pyrrhura melanura"


###################################################
### code chunk number 235: inc117_Pyrrhura.Rnw:118-119
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 236: inc117_Pyrrhura.Rnw:124-126
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 237: inc117_Pyrrhura.Rnw:130-131
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 238: inc117_Pyrrhura.Rnw:136-137
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 239: Pp
###################################################
ak <- "Pyrr_pict"
k ="Pyrrhura picta"


###################################################
### code chunk number 240: inc117_Pyrrhura.Rnw:149-150
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 241: inc117_Pyrrhura.Rnw:155-157
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 242: inc117_Pyrrhura.Rnw:161-162
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 243: inc117_Pyrrhura.Rnw:168-185
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
re <- ranef(mdl)
prd <- predict(mdl,"state")
psi.cond <- re@post[,2,1]
nms <- trans.info$NM[match(substr(as.character(mdl@data@siteCovs$idpunto),1,2),trans.info$IDTransect)]

layout(matrix(1:4,ncol=2))
for (nn in unique(nms)) {
    matplot(prd[nms %in% nn,c(1,3,4)],type="l",lty=c(1,3,3),col=1,ylim=c(0,1),main=sprintf("NM%s",nn),ylab="Probability of presence")
    y <- psi.cond[nms %in% nn]
    x <- seq(along=y)
    points(x[y<0.125], y[y<0.125],pch=19,col="red")
    points(x[y>0.125], y[y>0.125],pch=19,col="grey47")
    points(x[y>0.95], y[y>0.95],pch=19,col="blue")

}


###################################################
### code chunk number 244: Pr
###################################################
ak <- "Pyrr_rhod"
k ="Pyrrhura rhodocephala"


###################################################
### code chunk number 245: inc117_Pyrrhura.Rnw:197-198
###################################################
cat(paste(with(subset(trans.info,IDTransecta %in% subset(mexp,spp %in% k)$NM),sprintf("'%s', %s state (NeoMaps route NM%s)",Nombretransecta,Estado,NM)),collapse="; "))


###################################################
### code chunk number 246: inc117_Pyrrhura.Rnw:203-205
###################################################
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",k)),full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

#s21 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% k)
#s20 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% k))

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(shp,pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.9)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.75)
points(s11,col=4,pch=19,cex=.75)




###################################################
### code chunk number 247: inc117_Pyrrhura.Rnw:209-210
###################################################
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 248: inc117_Pyrrhura.Rnw:213-214
###################################################
fL <- get(sprintf("%s.fL",ak))
mdl <- fL[[which.min(lapply(fL,function(x) x@AIC))]]
summary(mdl)  


###################################################
### code chunk number 249: T
###################################################
subset(rsm.loros,grepl("Touit ",rownames(rsm.loros)))[,-1]


###################################################
### code chunk number 250: inc100_Touit.Rnw:13-15
###################################################
ak <- "Toui_bata"
k ="Touit batavica"


###################################################
### code chunk number 251: Th
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)




###################################################
### code chunk number 252: Td
###################################################
ak <- "Toui_dile"
k ="Touit dilectissima"


###################################################
### code chunk number 253: inc100_Touit.Rnw:32-34
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)




###################################################
### code chunk number 254: inc100_Touit.Rnw:39-43
###################################################
ak <- "Toui_huet"
k ="Touit huetii"
if (any(rslts$spp %in% ak))
    subset(rslts,spp %in% ak)


###################################################
### code chunk number 255: inc100_Touit.Rnw:47-49
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)




###################################################
### code chunk number 256: inc100_Touit.Rnw:55-57
###################################################
ak <- "Toui_purp"
k ="Touit purpurata"


###################################################
### code chunk number 257: Tp
###################################################
qry <- gsub(" ","_",gsub("Thectocercus","Psittacara",k))
if(grepl("Touit",k))
    qry <- gsub(" ","_",gsub("a$","us",k))
slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",
                        qry),
           full.names=T)
if (length(slc)==0) {
    slc <- dir(gis.data,pattern=sprintf("%s_[0-9]+.shp",gsub(" ","_",gsub("a$","us",k))),full.names=T)
}
    

s11 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & (species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)
s10 <- subset(aves, !(collectionCode %in% "doi:10.1594/PANGAEA.803430") & species %in% c(alt.loros,todos.loros) & !(species %in% c(k,gsub("Thectocercus","Psittacara",k),rsm.loros[k,"old.spp"])) & year %in% 2008:2012)

shp <- crop(shapefile(slc),e)
par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
plot(shp,add=T,lwd=2,col="pink")
plot(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],border="grey33",add=T)
text(CNEB[match(trans.info$CNEB,CNEB@data$cdg),],trans.info$NM,col="grey33",cex=.7)

plot(vz0,add=T,border="grey13")

points(s10,col=2,pch=19,cex=.5)
points(s11,col=4,pch=19,cex=.5)




###################################################
### code chunk number 258: inc100_Touit.Rnw:69-161 (eval = FALSE)
###################################################
## 
## 
## ##subset(rslts,Delta.AICc==0 & grepl("V",mod))
## 
## 
## 
## 
## evaluate(p=extract(Amaz_amaz.phPsiVC,s11),
##          a=extract(Amaz_amaz.phPsiVC,s10))
## evaluate(p=extract(Amaz_amaz.phPsiVC,s21),
##          a=extract(Amaz_amaz.phPsiVC,s20))
## 
## boxplot(extract(Amaz_amaz.phPsiVC,s10),extract(Amaz_amaz.phPsiVC,s11))
## 
## aves[aves@data$species %in% "Amazona amazonica",]
## 
## points(aves[aves@data$species %in% "Amazona amazonica",],col=2,pch=.7)
## 
## 
## mdl <- get(sprintf("%s.%s",rsm.Loros[slc,"acronimo"],
##                    gsub("[\\(\\)]","",
##                         subset(rslts,Delta.AICc==0 & grepl("V",mod) & spp %in% rsm.Loros[slc,"acronimo"])$mod)))
## 
##                                                                                                                  assign(sprintf("%s.val",rsm.Loros[slc,"acronimo"]),
##            values(rasterize(shp,mdl)))
## 
##     png(file=sprintf("Pred_%s.png",rsm.Loros[slc,"acronimo"]),bg="aliceblue",width=600,height=500,pointsize=14)
##     plot(mdl,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Oranges")),main=rsm.Loros[slc,1],    sub=sprintf("COR = %0.3f",cor(values(mdl),!is.na(get(sprintf("%s.val",rsm.Loros[slc,"acronimo"]))),
##         use="complete"))
## )
##     plot(shp,add=T)
##     plot(vz0,lty=3,add=T)
## dev.off()
## 
## 
## plot(Diop_nobi.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")),main="Diop. nobi.")
##  plot(Pion_mens.phPsiV,breaks=seq(0,1,length=11),col=c("grey77",brewer.pal(9,"Oranges")),main="Pionus menstruus")
## ##plot(Pion_mens.phPsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## 
## 
## plot(Pyrr_pict.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## plot(Brot_jugu.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## plot(Arat_pert.p.PsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## plot(Ara_seve.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## plot(Ara_mili.phPsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## 
## 
## 
## 
## plot(Amaz_ochr.p.PsiVC,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
## 
## par(mar=c(2,2,0,0))
## plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
## image(Amaz_amaz.p.PsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Oranges")),ylab="",xlab="",axes=F,add=T)
## axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
## axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))
## 
## ##
## plot(shp,add=T,lwd=5)
## plot(vz0,add=T,border="grey13")
## points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)==0,c("V2","V1")],col="cyan",pch=1,cex=.5)
## points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
## 
## for (k in c("Amazona amazonica","Amazona ochrocephala","Ara militaris",
##             "Ara severus","Eupsittula pertinax","Brotogeris jugularis",
##             "Pionus menstruus","Pyrrhura picta","Diopsittaca nobilis")) {
##     
##     slc <- match(k,rsm.Loros[,1])
##     shp <- crop(shapefile(sprintf("~/gisdata/distribuciones/BirdLife/%s.shp",rsm.Loros[slc,"BL.shp"])),e)
##     mdl <- get(sprintf("%s.%s",rsm.Loros[slc,"acronimo"],
##                        gsub("[\\(\\)]","",
##                             subset(rslts,Delta.AICc==0 & grepl("V",mod) & spp %in% rsm.Loros[slc,"acronimo"])$mod)))
## 
##     assign(sprintf("%s.val",rsm.Loros[slc,"acronimo"]),
##            values(rasterize(shp,mdl)))
## 
##     png(file=sprintf("Pred_%s.png",rsm.Loros[slc,"acronimo"]),bg="aliceblue",width=600,height=500,pointsize=14)
##     plot(mdl,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Oranges")),main=rsm.Loros[slc,1],    sub=sprintf("COR = %0.3f",cor(values(mdl),!is.na(get(sprintf("%s.val",rsm.Loros[slc,"acronimo"]))),
##         use="complete"))
## )
##     plot(shp,add=T)
##     plot(vz0,lty=3,add=T)
## dev.off()
## 
## png(file=sprintf("Boxplot_%s.png",rsm.Loros[slc,"acronimo"]),bg="aliceblue",width=400,height=400,pointsize=14)
## boxplot(values(mdl)~!is.na(get(sprintf("%s.val",rsm.Loros[slc,"acronimo"]))),notch=T)    
##    dev.off() 
## }
## 
## 
## 
## 


###################################################
### code chunk number 259: inc100_Touit.Rnw:164-197 (eval = FALSE)
###################################################
## 
## 
## 
## 
## 
## cor(values(Amaz_ochr.p.PsiVT),!is.na(Amaz_ochr.val),use="complete")
## 
## cor.test(as.vector(values(Amaz_ochr.p.PsiVT)),
##     as.vector(!is.na(Amaz_ochr.val)),use="complete")
## 
## boxplot(values(Amaz_amaz.p.PsiVT)~!is.na(Amaz_amaz.val),notch=T)
## boxplot(values(Amaz_ochr.p.PsiVT)~!is.na(Amaz_ochr.val),notch=T)
## boxplot(values(Ara_seve.p.PsiV)~!is.na(Ara_seve.val),notch=T)
## boxplot(values(Ara_mili.phPsiV)~!is.na(Ara_mili.val),notch=T)
## boxplot(values(Diop_nobi.p.PsiV)~!is.na(Diop_nobi.val),notch=T)
## boxplot(values(Pion_mens.phPsiVT)~!is.na(Pion_mens.val),notch=T)
## boxplot(values(Pyrr_pict.p.PsiV)~!is.na(Pyrr_pict.val),notch=T)
## boxplot(values(Brot_jugu.p.PsiV)~!is.na(Brot_jugu.val),notch=T)
## boxplot(values(Arat_pert.p.PsiVT)~!is.na(Arat_pert.val),notch=T)
## 
## 
## for (mdl in c("p(.)Psi(T)","p(h)Psi(T)")) {
##     ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
##     for (k in 1:nrow(ssr)) {
##         psi <- raster(mVars,1)*NA
##         ##values(psi)[ss] <- prdPsi$Predicted
##         fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
##         values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1,PET,PET^2,LSTd,LSTd^2))))
##         assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
##     }
## 
## }
## 


