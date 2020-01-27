(load("~/Dropbox/NeoMapas/Rdata/NMaves.rda"))

generos <- c("Amazona","Ara","Aratinga","Bolborhynchus","Brotogeris","Deroptyus","Diopsittaca","Eupsittula","Forpus","Hapalopsittaca","Nannopsittaca","Orthopsittaca","Pionites","Pionus","Psilopsiagon","Psittacara","Pionopsitta","Pyrrhura","Touit")
lst.spp <- c()
for (gg in generos)
    lst.spp <- unique(c(lst.spp,grep (gg,spp.aves$Latname)))

lst.spp <- lst.spp[sapply(spp.aves[lst.spp,"Latname"],function(x) strsplit(x," ")[[1]][1]) %in% generos]


table(spp.aves[lst.spp,"N_ave"] %in% NM.m1$Especieid)
table(spp.aves[lst.spp,"N_ave"] %in% NM.m2$Especieid)
table(NM.m1$Especieid[NM.m1$Especieid %in% spp.aves[lst.spp,"N_ave"]])
sort(table(NM.m1$Especieid[NM.m1$Especieid %in% spp.aves[lst.spp,"N_ave"]]))
subset(spp.aves,N_ave %in% c(299,333,334,292,308,324,311,336))

subset(spp.aves,N_ave %in% unique(NM.m1$Especieid[NM.m1$Especieid %in% spp.aves[lst.spp,"N_ave"]]))

## eliminar Aramus guarauna


x1 <- extract(WC,muestreos[,c("V2","V1")])
nmdat <- data.frame(x1)

colnames(nmdat) <- paste("w",colnames(x1),sep="")
nmdat$arcilla <- extract(arcilla,muestreos[,c("V2","V1")])
nmdat$pendiente <- extract(pendiente,muestreos[,c("V2","V1")])

krw <- cellFromXY(WC,muestreos[,c("V2","V1")])
krx <- cellFromXY(arcilla,muestreos[,c("V2","V1")])
kry <- cellFromXY(rtmp,muestreos[,c("V2","V1")])
krz <- cellFromXY(pendiente,muestreos[,c("V2","V1")])

trans.info[,c("IDTransecta","Fecha.Muestreo.1")]


kcol <- sprintf("X2010.%s.",substr(trans.info[ match(muestreos$NM,trans.info$IDTransecta) ,"Fecha.Muestreo.1"],0,2))

kyr <- rep("X2010",nrow(muestreos))
kmes <- sprintf("%s",substr(trans.info[ match(muestreos$NM,trans.info$IDTransecta) ,"Fecha.Muestreo.1"],0,2))


for (kc in unique(kcol)) {
    slc <- grep(kc[1],names(rtmp))
    nmdat[kcol %in% kc,"tmp"] <- extract(raster(rtmp,slc),muestreos[kcol %in% kc,c("V2","V1")])
    nmdat[kcol %in% kc,"pre"] <- extract(raster(rpre,slc),muestreos[kcol %in% kc,c("V2","V1")])
    nmdat[kcol %in% kc,"dtmp"] <- extract(raster(rdtmp,slc),muestreos[kcol %in% kc,c("V2","V1")])
    nmdat[kcol %in% kc,"dpre"] <- extract(raster(rdpre,slc),muestreos[kcol %in% kc,c("V2","V1")])
}

    gc()
    
for (j in 1:19) {
    lys <- sprintf("bio%02d",j)
    bb <- raster(get(lys),grep(2010,names(bio01)))
    nmdat[,paste("m",lys,sep="")] <- extract(bb,muestreos[,c("V2","V1")])
}


for (kc in unique(kmes)) {
    slc <- grep(kc[1],names(wtmp))
    nmdat[kmes %in% kc,"wtmp"] <- extract(raster(wtmp,slc),muestreos[kmes %in% kc,c("V2","V1")])
    nmdat[kmes %in% kc,"wpre"] <- extract(raster(wpre,slc),muestreos[kmes %in% kc,c("V2","V1")])

}

nmdat$ctmp <- (nmdat$wtmp)/10 + nmdat$dtmp 
nmdat$cpre <- (nmdat$wpre)/10 + nmdat$dpre

nmdat$cphi <- cos(2*pi*as.numeric(kmes)/12)
nmdat$sphi <- sin(2*pi*as.numeric(kmes)/12)

## importante, fecha corregida el 19/10/2015
mt1 <- tapply(NM.m1$hrs,list(factor(NM.m1$idpunto,levels=muestreos$V3)),max)
mt2 <- tapply(NM.m2$hrs,list(NM.m2$idpunto,NM.m2$Lapso),max)
mt2[,1] <- rowMeans(mt2,na.rm=T)
mt2[,2] <- mt2[,1]+3/(60*24)
mt2[,3] <- mt2[,2]+3/(60*24)

obs1 <- merge(mt1,mt2,by="row.names",all=T)

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

    
    assign(sprintf("%s.mtz",
                   sapply(subset(spp.aves,N_ave %in% acdg)$Latname,function(x) {
                       y <- strsplit(x," ")[[1]]
                       paste(substr(y[1],1,4),substr(y[2],1,4),sep="_")
                   })),
           ##abbreviate(subset(spp.aves,N_ave %in% acdg)$Latname)),
           mtz)
    nmbrs <- c(nmbrs,subset(spp.aves,N_ave %in% acdg)$Latname)
}

stdat <- nmdat
for (k in 1:ncol(nmdat)) {
    mu <- mean(nmdat[,k],na.rm=T)
    sg <- sd(nmdat[,k],na.rm=T)
    stdat[,k] <- (nmdat[,k]-mu)/sg
}


##lst.spp[!lst.spp %in% rslts$spp]
