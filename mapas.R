(load("~/Dropbox/NeoMapas/Rdata/NMaves.rda"))
##GBIF.org (10th June 2016) GBIF Occurrence Download http://doi.org/10.15468/dl.ofmi8y

system("unzip ~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos/0005167-160526112335914.zip")
aves2 <- read.table("0005167-160526112335914.csv",header=T,fill=T,nrows=-1,quote="",sep="\t")
ss <- !is.na(aves2$decimallongitude) & !is.na(aves2$decimallatitude)
aves2 <- aves2[ss,]
coordinates(aves2) <- c("decimallongitude","decimallatitude")
proj4string(aves2) <- tmp@proj4string


if (!exists("rsm.Loros")) {
    require(ROpenOffice)
    rsm.Loros <- read.ods("~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos/Situacion loros Venezuela 2015_NeoMapas.ods")
    rsm.Loros <- rsm.Loros[[1]]
}


##subset(spp.aves,grepl("Amazona",Latname))[,c("N_ave","Latname")]
##grep("Touit",dir("~/gisdata/distribuciones/BirdLife",pattern="shp",full.names=F),value=T)

##n.ave <- subset(spp.aves,Latname %in% "Pyrrhura hoematotis")$N_ave
for (k in 1:nrow(rsm.Loros)) {
    n.ave <- rsm.Loros[k,"N_ave"]
    slc <- unique(c(subset(NM.m1,Especieid %in% n.ave)$idpunto,
                subset(NM.m2,Especieid %in% n.ave)$idpunto))
    tmp <- crop(shapefile(sprintf("~/gisdata/distribuciones/BirdLife/%s.shp",rsm.Loros[k,"BL.shp"])),e)

    nms <- unique(trim(unlist(rsm.Loros[k,c(1,2,29)])))
    nms <- nms[!is.na(nms)]
    tmp1 <- subset(aves2,scientificname %in% nms | species %in% nms)

    tmp1@proj4string <- tmp@proj4string
    tmp2 <- subset(muestreos,V3 %in% slc)

    png(file=sprintf("Mapa_%s.png",gsub(" ","_",rsm.Loros[k,1])))
    plot(vz0,main=rsm.Loros[k,1])
    plot(tmp,col=rgb(.5,0,0,.5),add=T)
    if (nrow(tmp1)>0) {
        points(tmp1,col=2,cex=.5,pch=19)
        title(sub=sprintf("%s obs GBIF (%s en 2010) %s puntos fuera de la distr.",
                          nrow(tmp1),
                          nrow(subset(tmp1,year %in% 2010 & !collectioncode %in% "doi:10.1594/PANGAEA.803430")),
                          sum(is.na(over(tmp1,tmp)$PRESENCE))),line=1)
    }
    if (nrow(tmp2)>0) {
        coordinates(tmp2) <- c("V2","V1")
        proj4string(tmp2) <- tmp@proj4string
        points(tmp2,pch=19,col=4)
        title(sub=sprintf("%s puntos de NM fuera de la distr.",
                          sum(is.na(over(tmp2,tmp)$PRESENCE))),line=2)
    }
    dev.off()
    
}


##subset(muestreos,V3 %in% Pyrr_hoem.mtz[rowSums(Pyrr_hoem.mtz[,-1],na.rm=T)>0,"idpunto"])


table(!is.na(over(tmp1,tmp)$PRESENCE))
table(!is.na(over(tmp2,tmp)$PRESENCE))


table(sample(rsm.Loros[,1],size=700,prob=rsm.Loros$"GBIF todos",replace=T))
length(table(sample(rsm.Loros[,1],size=700,prob=rsm.Loros$"GBIF 2010",replace=T)))
