data.dir <- "~/Dropbox/CEBA/data/720_SeriesTiempoPsitacidos"
data.dir <- "~/Dropbox/ceba/data/720_SeriesTiempoPsitacidos"

rgs.Ab <- read.csv(sprintf("%s/rgs_abarbadensis.csv",data.dir),dec=".",sep=";")
rgs.Ab$lon <- -1*abs(rgs.Ab$lon)

plot(lat~lon,rgs.Ab)
table(rgs.Ab$fecha)

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

