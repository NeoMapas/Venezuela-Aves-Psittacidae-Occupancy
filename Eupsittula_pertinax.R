eup.per <- grepl("pertinax",aves@data$species)

ss <- !is.na(yr) & !is.na(mes) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(avdat$pre) & !is.na(avdat$wpre) & !is.na(avdat$pendiente) & !is.na(avdat$arcilla) & !duplicated(data.frame(qrw,qrx,qry,qrz,eup.per,mes,col,yr))
y <- eup.per[ss]


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


fm1 <- expand.grid(c(T),c(T,F),c(T,F),c(T,F))
colnames(fm1) <- c("W1","M2","S","D")
##fm1 <- subset(fm1,!(D & Y) & !(D & S))
fm1 <- subset(fm1,!(D & S))

fm2 <- expand.grid(c(T),c(T,F),c(T,F),c(T,F))
colnames(fm2) <- c("M1","W2","S","D")
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

lm.W1M2S

boxplot(avdat[ss & eup.per,c("wbio01")]*.1,
        avdat[ss & eup.per,c("wtmp")]*.1,
        avdat[ss & eup.per,c("tmp")])

boxplot(avdat[ss & eup.per,c("tmp")]~mes[ss & eup.per])
boxplot(avdat[ss & eup.per,c("wtmp")]*.1~mes[ss & eup.per])
boxplot(avdat[ss & eup.per,c("wbio01")]*.1~mes[ss & eup.per])

mi.t <- as.numeric(substr(yr[ss & eup.per],2,10))+as.numeric(mes[ss & eup.per])/12
plot(avdat[ss & eup.per,c("tmp")]~mi.t,xlim=c(1960,2010))
points(avdat[ss & eup.per,c("wtmp")]*.1~mi.t,xlim=c(1960,2010),col=2,pch=19)
points(avdat[ss & eup.per,c("wbio01")]*.1~mi.t,xlim=c(1960,2010),col=4,pch=19)

plot(avdat[ss & eup.per,c("tmp")],avdat[ss & eup.per,c("wtmp")]*.1,
     xlab="T for a given month/year (CRU)",ylab="mean monthly T (WorlClim)")
abline(a=0,b=1,lty=2,col=2)

plot(avdat[ss & eup.per,c("tmp")],avdat[ss & eup.per,c("wbio01")]*.1,
     xlab="T for a given month/year (CRU)",ylab="mean annual T (WorlClim)",
     xlim=c(12,32),ylim=c(12,32))
abline(a=0,b=1,lty=2,col=2)
###
# predicción

## promedio mensual
r01 <- raster(WC,1)
r02 <- raster(WC,3)
r03 <- raster(WC,15)

vrs <- stack(r01,r02,r03)
rvz <- rasterize(vz0,vrs)>0
vrs <- vrs*rvz

rsltds <- data.frame()
names(vrs) <- c("wbio01","wbio03","wbio15")
mpsi <- r01
values(mpsi) <- 0
mpsi.01 <- mpsi.02 <- mpsi.03 <- mpsi.04 <- mpsi.05 <- mpsi.06 <- mpsi.07 <- 
mpsi.08 <- mpsi.09 <- mpsi.10 <- mpsi.11 <- mpsi.12 <- mpsi
j <- 0
cop <- 0.78##median(aggregate(predict(lm3,type="response"),list(y),median)$x)
prd <- 1940:2010
##prd <- 1981:1995
prd <- 1981:2010
for (k in prd) {
    cc <- seq(along=names(bio02))[as.numeric(substr(names(bio02),2,5)) %in% k]
    ss2 <- (substr(yr,2,5) %in% k)
    mi.y <- eup.per[ss & !ss2] 
    tu.y <- eup.per[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ mbio02 + I(mbio02^2) + mbio18 + I(mbio18^2) + mbio17 + I(mbio17^2) +mbio12 + I(mbio12^2) + cphi + sphi,avdat[ss & !ss2,],family=binomial)

    
    if (k %in% 1950) {
        mpsi.t1 <- mpsi/j
        mpsi <- r01
        values(mpsi) <- 0
        j <- 0
    }
    if (k %in% 1980) {
        mpsi.t2 <- mpsi/j
        mpsi <- r01
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
        vrt <- addLayer(vrt,resample(dd,r01)*rvz)    
    }
    psi <- raster(vrt,1)
    nwdt <- data.frame(values(vrt))
    colnames(nwdt) <- c(names(vrs),"mbio02","mbio12","mbio17","mbio18")

    for (m in 1:12) {
        nwdt$cphi <- cos(2*pi*as.numeric(m)/12)
        nwdt$sphi <- sin(2*pi*as.numeric(m)/12)

        values(psi) <- boot::inv.logit(rowSums(predict(lm00,nwdt,type="terms"))) 
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
        } else {
            cop <- 0.78
        }
        rsltds <- rbind(rsltds,
                        data.frame(spp="Eupsittula_pertinax",fecha=k, mes=m,
                                   total.c=sum(values(psi),na.rm=T),
                                   umbral=cop,total.d=sum(values(psi>cop),na.rm=T)))
        assign(sprintf("mpsi.%02d",m),get(sprintf("mpsi.%02d",m))+(psi>cop))
        mpsi <- mpsi+(psi>cop)
        j <- j+1
        plot(mpsi/j)
        points(aves[eup.per,])
        title(main=k,sub=m)
    }
      print(tail(rsltds))
    plot(total.c~I(fecha+mes/12),subset(rsltds,spp %in% "Eupsittula_pertinax"))

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

png(file="Aratinga_pertinax.png",width=800,height=700,pointsize=18)
layout(matrix(c(2,1,1),ncol=1))
par(mar=c(0,0,0,0))
plot(vz0,ylim=c(0,13))
image(mpsi.t3,col=brewer.pal(9,"Spectral"),add=T)
par(mar=c(5,4,1,1))
plot((total.c*21)~I(fecha+mes/12),subset(rsltds,spp %in% "Eupsittula_pertinax"),
     type="l",xlab="Año",ylab="Área total [km²]")
dev.off()


plot(mpsi.01-mpsi.08,colbrewer.pal(9,"PuYG"))


rpst <- aggregate(Arat_pert.Psi,5)
rpre <- raster(spsis,52)
rspt <- resample(rpst,rpre)
tmp <- stack(rpre*rspt,rpre*(1-rspt),rspt*(1-rpre),(1-rpre)*(1-rspt))

png(file="Eupsittacula_pertinax_comp.png",width=500,height=440,pointsize=18)
par(mar=c(0,0,0,0))
plot(which.max(tmp),col=rev(brewer.pal(4,"Spectral")),legend=T,axes=F)
plot(vz0,add=T)
dev.off()
     
plot(mpsi.t3)

mpsis <- stack(mpsi.01,mpsi.02,mpsi.03,mpsi.04,mpsi.05,mpsi.06,
               mpsi.07,mpsi.08,mpsi.09,mpsi.10,mpsi.11,mpsi.12)

mpsi.p <- mpsi.a <- mpsi.01
values(mpsi.p) <- apply(values(mpsis),1,max)
values(mpsi.a) <- 1-apply(values(mpsis),1,min)
plot(mpsi.p)
plot(mpsi.a)

layout(matrix(1:3,ncol=3))
       
plot(mpsi.t1,main="E. pertinax",font.main=4)
title(sub="Periodo 1940-1950",line=2.5)
plot(mpsi.t2,main="E. pertinax",font.main=4)
title(sub="Periodo 1950-1980",line=2.5)
plot(mpsi.t3,main="E. pertinax",font.main=4)
title(sub="Periodo 1980-2010",line=2.5)


require(vegan)
xdt <- values(mpsis)
sxs <- rowSums(is.na(xdt))==0
pca1 <- rda(xdt[sxs,])
##anova(pca1,"axis")

##no seasonal variation
 cumsum(pca1$CA$eig)/sum(pca1$CA$eig)

values(mpsi.p)[sxs] <- scores(pca1,choice=1)
values(mpsi.a)[sxs] <- scores(pca1,choice=2)


ep.ts <- with(subset(rsltds,spp %in% "Eupsittula_pertinax"),
              ts(total.c,start=min(fecha),end=max(fecha),frequency=12))
ep.dts <- decompose(ep.ts)
plot(ep.dts)

mean(ep.dts$trend,na.rm=T)

sd(ep.dts$trend,na.rm=T)/mean(ep.dts$trend,na.rm=T)
sd(ep.dts$seasonal,na.rm=T)/mean(ep.dts$trend,na.rm=T)

sd(ep.dts$random,na.rm=T)/mean(ep.dts$trend,na.rm=T)
