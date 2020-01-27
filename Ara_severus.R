ara.sev <- grepl("Ara sever",aves@data$species)

ss <- !is.na(yr) & !is.na(mes) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(avdat$pre) & !is.na(qrv) & !is.na(avdat$ndvi) & !is.na(avdat$wpre) & !is.na(avdat$pendiente) & !is.na(avdat$arcilla) & !duplicated(data.frame(qrw,qrx,qry,qrz,qrv,nyr,ara.sev,mes,col,yr))
y <- ara.sev[ss]

shp.as <- shapefile("~/gisdata/distribuciones/BirdLife/Ara_severus_22685577.shp")

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
## ndvi
V  <- paste(sprintf("%1$s + I(%1$s^2)",c("ndvi")),
            collapse=" + ")

##Tendencia anual lineal ## la puedo usar pero prob. indica dif. muestreo
Y <- paste(sprintf("%1$s ","year"),collapse=" + ")
##estacionalidad 
S <- paste(sprintf("%1$s ",c("cphi","sphi")),collapse=" + ")
##anomalias tmp y pre
D <- paste(sprintf("%1$s ",c("dtmp", "dpre")),collapse=" + ")


fm1 <- expand.grid(c(T),c(T,F),c(T,F),c(T,F),c(T,F))
colnames(fm1) <- c("W1","M2","S","D","V")
##fm1 <- subset(fm1,!(D & Y) & !(D & S))
fm1 <- subset(fm1,!(D & S))

fm2 <- expand.grid(c(T),c(T,F),c(T,F),c(T,F),c(T,F))
colnames(fm2) <- c("M1","W2","S","D","V")
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

lm.W1M2D

###
# predicción

## promedio mensual
r01 <- raster(WC,1)
r02 <- raster(WC,3)
r03 <- raster(WC,15)

vrs <- stack(r01,r02,r03)
names(vrs) <- c("wbio01","wbio03","wbio15")
mpsi <- r01
values(mpsi) <- 0
mpsi.01 <- mpsi.02 <- mpsi.03 <- mpsi.04 <- mpsi.05 <- mpsi.06 <- mpsi.07 <- 
mpsi.08 <- mpsi.09 <- mpsi.10 <- mpsi.11 <- mpsi.12 <- mpsi
j <- 0
cop <- 0.78##median(aggregate(predict(lm3,type="response"),list(y),median)$x)
prd <- 1940:2010
prd <- 1981:2010

for (k in prd) {
    cc <- seq(along=names(bio02))[as.numeric(substr(names(bio02),2,5)) %in% k]
    ss2 <- (substr(yr,2,5) %in% k)
    mi.y <- ara.sev[ss & !ss2] 
    tu.y <- ara.sev[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ mbio02 + I(mbio02^2) + mbio18 + I(mbio18^2) + mbio17 + I(mbio17^2) +mbio12 + I(mbio12^2) +  ndvi + I(ndvi^2),avdat[ss & !ss2,],family=binomial)

    
    if (k %in% 1950) {
        mpsi.t1 <- mpsi/j
        mpsi <- r00
        values(mpsi) <- 0
        j <- 0
    }
    if (k %in% 1980) {
        mpsi.t2 <- mpsi/j
        mpsi <- r00
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
        vrt <- addLayer(vrt,resample(dd,r01))    
    }

    for (m in 1:12) {
      if (!is.na(grep(sprintf("A%s%s",k,m),names(ndvis))[1])) {

            rr <- raster(ndvis,grep(sprintf("A%s%s",k,m),names(ndvis))[1])
            dd <- disaggregate(rr,round(res(rr)[1]/res(r01)[1]))
            vrt <- addLayer(vrt,resample(dd,r01))    
            ##nwdt$cphi <- cos(2*pi*as.numeric(m)/12)
            ##nwdt$sphi <- sin(2*pi*as.numeric(m)/12)
            psi <- raster(vrt,1)
            nwdt <- data.frame(values(vrt))
            colnames(nwdt) <- c(names(vrs),"mbio02","mbio12","mbio17","mbio18","ndvi")

        values(psi) <- boot::inv.logit(rowSums(predict(lm00,nwdt,type="terms"))) 

        x01 <- extract(psi,aves[ss & ss2,])
        if (sum(tu.y)>0) {
            ev1 <- evaluate(x01[tu.y],x01[!tu.y])
            ##  boxplot(ev1)
            
            cop <- threshold(ev1,"no_omission")
        } 
        assign(sprintf("mpsi.%02d",m),get(sprintf("mpsi.%02d",m))+(psi>cop))
        mpsi <- mpsi+(psi>cop)
        j <- j+1
        plot(mpsi/j)
        points(aves[ara.sev,])
        title(main=k,sub=m)
        }
  }
}

mpsi.t3 <- mpsi/j


plot(mpsi.t3,col=brewer.pal(9,"Spectral"))
points(aves[ara.sev,])

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

plot(mpsi.t3)

mpsis <- stack(mpsi.01,mpsi.02,mpsi.03,mpsi.04,mpsi.05,mpsi.06,
               mpsi.07,mpsi.08,mpsi.09,mpsi.10,mpsi.11,mpsi.12)

mpsi.p <- mpsi.a <- mpsi.01
values(mpsi.p) <- apply(values(mpsis),1,max)
values(mpsi.a) <- apply(values(mpsis),1,min)
plot(mpsi.p-mpsi.a)
 




UMF <- unmarkedFrameOccu(Arsv.mtz[,-1], siteCovs=stdat, obsCovs=list(hora=obs1[,-1]))
   
fm <- occu(~ hora + I(hora^2) ~ wbio01+I(wbio01^2) + wbio17+I(wbio17^2) + wbio03+I(wbio03^2), UMF)
os <- fm@sitesRemoved
fm1 <- occu(~ 1 ~ wbio17+I(wbio17^2) + wbio03+I(wbio03^2), UMF[-os,],start=c(0,0,sample(c(-1,1),4,replace=T)))
fm2 <- occu(~ hora ~ wbio17+I(wbio17^2) + wbio03+I(wbio03^2), UMF[-os,],start=c(0,0,0,sample(c(-1,0,1),4,replace=T)))
fm3 <- occu(~ hora + I(hora^2) ~ wbio17+I(wbio17^2) + wbio03+I(wbio03^2), UMF[-os,],start=c(coef(fm2),0))
