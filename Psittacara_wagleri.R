

lls <- sp::coordinates(aves)
ss <- !is.na(yr) & !is.na(mes) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(avdat$pre) & !is.na(avdat$wpre) & !is.na(avdat$pendiente) & !is.na(avdat$arcilla) & lls[,1] > -73 & lls[,1] < -60.5 & lls[,2] > 7.25 & lls[,2] < 12.25 & !duplicated(data.frame(qrw,qrx,qry,qrz,wa,mes,col,yr))


y <- wa[ss]


## hipótesis
## arcilla (nidificación)
A1 <- paste(sprintf("%1$s + I(%1$s^2)",c("arcilla")),collapse=" + ")
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


fm1 <- expand.grid(c(T,F),c(T),c(T,F),c(T,F),c(T,F))
colnames(fm1) <- c("A1","W1","M2","S","D")
##fm1 <- subset(fm1,!(D & Y) & !(D & S))
fm1 <- subset(fm1,!(D & S))

fm2 <- expand.grid(c(T,F),c(T),c(T,F),c(T,F),c(T,F))
colnames(fm2) <- c("A1","M1","W2","S","D")
fm2 <- subset(fm2,!(D & S))

fm3 <- expand.grid(c(T,F),c(T))
colnames(fm3) <- c("A1","T1")
fm4 <- expand.grid(c(T,F),c(T),c(T,F))
colnames(fm4) <- c("A1","T2","D")



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



## promedio mensual
vz1 <- vz0[!vz0@data$ESTADO %in% c("Bolívar","Amazonas","Zona en Reclamación","Delta Amacuro"),]
e <- extent(c(-74,-61,6,13))

r01 <- crop(raster(WC,1),e)
r02 <- crop(raster(WC,3),e)
r03 <- crop(raster(WC,15),e)

vrs <- stack(r01,r02,r03)
rvz <- rasterize(vz1,vrs)>0

vrs <- vrs*rvz
names(vrs) <- c("wbio01","wbio03","wbio15")

mpsi <- raster(vrs,1)
values(mpsi) <- 0
mpsi.01 <- mpsi.02 <- mpsi.03 <- mpsi.04 <- mpsi.05 <- mpsi.06 <- mpsi.07 <- 
mpsi.08 <- mpsi.09 <- mpsi.10 <- mpsi.11 <- mpsi.12 <- mpsi
j <- 0
cop <- 0.78##median(aggregate(predict(lm3,type="response"),list(y),median)$x)
prd <- 1940:2010
##prd <- 1981:1995

rsltds <- data.frame()
for (k in prd) {
    cc <- seq(along=names(rdtmp))[as.numeric(substr(names(rdtmp),2,5)) %in% k]
    ss2 <- (substr(yr,2,5) %in% k)
    mi.y <- wa[ss & !ss2] 
    tu.y <- wa[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ dtmp + dpre,avdat[ss & !ss2,],family=binomial)

    
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
    for (m in 1:12) {
        r04 <- raster(rdtmp,cc[m])
        r05 <- raster(rdpre,cc[m])
        vrt <- vrs
        for (x in 4:5) {
            rr <- sprintf("r%02d",x)
            re <- crop(get(rr),e)
            dd <- disaggregate(re,round(res(re)[1]/res(r01)[1]))
            vrt <- addLayer(vrt,resample(dd,r01)*rvz)    
        }
        psi <- raster(vrt,1)
        nwdt <- data.frame(values(vrt))
        colnames(nwdt) <- c(names(vrs),"dtmp","dpre")

        values(psi) <- boot::inv.logit(rowSums(predict(lm00,nwdt,type="terms"))) 
        
        x01 <- extract(psi,aves[ss & ss2,])
        if (sum(tu.y)>0) {
            ev1 <- evaluate(x01[tu.y],x01[!tu.y])
            ##  boxplot(ev1)    
            cop <- threshold(ev1,"no_omission")
        } else {
            cop <- 0.78
        }
        rsltds <- rbind(rsltds,
                        data.frame(spp="Psittacara wagleri",fecha=k, mes=m,
                                   total.c=sum(values(psi),na.rm=T),
                                   umbral=cop,total.d=sum(values(psi>cop),na.rm=T)))
        assign(sprintf("mpsi.%02d",m),get(sprintf("mpsi.%02d",m))+(psi>cop))
        mpsi <- mpsi+(psi>cop)
        j <- j+1
        plot(mpsi/j)
        points(aves[wa,])
        title(main=k,sub=m)
    }
    print(rsltds)
    plot(total.c~I(fecha+mes/12),rsltds)
}
mpsi.t3 <- mpsi/j

plot(total.c~I(fecha+mes/12),rsltds)
plot(total.d~I(fecha+mes/12),rsltds)
plot(umbral~I(fecha+mes/12),rsltds)

wa.ts <- with(rsltds,ts(total.c,start=min(fecha),end=max(fecha),frequency=12))
wa.dts <- decompose(wa.ts)
plot(wa.dts)
plot(1:12,head(wa.dts$seasonal,12))
sd(wa.dts$trend,na.rm=T)/mean(wa.dts$trend,na.rm=T)
sd(wa.dts$seasonal,na.rm=T)/mean(wa.dts$trend,na.rm=T)

sd(wa.dts$random,na.rm=T)/mean(wa.dts$trend,na.rm=T)


boxplot(avdat[ss & wa,"dpre"]~I(substr(yr[ss & wa],2,5)>1995))

plot(aves[ss & wa,])
points(aves[ss & wa & substr(yr,2,5)<1995,],col=2)
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

plot(mpsi.t1>.5)

plot((mpsi.t1>.5)+(mpsi.t2>.5)*2+(mpsi.t3>.5)*4,col=c("white","red","red","red","blue","green","green","green"))


mpsis <- stack(mpsi.01,mpsi.02,mpsi.03,mpsi.04,mpsi.05,mpsi.06,
               mpsi.07,mpsi.08,mpsi.09,mpsi.10,mpsi.11,mpsi.12)

mpsi.p <- mpsi.a <- mpsi.01
values(mpsi.p) <- apply(values(mpsis),1,max)
values(mpsi.a) <- 1-apply(values(mpsis),1,min)
plot(mpsi.p)
plot(mpsi.a)

layout(matrix(1:3,ncol=3))
       
plot(mpsi.t1,main="P. wagleri",font.main=4)
title(sub="Periodo 1940-1950",line=2.5)
plot(mpsi.t2,main="P. wagleri",font.main=4)
title(sub="Periodo 1950-1980",line=2.5)
plot(mpsi.t3,main="P. wagleri",font.main=4)
title(sub="Periodo 1980-2010",line=2.5)


require(vegan)
xdt <- values(mpsis)
sxs <- rowSums(is.na(xdt))==0
pca1 <- rda(xdt[sxs,])
##anova(pca1,"axis")

##no seasonal variation
 cumsum(pca1$CA$eig)/sum(pca1$CA$eig)

values(mpsi.p)[sxs] <- unname(scores(pca1,choice=1,"sites"))
values(mpsi.a)[sxs] <- unname(scores(pca1,choice=2,"sites"))
