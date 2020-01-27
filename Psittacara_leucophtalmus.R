pst.leu <- grepl("Aratinga leucoph",aves@data$species)

## hace falta eliminar punto en Caracas y Sur del Lago
## registros dudosos: en caracas y los andes 
plot(aves)
points(aves[pst.leu,],col=3)
points(aves[pst.leu & lls[,1] < -66 & lls[,2]>8,],col=2,pch=18)


ss <- !is.na(yr) & !is.na(mes) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(avdat$pre) & !is.na(avdat$wpre) & !is.na(avdat$pendiente) & !is.na(avdat$arcilla) & !duplicated(data.frame(qrw,qrx,qry,qrz,pst.leu,mes,col,yr))
y <- pst.leu[ss] & !(lls[ss,1] < -66 & lls[ss,2]>8)

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


fms <- expand.grid(c(T,F),c(T,F),c(T,F),c(T,F),c(T,F),c(T,F),c(T,F),c(T,F))
colnames(fms) <- c("W1","W2","M1","M2","T1","T2","S","D")
fms <- subset(fms,!(D & S) & !(T1 & T2) & !(T1 & D)  & !(W1 & M1) & !(W2 & M2) & rowSums(fms)>0)


modnms <- c()
for (k in 1:nrow(fms)) {
    nms <- sprintf("lm.%s",paste(colnames(fms)[unlist(fms[k,])],collapse=""))
    assign(nms,
           glm(as.formula(sprintf("y ~  %s",paste(unlist(mget(colnames(fms)[unlist(fms[k,])])),collapse="+"))),
               avdat[ss,],family=binomial))
    modnms <- c(modnms,nms)
}

AICt <- aictab(mget(modnms),modnms)

###
# Mejor modelo

lm.W1W2D

###
# predicción

## promedio mensual
r01 <- raster(WC,1)
r02 <- raster(WC,3)
r03 <- raster(WC,15)
r04 <- raster(WC,2)
r05 <- raster(WC,12)
r06 <- raster(WC,17)
r07 <- raster(WC,7)

vrs <- stack(r01,r02,r03,r04,r05,r06,r07)
names(vrs) <- c("wbio01","wbio03","wbio15","wbio02","wbio12","wbio17","wbio07")
mpsi <- r01
values(mpsi) <- 0
mpsi.01 <- mpsi.02 <- mpsi.03 <- mpsi.04 <- mpsi.05 <- mpsi.06 <- mpsi.07 <- 
mpsi.08 <- mpsi.09 <- mpsi.10 <- mpsi.11 <- mpsi.12 <- mpsi
j <- 0

evs <- data.frame()
for (yys in sort(unique(as.numeric(substr(yr[ss & pst.leu],2,5))))) {
    ss2 <- (substr(yr,2,5) %in% yys)
    mi.y <- pst.leu[ss & !ss2] 
    tu.y <- pst.leu[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ wbio02 + I(wbio02^2) + wbio07 + I(wbio07^2) + wbio17 + I(wbio17^2) +wbio12 + I(wbio12^2) + dtmp + dpre,avdat[ss & !ss2,],family=binomial)
    prd00 <- boot::inv.logit(rowSums(predict(lm00,avdat[ss & ss2,],"terms")))
    ev00 <- evaluate(prd00[tu.y],prd00[!tu.y])
    evs <- rbind(evs,data.frame(año=yys,npres=sum(tu.y),nabs=sum(!tu.y),
                                AUC=ev00@auc,COR=ev00@cor,
                                D2=mean(prd00[tu.y])-mean(prd00[!tu.y]),
                                COP=threshold(ev00,"no_omission")))
    
}
weighted.mean(evs$AUC,evs$npres)
cop <- with(subset(evs,COR>0),
            weighted.mean(COP,npres))
with(subset(evs,COR>0),weighted.mean(D2,npres))


prd <- 1940:2010
prd <- 1981:1995
prd <- 2010

for (k in prd) {
    cc <- seq(along=names(rdtmp))[as.numeric(substr(names(rdtmp),2,5)) %in% k]
    ss2 <- (substr(yr,2,5) %in% k)
    mi.y <- pst.leu[ss & !ss2] 
    tu.y <- pst.leu[ss & ss2] 
    lm00 <- glm(mi.y ~ wbio01 + I(wbio01^2) + wbio15 + I(wbio15^2) + wbio03 + I(wbio03^2)+ wbio02 + I(wbio02^2) + wbio07 + I(wbio07^2) + wbio17 + I(wbio17^2) +wbio12 + I(wbio12^2) + dtmp + dpre,avdat[ss & !ss2,],family=binomial)

    
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
   
    for (m in 1:12) {
        r08 <- raster(rdtmp,cc[m])
        r09 <- raster(rdpre,cc[m])
        vrt <- vrs
        for (x in 8:9) {
            rr <- sprintf("r%02d",x)
            dd <- disaggregate(get(rr),round(res(get(rr))[1]/res(r01)[1]))
            vrt <- addLayer(vrt,resample(dd,r01))    
        }
        names(vrt) <- c(names(vrs),"dtmp","dpre")
        psi <- raster(vrt,1)
        nwdt <- data.frame(values(vrt))
        nwdt$cphi <- cos(2*pi*as.numeric(m)/12)
        nwdt$sphi <- sin(2*pi*as.numeric(m)/12)

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
        points(aves[pst.leu,])
        title(main=k,sub=m)
    }
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

plot(mpsi.t3)

mpsis <- stack(mpsi.01,mpsi.02,mpsi.03,mpsi.04,mpsi.05,mpsi.06,
               mpsi.07,mpsi.08,mpsi.09,mpsi.10,mpsi.11,mpsi.12)

mpsi.p <- mpsi.a <- mpsi.01
values(mpsi.p) <- apply(values(mpsis),1,max)
values(mpsi.a) <- apply(values(mpsis),1,min)
plot(mpsi.p-mpsi.a)
 

plot(psi)
points(aves[pst.leu,],pch=3,cex=.5)
points(lat~lon,subset(NM.m1,Especieid %in% 297),col=4)



x01 <- extract(psi,NM.m1[,c("lon","lat")])
evaluate(x01[NM.m1$Especieid %in% 297],x01[!NM.m1$Especieid %in% 297])
threshold(evaluate(x01[NM.m1$Especieid %in% 297],x01[!NM.m1$Especieid %in% 297]))
    boxplot(evaluate(x01[NM.m1$Especieid %in% 297],x01[!NM.m1$Especieid %in% 297]))
plot(psi>0.93)
points(lat~lon,NM.m1)
