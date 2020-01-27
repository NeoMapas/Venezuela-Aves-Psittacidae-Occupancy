tre.acu <- grepl("Aratinga acuti",aves@data$species)

plot(aves)
points(aves[tre.acu,],col=3)

ss <- !is.na(yr) & !is.na(mes) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(avdat$pre) & !is.na(avdat$wpre) & !is.na(avdat$pendiente) & !is.na(avdat$arcilla) & !duplicated(data.frame(qrw,qrx,qry,qrz,tre.acu,mes,col,yr))
y <- tre.acu[ss]


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
