

x1 <- extract(WC,aves)
avdat <- data.frame(x1)
colnames(avdat) <- paste("w",colnames(x1),sep="")
avdat$arcilla <- extract(arcilla,aves)
avdat$pendiente <- extract(pendiente,aves)

qrv <- cellFromXY(ndvis,aves)
qrw <- cellFromXY(WC,aves)
qrx <- cellFromXY(arcilla,aves)
qry <- cellFromXY(rtmp,aves)
qrz <- cellFromXY(pendiente,aves)

col <- sprintf("X%s.%02d.",aves@data$year,aves@data$month)
yr <- sprintf("X%s",aves@data$year)
mes <- sprintf("%02d",aves@data$month)
nyr <- as.numeric(substr(yr,2,5))+(cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))[as.numeric(mes)] + 1)/365


ss <-  !is.na(aves@data$year) & !is.na(aves@data$month) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) ##& !is.na(qrv)
unq <- unique(data.frame(qry=qry[ss],col=col[ss]))
    ##match(e0$col[1],substr(colnames(e0),0,9))

    xt <- values(rtmp)
    xdt <- values(rdtmp)
    xp <- values(rpre)
    xdp <- values(rdpre)
    for (k in 1:nrow(unq)) {
        rr <- unq[k,"qry"]
        cc <- unq[k,"col"]
        mtc <- (qry %in% rr) & (col %in% cc)
        if (any(grepl(cc,names(rtmp)))) {
            avdat$tmp[mtc] <- xt[rr,grep(cc,names(rtmp))]
            avdat$dtmp[mtc] <- xdt[rr,grep(cc,names(rtmp))]
            avdat$pre[mtc] <- xp[rr,grep(cc,names(rtmp))]
            avdat$dpre[mtc] <- xdp[rr,grep(cc,names(rtmp))]
        }
    }

    gc()
    

    unq <- unique(data.frame(qry=qry[ss],col=yr[ss]))
    for (k in 1:nrow(unq)) {
        rr <- unq[k,"qry"]
        cc <- unq[k,"col"]
        mtc <- (qry %in% rr) & (yr %in% cc)
        if (any(grepl(cc,names(bio01)))) {
            for (j in 1:19) {
                lys <- sprintf("bio%02d",j)
                bb <- get(lys)
                avdat[mtc,paste("m",lys,sep="")] <-
                    values(bb)[rr,grep(cc,names(bb))]
            }
        }
    }

    gc()

    
    unq <- unique(data.frame(qry=qrw[ss],col=mes[ss]))
    xt <- values(wtmp)
    xp <- values(wpre)

    ##match(e0$col[1],substr(colnames(e0),0,9))
    for (k in 1:nrow(unq)) {
        rr <- unq[k,"qry"]
        cc <- unq[k,"col"]
        mtc <- (qrw %in% rr) & (mes %in% cc)
        if (any(grepl(cc,names(wtmp)))) {
            avdat$wtmp[mtc] <- xt[rr,grep(cc,names(wtmp))]
            avdat$wpre[mtc] <- xp[rr,grep(cc,names(wtmp))]
        }
    }

    
    avdat$ctmp <- (avdat$wtmp)/10 + avdat$dtmp 
    avdat$cpre <- (avdat$wpre)/10 + avdat$dpre

    avdat$cphi <- cos(2*pi*as.numeric(mes)/12)
    avdat$sphi <- sin(2*pi*as.numeric(mes)/12)


ss <-  !is.na(aves@data$year) & !is.na(aves@data$month) & !is.na(qry) & !is.na(qrw) & !is.na(qrx) & !is.na(qrz) & !is.na(qrv) & !is.na(nyr)
unq <- unique(data.frame(qry=qrv[ss],col=nyr[ss]))
##match(e0$col[1],substr(colnames(e0),0,9))

xt <- values(ndvis)
avdat$ndvi <- NA
gc()
for (k in 1:nrow(unq)) {
    rr <- unq[k,"qry"]
    cc <- unq[k,"col"]
    mtc <- (qrv %in% rr) & (nyr %in% cc)
    if (any(cc %in% ndvis.t)) {
        avdat$ndvi[mtc] <- xt[rr,grep(cc,ndvis.t)]
    }
}

gc()
    
