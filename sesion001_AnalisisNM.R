###########
## todo esto fue pasado a Documento1_PsittacidaeVenezuelaNM // nov. 2018 //JR
############
require(unmarked)
require(AICcmodavg)

lst.spp <- c("Amaz_amaz", "Amaz_barb", "Amaz_fari", "Amaz_ochr", "Ara_chlo",
             "Ara_maca", "Ara_mili", "Ara_seve", "Arat_acut", "Arat_leuc",
             "Arat_pert", "Arat_wagl", "Brot_chry", "Brot_jugu", "Diop_nobi",
             "Forp_pass", "Nann_pany", "Orth_mani", "Pion_barr", "Pion_mela",
             "Pion_mens", "Pyrr_egre", "Pyrr_hoem", "Pyrr_mela", "Pyrr_pict",
             "Pyrr_rhod")

 mexp$aspp <- sapply(as.character(mexp$spp),function(x) {y <- strsplit(x," ")[[1]]; paste(substr(y[1],1,4),substr(y[2],1,4),sep="_")})

mexp$aspp <- sub("Psit_acut","Arat_acut",mexp$aspp)
mexp$aspp <- sub("Psit_leuc","Arat_leuc",mexp$aspp)
mexp$aspp <- sub("Eups_pert","Arat_pert",mexp$aspp)
mexp$aspp <- sub("Psit_wagl","Arat_wagl",mexp$aspp)
mexp$aspp <- sub("Pyri_barr","Pion_barr",mexp$aspp)

lst.spp[!lst.spp %in% mexp$aspp]

no.removal <- F
for (spp in lst.spp) {

    if(!exists(sprintf("%s.fL",spp))) {
        mtz <- get(sprintf("%s.mtz",spp))
        if (no.removal) {
            ## alternativa sin removal design
            mtz <- cbind(mtz[,2],rowSums(mtz[,3:5],na.rm=T))
            obs <- cbind(obs1[,2:3])
            mtz[is.na(obs)] <- NA
            UMF <- unmarkedFrameOccu(mtz,
                                     siteCovs=muestreos,
                                     obsCovs=list(hora=obs,
                                         dur=col(obs)))
            UMF@obsCovs$dur <- c(3,9)[UMF@obsCovs$dur]
        ## seleccionar solo los puntos dentro del área de distribución esperada
        os <- seq(along=muestreos$NM)[muestreos$NM %in% unique(c(muestreos[rowSums(mtz,na.rm=T)>0,"NM"], as.character(mexp[mexp$aspp %in% tolower(spp),"NM"]))) & !is.na(mtz[,1])]

        } else {

            mtz[is.na(obs1)] <- NA
            UMF <- unmarkedFrameOccu(mtz[,-1],
                                     siteCovs=muestreos,
                                     obsCovs=list(hora=obs1[,-1]))
        ## seleccionar solo los puntos dentro del área de distribución esperada
            os <- seq(along=muestreos$NM)[
                muestreos$NM %in% unique(c(muestreos[rowSums(mtz[,-1],na.rm=T)>0,"NM"], as.character(mexp[tolower(mexp$aspp) %in% tolower(spp),"NM"]))) & !is.na(mtz$M1)]

        }

        
        ##        fm <- occu(~ hora ~ LSTd, UMF[ssO,])
        ##        os <- fm@sitesRemoved
        
        fL <- list("nulo"=try(occu(~ 1 ~ 1, UMF[os,])),
                   "p(h)Psi(.)"=try(occu(~ hora ~ 1, UMF[os,])),
                   ##"p(d)Psi(.)"=try(occu(~ dur ~ 1, UMF[os,])),
                   ##"p(dh)Psi(.)"=try(occu(~ dur+hora ~ 1, UMF[os,])),
                   ##"p(h2)Psi(.)"=try(occu(~ hora + I(hora^2) ~ 1, UMF[os,])),
                   
                   "p(.)Psi(V)"=try(occu(~ 1 ~ EVI+I(EVI^2), UMF[os,])),
                   ##"p(d)Psi(V)"=try(occu(~ dur ~ EVI+I(EVI^2), UMF[os,])),
                   "p(h)Psi(V)"=try(occu(~ hora ~ EVI+I(EVI^2) , UMF[os,])),
                   ##"p(dh)Psi(V)"=try(occu(~ dur+hora ~ EVI+I(EVI^2) , UMF[os,])),
                   ##"p(h2)Psi(V)"=try(occu(~ hora + I(hora^2) ~ EVI+I(EVI^2), UMF[os,])),
                   
                   "p(.)Psi(T)"=try(occu(~ 1 ~ PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),
                   "p(h)Psi(T)"=try(occu(~ hora ~ PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),
                   ##"p(d)Psi(T)"=try(occu(~ dur ~ PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),
            ##"p(dh)Psi(T)"=try(occu(~ dur+hora ~ PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),
            ##"p(h2)Psi(T)"=try(occu(~ hora + I(hora^2) ~ PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),

                   "p(.)Psi(VT)"=try(occu(~ 1 ~ EVI+I(EVI^2) + PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),
                   "p(h)Psi(VT)"=try(occu(~ hora ~ EVI+I(EVI^2) + PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,]))
                   ##"p(d)Psi(VT)"=try(occu(~ dur ~ EVI+I(EVI^2) + PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,])),
                   ##"p(dh)Psi(VT)"=try(occu(~ dur+hora ~ EVI+I(EVI^2) + PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,]))
            ##"p(h2)Psi(VT)"=try(occu(~ hora + I(hora^2) ~ EVI+I(EVI^2) + PET+I(PET^2) + LSTd+I(LSTd^2), UMF[os,]))
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


 ls(pattern="\\.ms$")

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

with(rslts,aggregate(AICw,list(mod),sum))
with(rslts,aggregate(AICw,list(mod),function(x) sum(!is.na(x))))
with(rslts,aggregate(AICw,list(spp),function(x) sum(!is.na(x))))

lst.spp[!lst.spp %in% rslts$spp]

##round(with(rslts,tapply(AICw,list(spp,mod),sum)),3)
tabla.AIC <- round(with(rslts,
                        tapply(AICw,
                               list(spp,
                                    factor(mod,
                                           levels=c("nulo","p(h)Psi(.)",
                                               "p(.)Psi(T)","p(h)Psi(T)",
                                               "p(.)Psi(V)","p(h)Psi(V)",
                                               "p(.)Psi(VT)","p(h)Psi(VT)")))
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

for (arch in ls(pattern="\\.pmtz$")) {
    png(file=sprintf("%s_detectabilidad.png",sub(".pmtz","",arch)),
        width=400,height=400,pointsize=16)
    par(mar=c(5,4,1,1))
    matplot(nwdt$hora,get(arch),axes=F,
            xlab="Hora del día",ylab="Probabilidad de detección",
            lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
    axis(2)
    axis(1,seq(0.25,1,length=8),seq(5,12,length=8))
    box()
    dev.off()
}

layout(matrix(1:10,ncol=2))
par(mar=c(5,4,1,1))

## rapid decay
matplot(nwdt$hora,Amaz_amaz.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Pion_mens.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Arat_leuc.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Ara_mili.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))

## nearly constant
matplot(nwdt$hora,Arat_pert.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Forp_pass.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Ara_maca.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Brot_jugu.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
## increasing
matplot(nwdt$hora,Pyrr_pict.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))


layout(matrix(1:10,ncol=2))
matplot(nwdt$hora,Diop_nobi.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))

matplot(nwdt$hora,Orth_mani.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Pion_barr.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Pyrr_mela.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))

matplot(nwdt$hora,Arat_wagl.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Pyrr_egre.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Pyrr_rhod.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))

## very low
matplot(nwdt$hora,Amaz_fari.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))
matplot(nwdt$hora,Ara_chlo.pmtz,lty=c(3,1,3),col=1,type="l",ylim=c(0,1))


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

png(file="Psi_previo_y_post.png",bg="aliceblue",width=700,height=500,pointsize=14)
layout(1:3)
par(mar=c(0.5,5,0.5,0.5),oma=c(8,0,0,0))
boxplot((1-p.aus)~spp,dprob,las=2,ylim=c(0,1),axes=F,ylab="p*")##ylab=expression(p==(1-Pi[j=1]^K (1-p_j))))
box()
abline(h=c(0.2),lty=3)
axis(2)
boxplot(pre.Psi~spp,dprob,las=2,ylim=c(0,1),axes=F,ylab=expression(hat(Psi)))
axis(2)
abline(h=c(0.2),lty=3)
box()
boxplot(post.Psi~spp,dprob,las=2,ylim=c(0,1),ylab=expression(Psi[condl]))
abline(h=c(0.2),lty=3)
dev.off()


sim.prob <- data.frame()
for (k in 1:nrow(tt)) {
    fL <- get(sprintf("%s.fL",tt[k,"spp"]))
    y <- rowSums(fL[[as.character(tt[k,"mod"])]]@data@y,na.rm=T)
    pre.Psi <- predict(fL[[as.character(tt[k,"mod"])]],"state")[,1]

    maxcol <- 8
    for (maxhr in c(9:12)) {
        nwdt <- data.frame(hora=runif(length(y)*maxcol,5.5/24,maxhr/24))
        m0 <-matrix(predict(fL[[as.character(tt[k,"mod"])]],"det",
                            newdata=nwdt)$Predicted,ncol=maxcol,byrow=T)
        for (ncol in 2:maxcol) {
            p.new <- apply(1-m0[,1:ncol],1,prod)
            new.Psi <- (pre.Psi*p.new)/(pre.Psi*p.new + (1-pre.Psi))
            sim.prob <- rbind(sim.prob,data.frame(spp=tt[k,"spp"],
                                                  hora.max=maxhr,
                                                  n.mst=ncol,
                                                  post.Psi=new.Psi[y==0]))
        }
    }
}
sim.prob <- rbind(sim.prob,data.frame(spp=dprob$spp,
                       hora.max=0,
                       n.mst=0,
                       post.Psi=dprob$post.Psi))
boxplot(post.Psi~sprintf("%02d %02d",hora.max,n.mst),sim.prob)
abline(h=0.05,lty=3)
abline(h=0.1425,lty=3)


##
mm <- table(mexp$aspp)
mm[!names(mm) %in% tt$spp]

with(subset(mexp,!aspp %in% tt$spp),aggregate(freq,list(spp=spp),sum))



## el muestreo actual representa 1.6 dias de muestreo




    matrix(,ncol=8,byrow=T)
                       fL[[as.character(tt[k,"mod"])]]@data@obsCovs)
    s1 <- seq(1,nrow(nwdt),by=4)
    s2 <- seq(2,nrow(nwdt),by=4)
    s3 <- seq(3,nrow(nwdt),by=4)
    s4 <- seq(4,nrow(nwdt),by=4)
    h1 <- nwdt$hora[s1]
    h2 <- nwdt$hora[-seq(1,nrow(nwdt),by=4)]
    nwdt$hora <- NA
    nwdt$hora[s1] <- h1
    nwdt$hora[s2] <- rev(h1)
    nwdt$hora[s3] <- h1
    nwdt$hora[s4] <- rev(h1)


        post.Psi <- (pre.Psi*p.aus)/(pre.Psi*p.aus + (1-pre.Psi))
boxplot(new.Psi~spp,dprob,las=2,ylim=c(0,1))
abline(h=0.05,lty=3)

 sum(dprob$post.Psi < 0.05,na.rm=T)
sum(dprob$new.Psi[!is.na(dprob$new.Psi)] < 0.05,na.rm=T)



cbind(tt[,1:4],fq)
boxplot(bup(re, stat="mean")~Amaz_barb.fL[[1]]@data@siteCovs$NM)
abline(h=0.05)

table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] ) ##95% CI

subset(rslts,Delta.AICc==0 & grepl("V",mod))

vWC <- data.frame(values(mVars))
##colnames(vWC) <- sprintf("wbio%02d",1:19)
ss <- rowSums(is.na(vWC))==0

    mu <- mean(muestreos[,"LST_Day_1km"],na.rm=T)
    sg <- sd(muestreos[,"LST_Day_1km"],na.rm=T)
    vWC[,"LSTd"] <- (vWC[,"LSTd"]-mu)/sg
    mu <- mean(muestreos[,"PET_1km"],na.rm=T)
    sg <- sd(muestreos[,"PET_1km"],na.rm=T)
    vWC[,"PET"] <- (vWC[,"PET"]-mu)/sg
    mu <- mean(muestreos[,"v250m_16_days_EVI"],na.rm=T)
    sg <- sd(muestreos[,"v250m_16_days_EVI"],na.rm=T)
    vWC[,"EVI"] <- (vWC[,"EVI"]-mu)/sg




##prdPsi <- modavgPred(Amzo.fL,names(Amzo.fL),newdata=vWC[ss,], type = "link", parm.type = "psi")

##prdPsi <- predict(Amzo.fL[["p(h)Psi(wc)"]],newdata=vWC[ss,],type="state")


## tarda mucho
##       prdPsi <- predict(Amzo.fL[["p(.)Psi(VT)"]],newdata=vWC[ss,],type="state")

##prdPsi <- modavgPred(Amzo.fL,names(Amzo.fL),newdata=vWC[ss,], type = "link", parm.type = "psi")


for (mdl in c("p(.)Psi(VT)","p(h)Psi(VT)")) {
    ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.4)
    for (k in 1:nrow(ssr)) {
        psi <- raster(mVars,1)*NA
        ##values(psi)[ss] <- prdPsi$Predicted
        fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
        values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1,EVI,EVI^2,PET,PET^2,LSTd,LSTd^2))))
        assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
    }
}

for (mdl in c("p(.)Psi(V)","p(h)Psi(V)")) {
    ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
    for (k in 1:nrow(ssr)) {
        psi <- raster(mVars,1)*NA
        ##values(psi)[ss] <- prdPsi$Predicted
        fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
        values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1,EVI,EVI^2))))
        assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
    }
}


require(RColorBrewer)
subset(rslts,Delta.AICc==0 & grepl("V",mod))

plot(Diop_nobi.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")),main="Diop. nobi.")
 plot(Pion_mens.phPsiV,breaks=seq(0,1,length=11),col=c("grey77",brewer.pal(9,"Oranges")),main="Pionus menstruus")
##plot(Pion_mens.phPsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))


plot(Pyrr_pict.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
plot(Brot_jugu.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
plot(Arat_pert.p.PsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
plot(Ara_seve.p.PsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))
plot(Ara_mili.phPsiV,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))




plot(Amaz_ochr.p.PsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Greens")))

par(mar=c(2,2,0,0))
plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
image(Amaz_amaz.p.PsiVT,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Oranges")),ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))

##
plot(shp,add=T,lwd=5)
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)==0,c("V2","V1")],col="cyan",pch=1,cex=.5)
points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)

for (k in c("Amazona amazonica","Amazona ochrocephala","Ara militaris",
            "Ara severus","Eupsittula pertinax","Brotogeris jugularis",
            "Pionus menstruus","Pyrrhura picta","Diopsittaca nobilis")) {
    
    slc <- match(k,rsm.Loros[,1])
    shp <- crop(shapefile(sprintf("~/gisdata/distribuciones/BirdLife/%s.shp",rsm.Loros[slc,"BL.shp"])),e)
    mdl <- get(sprintf("%s.%s",rsm.Loros[slc,"acronimo"],
                       gsub("[\\(\\)]","",
                            subset(rslts,Delta.AICc==0 & grepl("V",mod) & spp %in% rsm.Loros[slc,"acronimo"])$mod)))

    assign(sprintf("%s.val",rsm.Loros[slc,"acronimo"]),
           values(rasterize(shp,mdl)))

    png(file=sprintf("Pred_%s.png",rsm.Loros[slc,"acronimo"]),bg="aliceblue",width=600,height=500,pointsize=14)
    plot(mdl,breaks=seq(0,1,length=11),col=c(NA,brewer.pal(9,"Oranges")),main=rsm.Loros[slc,1],    sub=sprintf("COR = %0.3f",cor(values(mdl),!is.na(get(sprintf("%s.val",rsm.Loros[slc,"acronimo"]))),
        use="complete"))
)
    plot(shp,add=T)
    plot(vz0,lty=3,add=T)
dev.off()

png(file=sprintf("Boxplot_%s.png",rsm.Loros[slc,"acronimo"]),bg="aliceblue",width=400,height=400,pointsize=14)
boxplot(values(mdl)~!is.na(get(sprintf("%s.val",rsm.Loros[slc,"acronimo"]))),notch=T)    
   dev.off() 
}


    par(mar=c(2,2,0,0))
    plot(vz0,ylim=c(0,13),xlim=c(-74,-58),pty="n")
    image(Amaz_amaz.p.PsiVT,,ylab="",xlab="",axes=F,add=T)
axis(2,seq(0,14,by=2),sprintf("%02s°N",seq(0,14,by=2)))
axis(1,seq(-74,-58,by=2),sprintf("%02s°W",abs(seq(-74,-58,by=2))))

    plot(shp,add=T,lwd=5)
plot(vz0,add=T,border="grey13")
points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)==0,c("V2","V1")],col="cyan",pch=1,cex=.5)
points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)



cor(values(Amaz_ochr.p.PsiVT),!is.na(Amaz_ochr.val),use="complete")

cor.test(as.vector(values(Amaz_ochr.p.PsiVT)),
    as.vector(!is.na(Amaz_ochr.val)),use="complete")

boxplot(values(Amaz_amaz.p.PsiVT)~!is.na(Amaz_amaz.val),notch=T)
boxplot(values(Amaz_ochr.p.PsiVT)~!is.na(Amaz_ochr.val),notch=T)
boxplot(values(Ara_seve.p.PsiV)~!is.na(Ara_seve.val),notch=T)
boxplot(values(Ara_mili.phPsiV)~!is.na(Ara_mili.val),notch=T)
boxplot(values(Diop_nobi.p.PsiV)~!is.na(Diop_nobi.val),notch=T)
boxplot(values(Pion_mens.phPsiVT)~!is.na(Pion_mens.val),notch=T)
boxplot(values(Pyrr_pict.p.PsiV)~!is.na(Pyrr_pict.val),notch=T)
boxplot(values(Brot_jugu.p.PsiV)~!is.na(Brot_jugu.val),notch=T)
boxplot(values(Arat_pert.p.PsiVT)~!is.na(Arat_pert.val),notch=T)


for (mdl in c("p(.)Psi(T)","p(h)Psi(T)")) {
    ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
    for (k in 1:nrow(ssr)) {
        psi <- raster(mVars,1)*NA
        ##values(psi)[ss] <- prdPsi$Predicted
        fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
        values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1,PET,PET^2,LSTd,LSTd^2))))
        assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
    }

}

for (mdl in c("nulo","p(h)Psi(.)")) {
    ssr <- subset(rslts,mod %in% c(mdl) & AICw>0.001)
    for (k in 1:nrow(ssr)) {
        psi <- raster(mVars,1)*NA
        ##values(psi)[ss] <- prdPsi$Predicted
        fL <- get(sprintf("%s.fL",ssr[k,"spp"]))
        values(psi)[ss] <- boot::inv.logit(colSums(coef(fL[[mdl]],"state")*with(vWC[ss,],rbind(1))))
        assign(sprintf("%s.%s",ssr[k,"spp"],gsub("[()]","",mdl)),psi)
    }
}

ls(pattern="Psi")

##computes the conditional distribution of occurrence given the data and the estimates of the fixed effects, Pr(z(i)=1 | y(i,j), psi(i), p(i,j))
##Empirical Bayes methods can underestimate the variance of the     posterior distribution because they do not account for uncertainty     in the hyperparameters (lambda or psi). Eventually, we hope to add     methods to account for the uncertainty of the hyperparameters.
##     Note also that the posterior mode appears to exhibit some bias as     an estimator or abundance. Consider using the posterior mean     instead, even though it will not be an integer in general. More     simulation studies are needed to evaluate the performance of     empirical Bayes methods for these models.

re <- ranef(Amaz_amaz.fL[["p(h)Psi(T)"]])
 ## Extract all values in convenient formats
post.df <- as(re, "data.frame")
head(post.df)
post.arr <- as(re, "array")
     

## Best Unbiased Predictors
##bup(re, stat="mean")           # Posterior mean
table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] ) ##95% CI

(re <- ranef(Amaz_barb.fL[["p(h)Psi(T)"]]))
     # Best Unbiased Predictors
     bup(re, stat="mean")           # Posterior mean


 re <- ranef(Amaz_ochr.fL[["p(.)Psi(VT)"]])
## Best Unbiased Predictors
##   bup(re, stat="mean")           # Posterior mean

table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] ) ##95% CI

  (re <- ranef(Amaz_barb.fL[["p(h)Psi(T)"]]))
     # Best Unbiased Predictors
     bup(re, stat="mean")           # Posterior mean

table(bup(re, stat="mean")> 0.95)
table(bup(re, stat="mean")< 0.05)
boxplot(bup(re, stat="mean")~Amaz_barb.fL[[1]]@data@siteCovs$NM)
abline(h=0.05)

table(confint(re, level=0.95)[,1] ,confint(re, level=0.95)[,2] )## 95% CI

    
for (spps in c("Amaz_amaz","Amaz_ochr","Ara_seve","Arat_pert","Forp_pass","Pion_mens","Brot_jugu","Amaz_fari","Pyrr_pict"))
    print(sprintf("%s.Psi <- %s",spps,with(subset(rslts,spp %in% spps & AICw>0.001),paste(sprintf("(%s.%s * %s)",spp,gsub("[()]","",mod),AICw),collapse="+"))))

Amaz_amaz.Psi <- (Amaz_amaz.phPsiT * 0.592)+(Amaz_amaz.phPsiVT * 0.408)
Amaz_ochr.Psi <- (Amaz_ochr.p.PsiVT * 0.968)+(Amaz_ochr.p.PsiT * 0.031)
Ara_seve.Psi <- (Ara_seve.nulo * 0.724)+(Ara_seve.p.PsiV * 0.276)
Arat_pert.Psi <- (Arat_pert.p.PsiVT * 0.632)+(Arat_pert.phPsiVT * 0.251)+(Arat_pert.p.PsiT * 0.084)+(Arat_pert.phPsiT * 0.032)
Forp_pass.Psi <- (Forp_pass.p.PsiT * 0.417)+(Forp_pass.phPsiT * 0.279)+(Forp_pass.p.PsiVT * 0.087)+(Forp_pass.nulo * 0.069)+(Forp_pass.phPsiVT * 0.057)+(Forp_pass.phPsi. * 0.051)+(Forp_pass.p.PsiV * 0.025)+(Forp_pass.phPsiV * 0.015)
Pion_mens.Psi <- (Pion_mens.phPsi. * 0.801)+(Pion_mens.phPsiV * 0.125)+(Pion_mens.phPsiT * 0.075)
Brot_jugu.Psi <- (Brot_jugu.p.PsiV * 0.377)+(Brot_jugu.p.PsiVT * 0.175)+(Brot_jugu.p.PsiT * 0.166)+(Brot_jugu.phPsiV * 0.14)+(Brot_jugu.phPsiVT * 0.061)+(Brot_jugu.phPsiT * 0.059)+(Brot_jugu.nulo * 0.016)+(Brot_jugu.phPsi. * 0.006)
Amaz_fari.Psi <- (Amaz_fari.p.PsiT * 0.774)+(Amaz_fari.nulo * 0.133)+(Amaz_fari.p.PsiVT * 0.093)
Pyrr_pict.Psi <- (Pyrr_pict.p.PsiV * 0.668)+(Pyrr_pict.phPsiV * 0.332)


subset(rslts,spp %in% "Arat_pert" & AICw>0.001)


png(file="Aratinga_pertinax_psi.png")
plot(Arat_pert.Psi,main="Eupsittula pertinax")
points(muestreos[rowSums(Arat_pert.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
points(muestreos[rowSums(Arat_pert.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
dev.off()

png(file="Ara_severus_psi.png")
plot(Ara_seve.Psi,main="Ara severus")
points(muestreos[rowSums(Ara_seve.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
points(muestreos[rowSums(Ara_seve.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
shp <- shapefile("~/gisdata/distribuciones/BirdLife/Ara_severus_22685577.shp")
plot(shp,add=T)
dev.off()

png(file="Amazona_amazonica_psi.png")
plot(Amaz_amaz.Psi,main="Amazona amazonica",col=brewer.pal(9,"Greens"))
points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
points(muestreos[rowSums(Amaz_amaz.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
dev.off()

png(file="Amazona_farinosa_psi.png")
plot(Amaz_fari.Psi,main="Amazona farinosa",col=brewer.pal(9,"Greens"))
points(muestreos[rowSums(Amaz_fari.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
points(muestreos[rowSums(Amaz_fari.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
dev.off()

require(RColorBrewer)
shp <- shapefile("~/gisdata/distribuciones/BirdLife/Amazona_ochrocephala_22686346.shp")

png(file="Amazona_ochrocephala_psi.png",width=500,height=400)
plot(Amaz_ochr.Psi,main="Amazona ochrocephala",col=brewer.pal(9,"Spectral"),ylim=c(0,13))
plot(vz0,add=T,border="grey33")
points(muestreos[rowSums(Amaz_ochr.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=1,pch=19,cex=.5)
points(muestreos[rowSums(Amaz_ochr.mtz[,-1],na.rm=T)>0,c("V2","V1")],col="purple",pch=3)
##plot(shp,add=T)

dev.off()

shp <- shapefile("~/gisdata/distribuciones/BirdLife/Pionus_menstruus_45429607.shp")

png(file="Pionus_menstruus_psi.png",width=900,height=440,pointsize=18)
layout(matrix(1:2,ncol=2))
par(mar=c(1,0,1,5),oma=c(0,0,3,0))
plot(vz0,ylim=c(0,13),border="darkgreen",main="IUCN, 2014")
plot(shp,col=rgb(.1,.1,.9,.7),add=T)
points(aves[aves@data$species %in% "Pionus menstruus",],col=2,pch=.7)

plot(Pion_mens.Psi,col=brewer.pal(9,"Greens"),
     ##breaks=round(seq(0,1,length=10),2),
     ylim=c(0,13),axes=F,main="NeoMapas + Occupancy model")
plot(vz0,add=T)
points(muestreos[rowSums(Pion_mens.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
points(muestreos[rowSums(Pion_mens.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)


mtext("Pionus menstruus",3,cex=2.7,outer=T,font=3)
dev.off()


dev.off()

png(file="Brotogeris_jugularis_psi.png")

plot(Brot_jugu.Psi,main="Brotogeris jugularis",col=brewer.pal(9,"Greens"))
points(muestreos[rowSums(Brot_jugu.mtz[,-1],na.rm=T)==0,c("V2","V1")],col=2,pch=1,cex=.5)
points(muestreos[rowSums(Brot_jugu.mtz[,-1],na.rm=T)>0,c("V2","V1")],col=1,pch=3)
shp <- shapefile(sprintf("%s/%s",SEDAC.mptc,"brot_jugu_pl.shp"))
plot(shp,add=T)

dev.off()



rowSums(tabla.AIC[,grep("h",colnames(tabla.AIC))],na.rm=T)
rowSums(tabla.AIC[,grep("V",colnames(tabla.AIC))],na.rm=T)
rowSums(tabla.AIC[,grep("T",colnames(tabla.AIC))],na.rm=T)


### para ver si la falta de detección provee información confiable sobre su ausencia hay que calcular para cada punto (según hora etc...) estos valores


## lista
## no-detectado vs detectado
## no-detectado: no esperado (escaso o distribución restringida) vs esperado
## esperado: baja detectabilidad o ausente
## detectado: abundante/común vs. escaso o restringido 
## restringido por clima o vegetación


## probabilidad de detección después de varias visitas a un sitio:
##Probabilidad de no detección
##gP <- getP(Arat_pert.fL[[3]])
gP <- obs1[,-1]^0 * 0.3
gP[is.na(obs1[,-1])] <- NA
##gP <- getP(Arat_pert.fL[["p(.)Psi(VT)"]])
##gP[is.na(obs1[os,-1])] <- NA
preF <- apply(gP,1,function(x) prod(1-x,na.rm=T))

## si estamos 25% seguros de la presencia...
prePsi <- 0.5

postPsi <- prePsi*preF/(prePsi*preF + (1-prePsi))
boxplot(postPsi)


tapply(mexp$NM,mexp$spp,luq)

boxplot(with(subset(rslts,mod %in% "nulo"),dtt/n))

shp <- shapefile("~/gisdata/distribuciones/BirdLife/Pyrilia_caica_22686136.shp")
plot(vz0,border="grey33")
plot(shp,add=T)
shp <- shapefile("~/gisdata/distribuciones/BirdLife/Pionus_fuscus_22686198.shp")
plot(shp,add=T)
shp <- shapefile("~/gisdata/distribuciones/BirdLife/Deroptyus_accipitrinus_22686416.shp")
plot(shp,add=T)

res <- data.frame()
for (j in 1:nrow(rslts)) {
    fL <- get(sprintf("%s.fL",rslts[j,"spp"]))
    mdl <- fL[[as.character(rslts[j,"mod"])]]
    re <- ranef(mdl)
    ci <- confint(re, level=0.95)
    res <- rbind(res,data.frame(
                         spp=rslts[j,"spp"],
                         mod=rslts[j,"mod"],
                         aus=sum(ci[,1]==0 & ci[,2]==0 ),
                         q.aus=sum(ci[,1]==0 & ci[,2]==1 ),
                         pres=sum(ci[,1]==1 & ci[,2]==1 )))


}
bst.res <- cbind(res[rslts$Delta.AICc==0,],rslts[rslts$Delta.AICc==0,c("n","dtt")])
write.csv(file="TablaDeteccionesPrediccionPsittacidae.csv",bst.res)
write.csv(file="TablaModelosPsittacidae.csv",rslts)
sum(bst.res$aus)/sum(bst.res$n-bst.res$dtt)
