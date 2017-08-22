plotGamma.dens <- function(shape=2, rate=0.5, to=0.99, p=c(0.1, 0.9), cex=1.5, ...){
  to <- qgamma(p=to, shape=shape, rate=rate)
  curve(dgamma(x, shape, rate), from=0, to=to, n=500, type="l", 
        main=sprintf("", shape, rate),#gamma.dens(x, shape=%1.2f, rate=%1.2f)
        bty="n", xaxs="i", yaxs="i", col="forestgreen", xlab="", ylab="", 
        las=1, lwd=2, cex=cex, cex.axis=cex, cex.main=cex, ...)
  gx <- qgamma(p=p,  shape=shape, rate=rate)
  gy <- dgamma(x=gx, shape=shape, rate=rate)
  for(i in seq_along(p)) { lines(x=rep(gx[i], 2), y=c(0, gy[i]), col="darkblue") }
  for(i in seq_along(p)) { text(x=gx[i], 0, p[i], adj=c(1.1, -0.2), cex=cex) }
}
# gamma.cdf(x, shape=%1.2f, rate=%1.2f)
plotGamma.cum <- function(shape=2, rate=0.5, to=0.99, p=c(0.1,0.9), cex=1.5, ...){
  to <- qgamma(p=to, shape=shape, rate=rate)
  curve(pgamma(x, shape, rate), from=0, to=to, n=500, type="l", 
        main=sprintf("", shape, rate),
        bty="n", xaxs="i", yaxs="i", col="red", xlab="", ylab="", ylim=c(0,1),
        las=1, lwd=2, cex=cex, cex.axis=cex, cex.main=cex, ...)
  #gx <- qgamma(p=p,  shape=shape, rate=rate)
  gx<- qgamma(p=p,  shape=shape, rate=rate)
  gy <- pgamma(q=gx, shape=shape, rate=rate)
  for(i in seq_along(p)) { lines(x=rep(gx[i], 2), y=c(0, gy[i]), col="darkblue") }
  for(i in seq_along(p)) { text(x=gx[i], 0, p[i], adj=c(1.1, -0.2), cex=cex) }
}
#plotGamma.dens(6, 0.1)
#plotGamma.cum(6, 0.1)

# Einstieg in die nächste Übung
#pdf("dens_cdf.pdf")
#par(mfrow=c(2,1))
#plotGamma.cum(shape=7, rate=2/3,p=c(0.25,0.5,0.75,0.9))
#plotGamma.dens(shape=7, rate=2/3,p=c(0.25,0.5,0.75,0.9))
#dev.off()

#pdf("cdf.pdf")
#par(mfrow=c(1,1))
#plotGamma.cum(shape=7, rate=2/3,p=c(0.25,0.5,0.75,0.9))
#plotGamma.dens(shape=7, rate=2/3)
#dev.off()
# http://www.bib-demografie.de/DE/ZahlenundFakten/02/Abbildungen/abbildungen_node.html
#qgamma(0.1,shape=7, rate=2/3)

plotGauss.dens <- function(mean=0, sd=1.0, to1=0.999, p=c(0.05,0.25,0.75, 0.95), cex=1.5,
                           cex.main=1.75,cex.axis=1.75,cex.lab=1.75,...){
  to <- qnorm(p=to1, mean=mean, sd=sd)
  from<-qnorm(p=1-to1, mean=mean, sd=sd)
  curve(dnorm(x, mean, sd), from=from, to=to, n=500, type="l", 
        main=sprintf("Dichte", mean, sd),#gamma.dens(x, shape=%1.2f, rate=%1.2f)
        bty="n",axes = FALSE, col="red", xlab="", ylab="", #
        las=1, lwd=2, cex=cex, cex.axis=cex, cex.main=cex, ...)
  axis(side = 1, at = seq(0,80,10),cex=cex, cex.axis=cex.axis, cex.lab=cex.lab)
  gx <- qnorm(p=p,  mean=mean, sd=sd)
  gy <- dnorm(x=gx, mean=mean, sd=sd)
  for(i in seq_along(p)) { lines(x=rep(gx[i], 2), y=c(0, gy[i]), col="darkblue") }
  for(i in seq_along(p)) { text(x=gx[i], 0, p[i], adj=c(1.1, -0.2), cex=cex) }
}
# gamma.cdf(x, shape=%1.2f, rate=%1.2f)
plotGauss.cdf <- function(mean=0, sd=1.0, to1=0.999, p=c(0.05,0.25,0.75, 0.95), cex=1.5, 
                          cex.main=1.75,cex.axis=1.75,cex.lab=1.75, ...){
  to <- qnorm(p=to1, mean=mean, sd=sd)
  from<-qnorm(p=1-to1, mean=mean, sd=sd)
  curve(pnorm(x, mean=mean, sd=sd), from=from, to=to, n=500, type="l", 
        main=sprintf("Verteilungsfunktion", mean, sd),
        bty="n", axes = FALSE, col="darkgreen", xlab="", ylab="", ylim=c(0,1),
        las=1, lwd=2, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, ...)
  axis(side = 1, at = seq(0,100,10), cex.axis=cex.axis, cex.lab=cex.lab)
  axis(side = 2, at = seq(0,1,0.1), cex.axis=cex.axis, cex.lab=cex.lab)
  #gx <- qgamma(p=p,  shape=shape, rate=rate)
  gx<- qnorm(p=p,  mean=mean, sd=sd)
  gy <- pnorm(q=gx, mean=mean, sd=sd)
  for(i in seq_along(p)) { lines(x=rep(gx[i], 2), y=c(0, gy[i]), col="darkblue") }
  for(i in seq_along(p)) { text(x=gx[i], 0, p[i], adj=c(1.1, -0.2), cex=cex) }
}
mean75 <- 37.2
sd75 <-20

#setwd("DeskrStatistik/Übung/Übung2")
pdf("normDens_1975.pdf")
plotGauss.dens(mean=mean75, sd=sd75,cex=2.0)
abline(v=mean75,lty=2,col="grey")
dev.off()

pdf("normCDF_1975.pdf")
plotGauss.cdf(mean=mean75, sd=sd75,cex=2.0)
#abline(v=mean75,lty=2,col="grey")
abline(h=c(0.05,0.25,0.5,0.75,0.95),lty=2,col="grey")
dev.off()

pdf("normDens_2015.pdf")
plotGauss.dens(mean=44, sd=sd75,cex=2.0)
abline(v=44,lty=2,col="grey")
dev.off()

pdf("normDens_2015B.pdf")
plotGauss.dens(mean=44, sd=sd75,cex=2.0)
abline(v=44,lty=2,col="grey")
abline(v=c(22,66),lty=2)
dev.off()

pdf("normCDF1.pdf")
plotGauss.cdf(mean=mean, sd=sd,cex=2.0)
dev.off()


pdf("norm_CDF_DENS.pdf")
par(mfrow=c(2,1))
plotGauss.dens(mean=mean, sd=sd)
plotGauss.cdf(mean=mean, sd=sd)
dev.off()

qnorm(0.05, mean=mean, sd=sd)
# 27.64377
round(qnorm(c(0.05,0.25,0.75,0.95), mean=mean, sd=sd),2)
# 27.64 43.01 64.37 79.73

round(qnorm(0.5, mean=mean, sd=sd),2)

15.8/53.7