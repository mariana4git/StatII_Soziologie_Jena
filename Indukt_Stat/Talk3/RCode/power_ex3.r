# Plot sample size curves for detecting correlations of
# various sizes.
# http://www.statmethods.net/stats/power.html
library(pwr)

# range of correlations
#r <- seq(0.1,1.0,0.1)
r <- seq(-0.2,0.2,0.01)
nr <- length(r)

# power values
n <- seq(1000,2500,500)
np <- length(n)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.t.test(n = n[i], d = r[j],
                         sig.level = .05, power = NULL,
                         alternative = "less",type="two")
    samsize[j,i] <- round(result$power,4)
  }
}
# http://astrostatistics.psu.edu/su07/R/html/grDevices/html/plotmath.html
# set up graph
xrange <- range(r)
yrange <- range(samsize)
colors <- rainbow(length(n))
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk3")
pdf("powerPisa.pdf")
plot(xrange, yrange, type="n",
     xlab=expression(delta),
     ylab="Power (p)" ,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.2), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.2), lty=2,
       col="grey89")
abline(v=-0.1, col="red", lty=2)
abline(v=0.1, col="red", lty=2)
abline(v=-0.05, col="darkgreen", lty=2)
abline(v=0.05, col="darkgreen", lty=2)
title("Power, Sig=0.05 (one-tailed)",cex=1.5)
legend("top", title="Sample Size", as.character(n),
       fill=colors)
dev.off()

# pwr.t.test(n = 20, d = 10,
#            sig.level = .05, power = NULL,
#            alternative = "two.sided",type="one.sample")
# 
# pwr.t.test(d=d,n=40,sig.level=0.05,type="paired",alternative="two.sided")
