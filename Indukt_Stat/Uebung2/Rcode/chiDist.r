# http://davegiles.blogspot.de/p/code.html
# https://www.r-bloggers.com/central-and-non-central-distributions/
# http://web.uvic.ca/~dgiles/blog/noncentral_chisq.R
# https://plot.ly/r/text-and-annotations/

# crit<-qchisq(0.95, df=3)
# x <- seq(from=0, to=20, length.out=200)           
# ylim <- c(0, 0.25)
# plot(x, dchisq(x,df=3), main=paste("Non-Central Chi-Square Densities", "\n(v = 3)"), type='l', ylim=ylim)
# lines(x, dchisq(x,df=3, ncp=1),type='l', ylim=ylim, col="2")
# lines(x, dchisq(x,df=3, ncp=5),type='l', ylim=ylim, col="4")
# abline(v = crit, col = "3")
# legend(x=10,y=0.2, c("lambda = 0", "lambda = 1", "lambda = 5"),cex=.8, 
#        col=c(1,2,4), lty=c(1,1,1))
# text(crit, -0.004, paste("crit"))

setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Uebung2")
df<-1
pdf("chi2.pdf")
crit<-qchisq(0.95, df=df)
x <- seq(from=0, to=10, length.out=200)           
ylim <- c(0, 0.75)
plot(x, dchisq(x,df=1), main=expression(paste(chi^2,"-Dichte bei df=1",sep="")), 
     type='l', ylim=ylim, ylab="Dichtefunktion",xlab="",lwd=2,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
#lines(x, dchisq(x,df=3, ncp=1),type='l', ylim=ylim, col="2")
#lines(x, dchisq(x,df=3, ncp=5),type='l', ylim=ylim, col="4")
abline(v = crit, col = "3",lwd=2)
#legend(x=10,y=0.2, c("lambda = 0", "lambda = 1", "lambda = 5"),cex=.8, 
       #col=c(1,2,4), lty=c(1,1,1))
text(crit+0.5, 1.4, paste("k"),cex=1.5)
dev.off()
