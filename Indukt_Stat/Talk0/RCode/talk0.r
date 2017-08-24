# https://www.kiggs-studie.de/fileadmin/KiGGS-Dokumente/kiggs_tn_broschuere_web.pdf
# https://cran.r-project.org/web/packages/readstata13/readstata13.pdf
# https://stackoverflow.com/questions/10535235/generate-correlated-random-numbers-from-binomial-distributions-in-r
# https://stackoverflow.com/questions/16089178/how-to-simulate-correlated-binary-data-with-r
# Jungs 14% und Mädchen 21%
# Simuliere Taschengeld von 100 Kindern
set.seed(1)
rnorm(100,mean=12,sd=3)->x



# Summe von drei mean=36, sd=sqrt(27)
pnorm(12,mean=36, sd=sqrt(27))
pnorm(12,mean=12, sd=sqrt(9))*pnorm(12,mean=12, sd=sqrt(9))
#http://zoonek2.free.fr/UNIX/48_R/03.html
# mean von 10
pnorm(10,mean=12,sqrt(0.9))

# N <- 2000
# x <- rnorm(N)
# op <- par(mar=c(0,0,0,0), oma=c(0,0,0,0)+.1)
# layout(matrix(c(1,1,1,2), nc=1))
# y <- ppoints( length(x) )
# plot(sort(x), y, type="l", lwd=3,
#      xlab="", ylab="", main="")
# abline(h=c(0,.25,.5,.75,1), lty=3)
# abline(v = quantile(x), col = "blue", lwd = 3, lty=2)
# points(quantile(x), c(0,.25,.5,.75,1), lwd=10, col="blue")
# boxplot(x, horizontal = TRUE, col = "pink", lwd=5)  
# abline(v = quantile(x), col = "blue", lwd = 3, lty=2)
# par(new=T)
# boxplot(x, horizontal = TRUE, col = "pink", lwd=5)  
# par(op)

pdf("taschengeld_box.pdf")
N <- 100#2500
set.seed(1)
x <- rnorm(N,mean=12,sd=3)
op <- par(mar=c(0,0,0,0), oma=c(0,0,0,0)+.1)
layout(matrix(c(1,1,1,2), nc=1))
y <- ppoints( length(x) )
plot(round(sort(x)), y, type="s", lwd=3,
     xlab="Taschengeld von Grundschulkindern", ylab="Anteil der Kinder", 
     main="",cex.axis=2.5,axes = FALSE,cex.lab=1.5)
axis(side = 1, at = seq(0,34,2))
axis(side = 2, at = c(0,0.25,0.5,0.75,0.9,1.0))
abline(h=c(0,.25,.5,.75,.9,1), lty=3)
abline(v = quantile(round(x),probs=c(0.25,0.5,0.75,0.9)), col = "blue", lwd = 1, lty=2)
points(quantile(round(x),probs=c(0,.25,.5,.75,.9,1)), c(0,.25,.5,.75,.9,1), lwd=2, col="blue")
boxplot(round(x), horizontal = TRUE, col = "pink", lwd=5)  
abline(v = quantile(round(x)), col = "blue", lwd = 3, lty=2)
par(new=T)
boxplot(round(x), horizontal = TRUE, col = "pink", lwd=2)  
par(op)
dev.off()

summary(round(x))
round(x)->r.x
data.frame(Dauer_H=r.x)->ha
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk0")
write.dta(ha, "ha.dta")
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.00    8.00   10.00   10.37   13.00   21.00
boxplot(round(x))->b
# plot(1:10, 1:10, axes = FALSE)
# axis(side = 1, at = c(1,5,10))
# axis(side = 2, at = c(1,3,7,10))
# box()

pdf("taschengeld_theo.pdf")
N <- 100#2500
set.seed(1)
x <- rnorm(N,mean=12,sd=3)
#op <- par(mar=c(0,0,0,0), oma=c(0,0,0,0)+.1)
#layout(matrix(c(1,1,1,2), nc=1))
y <- ppoints( length(x) )
plot(round(sort(x)), y, type="s", lwd=3,
     xlab="Taschengeld von Grundschulkindern", ylab="Anteil der Kinder", main="",cex.axis=2.0,
     axes = FALSE,cex.lab=1.5)
lines(sort(x),pnorm(sort(x), mean=mean(x),sd=sd(x)),col="red",lwd=2)
axis(side = 1, at = seq(0,34,2))
axis(side = 2, at = c(0,0.25,0.5,0.75,0.9,1.0))
abline(h=c(0,.25,.5,.75,.9,1), lty=3)
abline(v = quantile(round(x),probs=c(0.25,0.5,0.75,0.9)), col = "blue", lwd = 1, lty=2)
points(quantile(round(x),probs=c(0,.25,.5,.75,.9,1)), c(0,.25,.5,.75,.9,1), lwd=2, col="blue")
#boxplot(round(x), horizontal = TRUE, col = "pink", lwd=5)  
#abline(v = quantile(round(x)), col = "blue", lwd = 3, lty=2)
#par(new=T)
#boxplot(round(x), horizontal = TRUE, col = "pink", lwd=2)  
#par(op)
dev.off()

#https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r

pdf("taschengeld_dens.pdf")
g <- x
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=15, prob=TRUE, 
     xlab="Taschengeld", ylim=c(0, 0.18), 
     main="Dichte und Histogramm", ylab="",cex.lab=2,cex.main=2)
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off()

# in zahlen
summary(x)
sd(x)

#8.84
#9.7

# 11.37
# 12.42

#12.42*91
