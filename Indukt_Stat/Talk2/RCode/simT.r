set.seed(789)
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk2")
x<-rnorm(n=100,mean=250,sd=4)# Mittelwert der x-Gruppe wird simuliert für eine Standardabweichung von 40.
# und 10 Personen pro Gruppe
y<-rnorm(n=100,mean=250,sd=4)# Mittelwert der y-Gruppe wird simuliert für eine Standardabweichung von 40.
t<-(x-y)/sqrt(var(c(x,y)))
#z<-rnorm(n=100,mean=0,sd=sqrt(32))
qt(p=0.05,df=18)
g<-t
# https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
m<-mean(g)
std<-sqrt(var(g))

pdf("t_dist2.pdf")
h <- hist(g, breaks = 10, density = 10, col = "lightgray",
          xlab = "Teststatistik T",main = "Histogramm und theo. Dichte von T",prob=T,  
          ylim=c(0,0.4),xlim=c(-4,4),ylab="rel. Häufigkeit",cex.lab=1.5, cex.axis=2.0,
          cex.main=2.0, cex.sub=2.0)
xfit <- seq(-4, 4, length = 40)  
yfit <- dt(xfit, df=18, ncp=0)  

lines(xfit, yfit, col = "red", lwd = 2)
abline(v=qt(p=0.05,df=18),col="blue", lwd = 2)
dev.off()
yfit

#https://stackoverflow.com/questions/16504452/color-a-portion-of-the-normal-distribution

sigma <- 1
mu    <- 0

lower.x <- -0.0
upper.x <-  2.1

x  <- seq(-4, 4, by = 0.1)
y  <- ( 1/(sigma * sqrt(2*pi)) ) * ( exp(1)^( (-1 * ((x - mu)^2)) / (2*(sigma^2)) ) )
plot(x,y,type="l", lwd=2, col="blue")

x=seq(lower.x, upper.x, by = 0.1)
y  <- ( 1/(sigma * sqrt(2*pi)) ) * ( exp(1)^( (-1 * ((x - mu)^2)) / (2*(sigma^2)) ) )
polygon(c(lower.x,x,upper.x), c(0,y,0), col="gray")
lines(x, y, col="blue", lwd=2)