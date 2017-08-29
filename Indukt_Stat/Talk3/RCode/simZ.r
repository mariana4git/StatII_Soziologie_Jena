set.seed(789)
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk3")
x<-rnorm(n=100,mean=250,sd=4)# Mittelwert der x-Gruppe wird simuliert für eine Standardabweichung von 40.
y<-rnorm(n=100,mean=250,sd=4)# Mittelwert der y-Gruppe wird simuliert für eine Standardabweichung von 40.
z<-x-y
#z<-rnorm(n=100,mean=0,sd=sqrt(32))
qnorm(p=0.05,mean=0,sd=sqrt(32))
g<-z
# https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
m<-mean(g)
std<-sqrt(var(g))
# hist(g, density=20, breaks=20, prob=TRUE, 
#      xlab="x-variable", ylim=c(0, 2), 
#      main="normal curve over histogram")
# curve(dnorm(x, mean=m, sd=std), 
#       col="darkblue", lwd=2, add=TRUE, yaxt="n")
pdf("distZ.pdf")
h <- hist(g, breaks = 10, density = 10,
          col = "lightgray", xlab = "Teststatistik Z", 
          main = "Histogramm und theo. Dichte von Z",ylim=c(0,0.09),xlim=c(-20,20),
          ylab="rel. Häufigkeit",prob=TRUE,cex.lab=1.5, cex.axis=2.0, cex.main=2.0, cex.sub=2.0) 
xfit <- seq(-20, 20, length = 40) #seq(min(g)-5, max(g)+5, length = 40) 
yfit <- dnorm(xfit, mean = 0, sd = sqrt(32)) #dnorm(xfit, mean = mean(g), sd = sd(g)) 
#yfit <- yfit * diff(h$mids[1:2]) #* length(g) 

lines(xfit, yfit, col = "red", lwd = 2)
#abline(v=qnorm(p=0.05,mean=0,sd=sqrt(32)),col="blue", lwd = 2)
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