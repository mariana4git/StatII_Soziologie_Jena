#http://freakonometrics.hypotheses.org/18117
u=seq(.1,.2,length=501)
v=dbeta(u,1+xbar,1+n-xbar)
plot(u,v,axes=FALSE,type="l")
I=u<qbeta(.025,1+xbar,1+n-xbar)
polygon(c(u[I],rev(u[I])),c(v[I],
                              + rep(0,sum(I))),col="red",density=30,border=NA)
I=u>qbeta(.975,1+xbar,1+n-xbar)
polygon(c(u[I],rev(u[I])),c(v[I],
                              + rep(0,sum(I))),col="red",density=30,border=NA)
axis(1)

# http://www.statmethods.net/advgraphs/probability.html
# Children's IQ scores are normally distributed with a
# mean of 100 and a standard deviation of 15. What
# proportion of children are expected to have an IQ between
# 80 and 120?

mean=100; sd=15
lb=80; ub=120

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="Normal Distribution", axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="green")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(40, 160, 20), pos=0) 