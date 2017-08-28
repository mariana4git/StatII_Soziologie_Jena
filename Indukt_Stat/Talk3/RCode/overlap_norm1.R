# http://rpsychologist.com/calculating-the-overlap-of-two-normal-distributions-using-monte-carlo-integration
# Wie hoch ist W'keit, dass X < Y ?
set.seed(456456)
n <- 100000
mu1 <- 250
sd1 <- 40
mu2 <- 250 # i.e. cohen's d = 0.8
sd2 <- 40

xs <- seq(min(mu1 - 3*sd1, mu2 - 3*sd2), max(mu1 + 3*sd1, mu2 + 3*sd2), length.out=n)
f1 <- dnorm(xs, mean=mu1, sd=sd1) # dist1
f2 <- dnorm(xs, mean=mu2, sd=sd2) # dist2

ps <- matrix(c(runif(n, min(xs), max(xs)), runif(n, min=0, max=max(f1,f2)) ), ncol=2) # sample x,y from uniform dist

z1<- ps[,2] <= dnorm(ps[,1], mu1, sd1) # dist1
z2<- ps[,2] <= dnorm(ps[,1], mu2, sd2) # dist 2
z12 <- z1 | z2 # both dists
z3 <- ps[,2] <= pmin(dnorm(ps[,1], mu1, sd1), dnorm(ps[,1], mu2, sd2)) # overlap

# plot
pdf("nullhyp.pdf")
plot(ps[!z12, 1], ps[!z12, 2],col="lightgray" , pch=20, ylim=c(0, max(f1,f2)), 
     xlim=range(xs), xlab="", ylab="",cex.lab=2, cex.axis=2)#col='#137072'
points(ps[z1,1], ps[z1,2], col="#FBFFC0")
points(ps[z2,1], ps[z2,2], col="#56B292")
points(ps[z3, 1], ps[z3,2], col="#BF223D")
lines(xs, f1, lwd=2)
lines(xs, f2, lwd=2)
abline(v=mu1)
abline(v=mu2)
dev.off()


#setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk3")

# Rüger S. 44: Die hellgelbe + grüne Fäche entspricht der Totalvariation, Diese geteilt durch zwei ist die 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2774909/
# https://mathoverflow.net/questions/156946/are-there-known-expressions-for-total-variation-distance-between-n0-sigma-12

# overlapping histogram
# https://www.r-bloggers.com/overlapping-histogram-in-r/