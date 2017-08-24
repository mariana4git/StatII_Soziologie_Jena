graph.theo <- function(n=10,p=0.5){
  x <- dbinom(0:n,size=n,prob=p)
  return(barplot(x,names.arg=0:n,col="darkgreen",sub=paste("Bin(",n,",",p,")" ,"theo."),
                 cex.lab=2.0,cex.axis=2.0,cex.sub=2.0,cex.names=2.0,ylim=c(0,max(x)+0.1)))
}

graph.emp <- function(n=10,p=0.5,anz){
  x2 <- rbinom(n=anz,size=n,prob=p)
  z2<-rep(0,n+1)
  for(i in 0:n){
    sum(x2==i)->z2[i+1]
  }
  return(barplot(z/anz,names.arg=0:n,col="grey",sub=paste("Bin(",n,",",p,")" ,"emp."),
                 cex.lab=2.0,cex.axis=2.0,cex.sub=2.0,cex.names=2.0,ylim=c(0,max(z2/anz)+0.1)))
}#
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Uebung1")
set.seed(1)
pdf("muenzeEmp.pdf")
graph.emp(anz=200)
dev.off()
data.frame(x2)->my.frame
names(my.frame)
# https://www.beratung-statistik.de/statistik-beratung-infos/r-tutorial/
names(my.frame)<-c("AnzTreffer")
write.dta(my.frame,"muenze.dta")
#https://stackoverflow.com/questions/17759540/r-graph-of-binomial-distribution

#graph(20,0.1)
#graph(20,0.2)
#graph(20,0.3)
#graph(20,0.4)
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk1")
pdf("muenze.pdf")
graph(10,0.5)
dev.off()
#graph(20,0.6)
#graph(20,0.7)
#graph(20,0.8)
#graph(20,0.9)