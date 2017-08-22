graph <- function(n,p){
  x <- dbinom(0:n,size=n,prob=p)
  return(barplot(x,names.arg=0:n,col="darkgreen",sub="Bin(10,0.5)",
                 cex.lab=2.0,cex.axis=2.0,cex.sub=2.0,cex.names=2.0,ylim=c(0,0.25)))
}

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