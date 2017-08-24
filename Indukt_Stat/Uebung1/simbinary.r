# https://stackoverflow.com/questions/10535235/generate-correlated-random-numbers-from-binomial-distributions-in-r
# https://stackoverflow.com/questions/16089178/how-to-simulate-correlated-binary-data-with-r
set.seed(1)
rnorm(100,mean=12,sd=3)->x

#library(bindata)
# https://www.beratung-statistik.de/statistik-beratung-infos/r-tutorial/
## Construct a binary correlation matrix
rho <- 0.3
m <- matrix(c(1,rho,rho,1), ncol=2)   
set.seed(1)
## Simulate 10000 x-y pairs, and check that they have the specified
## correlation structure
z <- rmvbin(100, margprob = c(0.5, 0.7), bincorr = m) 
#cor(z)
#           [,1]      [,2]
# [1,] 1.0000000 0.7889613
# [2,] 0.7889613 1.0000000
id<-1:100
as.data.frame(cbind(id,round(x,0),z))->data_set
names(data_set)<-c("id","TG","G","WSp")
#setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Uebung1")
library(foreign)
write.dta(data_set,"taschengeld.dta")




# http://www.statmethods.net/stats/frequencies.html
# 2-Way Frequency Table
A<-z[,2]
B<-z[,1]
#attach(mydata)
mytable <- table(data.frame(A,B)) # A will be rows, B will be columns
mytable # print table

margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
round(prop.table(mytable, 1),2) # row percentages
round(prop.table(mytable, 2),2) # column percentages 



size <- 200
p1 <- 0.5
p2 <- 0.3
rho<- 0.2

# Create one pair of correlated binomial values
trials <- rmvbin(size, c(p1,p2), bincorr=(1-rho)*diag(2)+rho)
table(data.frame(trials))->tab
margin.table(tab,1)
margin.table(tab,2)