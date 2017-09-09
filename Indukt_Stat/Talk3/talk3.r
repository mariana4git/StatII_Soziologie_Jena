# Beginne mit S. 171, als man sollte eigentlich die Power miteinbeziehen,
# tut es aber nicht, von dort zum Konfidenzintervall in Anlehnung an Ludwig-Mayerhofer

# https://statistics.berkeley.edu/computing/r-t-tests
# http://www.instantr.com/2012/12/29/performing-a-one-sample-t-test-in-r/
# http://stats.seandolinar.com/one-sample-t-test-with-r-code/
# https://www.stata.com/manuals13/rttest.pdf
# http://www.r-tutor.com/elementary-statistics/type-2-errors/type-2-errors-two-tailed-test-population-mean-unknown-variance

setwd("C:/Users/zo95yup/Documents/Indukt_Stat/Exercise2")
read.csv(file="flaschen.csv", header=TRUE)->flaschen
library(foreign)
write.dta(flaschen,"flaschen.dta")
#C:\Users\zo95yup\Documents\Indukt_Stat\Exercise2