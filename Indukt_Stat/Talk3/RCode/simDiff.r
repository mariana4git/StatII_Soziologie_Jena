x<-rnorm(n=1050,mean=250,sd=40)
y<-rnorm(n=1050,mean=250,sd=40)
# z:= mean(x)-mean(y)~ N (0,sqrt(32))
# https://mattmazur.com/2014/10/

experiments = 1000
visitors = 100
#conversion_rate = 0.3

expected_conversions = 0 #visitors * conversion_rate
expected_sd = sqrt(32)#sqrt( visitors * conversion_rate * ( 1 - conversion_rate ) )
sd_for_axis_range = 50#4.5
axis_divisions = 5#5

results = vector()
for ( experiment in 1:experiments ) {
  conversions = 0
  for ( visitor in 1:visitors ) {
    if ( runif( 1 ) <= conversion_rate ) {
      conversions = conversions + 1
    }
  }
  results = c( results, conversions )
}

par( oma = c( 0, 2, 0, 0 ) )

axis_min = floor( ( expected_conversions - sd_for_axis_range * expected_sd ) / axis_divisions ) * axis_divisions
axis_max = ceiling( ( expected_conversions + sd_for_axis_range * expected_sd ) / axis_divisions ) * axis_divisions
hist( results, axes = FALSE, breaks = seq( axis_min, axis_max, by = 1 ), ylab = 'Probability', xlab = 'Conversions', freq = FALSE, col = '#4B85ED', main = 'Distribution of Results' )

axis( side = 1, at = seq( axis_min, axis_max, by = axis_divisions ), pos = 0, col = "#666666", col.axis = "#666666", lwd = 1, tck = -0.015 )
axis( side = 2, col = "#666666", col.axis = "#666666", lwd = 1, tck = -0.015 )

curve( dnorm(x, mean = conversion_rate * visitors, sd = expected_sd ), add = TRUE, col = "red", lwd = 4 )