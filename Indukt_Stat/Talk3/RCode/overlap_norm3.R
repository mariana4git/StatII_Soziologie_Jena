# https://mattmazur.com/2014/10/
# 
# setClass(
#   Class = "Distribution",
#   representation = representation(
#     name = "character",
#     participants = "numeric",
#     conversions = "numeric",
#     sample_proportion = "numeric",
#     se = "numeric",
#     color = "character",
#     x = "vector",
#     y = "vector"
#   )
# )
# 
# # We rewrite the initialize method for Distribution objects so that we can
# # set the x and y values which are used throughout the plotting process
# setMethod(
#   f = "initialize",
#   signature = "Distribution",
#   definition = function( .Object, name, participants, conversions, color ) {
#     .Object@name = name
#     .Object@sample_proportion = conversions / participants
#     .Object@se = sqrt( ( .Object@sample_proportion * ( 1 - .Object@sample_proportion ) ) / participants )
#     .Object@color = color
#     .Object@x = seq( -4, 4, length = 100 ) * .Object@se + .Object@sample_proportion
#     .Object@y = dnorm( .Object@x, .Object@sample_proportion, .Object@se )
#     
#     return ( .Object )
#   }
# )
setwd("C:/Users/zo95yup/Documents/GitHub/StatII_Soziologie_Jena/Indukt_Stat/Talk3")
setClass(
  Class = "Distribution",
  representation = representation(
    name = "character",
    mean = "numeric",
    sd = "numeric",
    color = "character",
    x = "vector",
    y = "vector"
  )
)

# We rewrite the initialize method for Distribution objects so that we can
# set the x and y values which are used throughout the plotting process
setMethod(
  f = "initialize",
  signature = "Distribution",
  definition = function( .Object, name, mean, sd, color ) {
    .Object@name = name
    .Object@mean = mean
    .Object@sd = sd
    .Object@color = color
    .Object@x = seq( -4, 4, length = 1000 ) * sd + mean
    .Object@y = dnorm( .Object@x, mean, sd )
    
    return ( .Object )
  }
)

# Given a list of distributions, this returns a list of the x and y axis range
get_axis_ranges = function( distributions ) {
  x_all = vector()
  y_all = vector()
  
  for ( distribution in distributions ) {
    x_all = c( x_all, distribution@x )
    y_all = c( y_all, distribution@y )
  }
  
  xlim = c( min( x_all ), max( x_all ) )
  ylim = c( min( y_all ), max( y_all ) )
  
  # Note that by forming a list of the vectors, the vectors get converted to lists
  # which we then have to convert back to vectors in order to use them for plotting
  return ( list( xlim, ylim ) )
}

get_x_axis_values = function( x_range ) {
  by = 30
  
  min_x = floor( min( x_range ) / by ) * by
  max_x = ceiling( max( x_range ) / by ) * by
  
  return ( seq( min_x, max_x, by = by ) )
}

# Define the distributions that we want to plot
# distributions = list(
#   new( Class = "Distribution", name = "Klient(inn)en", participants = 250, conversions = 40, color = "#00cc00" ),
#   new( Class = "Distribution", name = "Miarbeiter(innen)", participants = 270, conversions = 60, color = "blue" )
# )
##Nur hier ändern
 distributions = list(
   new( Class = "Distribution", name = "Klient(inn)en", mean = 250, sd = 40, color = "#00cc00" ),
   new( Class = "Distribution", name = "Miarbeiter(innen)", mean = 250, sd = 40, color = "blue" )
 )

# Determine the range to use for each axis
axis_range = get_axis_ranges( distributions )
xlim = axis_range[[ 1 ]]
ylim = axis_range[[ 2 ]]
pdf("nullhyp.pdf")
# Create the plot
plot( NULL, NULL, type = "n", xlim = xlim, ylim = ylim, xlab = "Fleischmenge in g", ylab = "", 
      main = "", axes = FALSE, cex.lab=2.0 )

# Render each of the curves
line_width = 3
for ( distribution in distributions ) {
  polygon( distribution@x, distribution@y, col = adjustcolor( distribution@color, alpha.f = 0.3 ), border = NA )
  lines( distribution@x, distribution@y, col = adjustcolor( distribution@color, alpha.f = 1 ), lwd = line_width )
  
  # Draw a line down the center of the curve
  ci_center_y = dnorm( distribution@mean, distribution@mean, distribution@sd )
  coords = xy.coords( c( distribution@mean, distribution@mean ), c( 0, ci_center_y ) )
  lines( coords, col = adjustcolor( distribution@color, alpha.f = 0.4 ), lwd = 1 )
}

# Render the x axis
axis( side = 1, at = round(get_x_axis_values( xlim )), pos = 0, col = "#777777", 
      col.axis = "#777777", lwd = line_width,cex.axis=2.0 )

# Finally, render a legend
legend_text = vector()
legend_colors = vector()
for ( distribution in distributions ) {
  legend_text = c( legend_text, distribution@name )
  legend_colors = c( legend_colors, distribution@color )
}
legend('topleft', legend_text, lty = 1, lwd = line_width, col = legend_colors, bty = 'n', cex=2.0 )
dev.off()