require("ggplot2")
# http://rpsychologist.com/short-r-script-to-plot-effect-sizes-cohens-d-and-shade-overlapping-area
# Standardized Mean Difference (Cohen's d)
ES <- 0.8
# get mean2 depending on value of ES from d = (u1 - u2)/sd
mean1 <- ES*1 + 1
# create x sequence
x <- seq(1 - 3*1, mean1 + 3*1, .01)
# generate normal dist #1
y1 <- dnorm(x, 1, 1)
# put in data frame
df1 <- data.frame("x" = x, "y" = y1)
# generate normal dist #2
y2 <- dnorm(x, mean1, 1)
# put in data frame
df2 <- data.frame("x" = x, "y" = y2)
# get y values under overlap
y.poly <- pmin(y1,y2)
# put in data frame
poly <- data.frame("x" = x, "y" = y.poly)

# Cohen's U3, proportion of control > 50th perc. treatment
u3 <- 1 - pnorm(1, mean1,1)
u3 <- round(u3,3)

# plot with ggplot2
ggplot(df1, aes(x,y, color="treatment")) +
  # add line for treatment group
  geom_line(size=1) + 
  # add line for control group
  geom_line(data=df2, aes(color="control"),size=1) +
  # shade overlap
  geom_polygon(aes(color=NULL), data=poly, fill="red", alpha=I(4/10),
               show_guide=F) +
  # add vlines for group means
  geom_vline(xintercept = 1, linetype="dotted") + 
  geom_vline(xintercept = mean1, linetype="dotted") + 
  # add plot title
  opts(title=paste("Visualizing Effect Sizes 
                   (Cohen's d = ",ES,"; U3 = ",u3,")", sep="")) +
  # change colors and legend annotation
  scale_color_manual("Group", 
                     values= c("treatment" = "black","control" = "red")) +
  # remove axis labels
  ylab(NULL) + xlab(NULL)