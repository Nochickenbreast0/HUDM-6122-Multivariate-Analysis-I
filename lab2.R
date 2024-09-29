#Scatterplot
#A scatter plot is a type of data visualization that uses dots to represent 
#the values obtained for two different variables - one plotted along the x-axis 
#and the other plotted along the y-axis. 
#Scatter plots are used to observe and show relationships between two numeric variables. 
#The pattern of the scatter plot indicates the relationship which is also known as 
#the correlation between the variables.
library("MVA")
data("USairpollution", package = "HSAUR2")

set.seed(280875)
#It's used for random number generating.
#By setting the seed, you can ensure that the sequence of 
#random numbers is the same each time you run the code



#Fig. 2.1 Scatterplot of manu and popul
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"
plot(popul ~ manu, data = USairpollution,
     xlab = mlab, ylab = plab)
#the part left ofthe tilde defines the variable to be associated with the ordinate, 
#the part right of the tilde is the variable that goes with the abscissa


#Fig. 2.2. Scatterplot of manu and popul that shows the 
#marginal distribution in each variable as a rug plot
plot(popul ~ manu, data = USairpollution,
     xlab = mlab, ylab = plab)
rug(USairpollution$manu, side = 1)
#A rug plot adds a small tick at each data point along the x-axis margin (bottom side of the plot). 
#The side argument specifies which side of the plot the rug should be drawn on. 
#side = 1 means the bottom of the plot.
rug(USairpollution$popul, side = 2)


#Fig. 2.3. Scatterplot of manu and popul that shows 
#the marginal distributions bymhistogram and boxplot.
# Close any open graphics devices
while(dev.cur() > 1) dev.off()

# Now set the margins
par(mar=c(2, 2, 2, 2))

plot.new()

layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),
       widths = c(2, 1), heights = c(1, 2), respect = TRUE)
#2: This is the first plot area and will be placed in the top-left position.
#0: This indicates an empty space in the top-right position.
#1: This is the second plot area and will be placed in the bottom-left position.
#3: This is the third plot area and will be placed in the bottom-right position.

#widths vector of c(1, 2) means that the second column will be
#twice as wide as the first column.

#The heights vector c(1, 2) means that the second row is 
#twice as tall as the first row.

#respect indicating whether layout() should respect the aspect ratios of the plots.
xlim <- with(USairpollution, range(manu)) * 1.1
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"
plot(popul ~ manu, data = USairpollution, cex.lab = 0.8,
     xlab = mlab, ylab = plab, type = "n", xlim = xlim)
with(USairpollution, text(manu, popul, cex = 0.6,
                          labels = abbreviate(row.names(USairpollution))))
#The with() function in R is a convenience function that simplifies 
#the syntax when you want to apply an expression to a dataset. 

#with(data, expression)

#Add text lables
with(USairpollution, hist(manu, main = "", xlim = xlim))
with(USairpollution, boxplot(popul))
#Chicago is an obvious outlier


#Fig. 2.4. Scatterplot of manu and popul showing 
#the bivariate boxplot of the data
#Bivariate Data: This is data on two variables where each value of one variable is paired with a value of the other variable. 
#For example, the heights and weights of a group of individuals are bivariate data.
par(mar=c(5, 4, 4, 2) + 0.1, mfrow=c(1,1))
# Set the margins: bottom, left, top, right

outcity <- match(lab <- c("Chicago", "Detroit",
                          "Cleveland", "Philadelphia"), rownames(USairpollution))
x <- USairpollution[, c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
text(x$manu[outcity], x$popul[outcity], labels = lab,
     cex = 0.7, pos = c(2, 2, 4, 2, 2))
#Figure 2.4 clearly tells us that #Chicago, Philadelphia, Detroit, and 
#Cleveland should be regarded as outliers
#but not Houston, because it is on the “fence” rather than outside the “fence”.


#Correlation
with(USairpollution, cor(manu, popul))

outcity <- match(c("Chicago", "Detroit",
                   "Cleveland", "Philadelphia"),
                    rownames(USairpollution))
with(USairpollution, cor(manu[-outcity], popul[-outcity]))
#The match() function identifies rows of the data frame USairpollution corresponding to 
#the cities of interest, and the subset starting with a minus sign
#removes these units before the correlation is computed.

#Exclude the rows corresponding to the indices in outcity. 
#The negative indexing -outcity means "all rows except these indices."


#Fig. 2.5. Scatterplot of manu against popul showing the convex hull of the data
#Removal of the points lying on the convex hull can eliminate isolated
#outliers without disturbing the general shape of the bivariate distribution.

(hull <- with(USairpollution, chull(manu, popul)))
#The chull() function is being applied to two columns of the data frame: manu and popul. 
#This indicates that these columns contain x and y coordinates of points, respectively
#indices of the points that form the convex hull

with(USairpollution,
     plot(manu, popul, pch = 1, xlab = mlab, ylab = plab))
#pch: the type of symbol
with(USairpollution,
     polygon(manu[hull], popul[hull], density = 15, angle = 30))

with(USairpollution, cor(manu[-hull],popul[-hull]))


#Fig. 2.6. Chi-plot for manu and popul showing a clear deviation from independence
#Chi- plot can be used to detect deviations from independence
par(mar=c(5, 4, 4, 2) + 0.1, mfrow=c(1,2))
with(USairpollution, plot(manu, popul,
                          xlab = mlab, ylab = plab,
                          cex.lab = 0.6))
with(USairpollution, chiplot(manu, popul))
#Departure from independence is indicated in the latter 
#by a lack of points in the horizontal band indicated on the plot. 
#Here there is a very clear departure since there are very few of the observations 
#in this region.


#Fig. 2.7. Bubble plot of temp, wind, and SO2.
#For extra variables e.g. more than 2 vars

#the values of the third variable are represented by circles with radii proportional
#to these values and centred on the appropriate point in the scatterplot
par(mar=c(5, 4, 4, 2) + 0.1, mfrow=c(1,1))
ylim <- with(USairpollution, range(wind)) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution,
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution, symbols(temp, wind, circles = SO2,
                             inches = 0.5, add = TRUE))
#The plot seems to suggest that cities with moderate annual
#temperatures and moderate annual wind speeds tend to suffer the greatest air pollution
#but this is unlikely to be the whole story because none of the other
#variables in the data set are used in constructing Figure 2.7


#Fig. 2.8. Scatterplot of temp and wind showing
#five-sided stars representing the other variables

#five-sided “stars”, with the lengths of each side representing each
#of the remaining five variables.
plot(wind ~ temp, data = USairpollution,
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution,
     stars(USairpollution[,-c(2,5)], locations = cbind(temp, wind),
           labels = NULL, add = TRUE, cex = 0.5))
#This specifies the x and y coordinates where the stars will be drawn on the existing plot. 
#Here, temp and wind are used as the coordinates.

#add = TRUE: This tells R to add the stars to the existing plot 
#rather than creating a new one.

#Fig. 2.9. Star plot of the air pollution data.
par(cex=0.7, cex.axis=0.7)
stars(USairpollution, cex = 0.55)
#Each star represents all the variables for a single observation, 
#with the length of each ray from the center proportional to the value of the variable

#for New Orleans, Miami, Jacksonville, and Atlanta, have
#similar shapes, with their higher average annual temperature being distinctive,
#but telling a story about the data with this display is difficult.


#Fig. 2.10. Scatterplot matrix of the air pollution data
#A scatterplot matrix is nothing more than a square, 
#symmetric grid of bivariate scatterplots.

#it enables a row and a column to be visually scanned to
#see one variable against all others, with the scales for the one variable lined up
#along the horizontal or the vertical.
pairs(USairpollution, pch = ".", cex = 1.5)
round(cor(USairpollution), 4)

#Fig. 2.11. Scatterplot matrix of the air pollution data 
#showing the linear fit of each pair of variables.
pairs(USairpollution,
      panel = function (x, y, ...) {
      points(x, y, ...)
      abline(lm(y ~ x), col = "grey")
      }, pch = ".", cex = 1.5)
