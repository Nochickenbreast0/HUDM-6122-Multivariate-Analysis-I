#Three-dimensional plots
#Advantages:
#(1)Depth Perception: 3D plots allow viewers to see patterns, trends, and relationships 
#in data that might not be apparent in two-dimensional (2D) plots. 
#By adding the third dimension, data visualization can convey the depth of data points in space.

#(2)Complex Relationships: They can be useful for visualizing 
#complex relationships between three variables.

#(3)Data Exploration: They allow for more interactive data exploration. 
#Rotating the plot in three dimensions can provide new insights 
#and help to spot patterns, outliers, or clusters that are not detectable in a static 2D plot.

library(MVA)
measure <-
  structure(list(V1 = 1:20, 
                 V2 = c(34L, 37L, 38L, 36L, 38L, 43L,
                        40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L, 35L), 
                 V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L,
                        24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L), 
                 V4 = c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L,
                        38L, 37L, 38L, 37L, 40L, 35L)), .Names = c("V1", "V2", "V3", "V4"), 
            class = "data.frame", row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")
#Fig. 2.18. A three-dimensional scatterplot for the body measurements data with
#points corresponding to male and triangles to female measurements.
install.packages("scatterplot3d")
library("scatterplot3d")
par(mfrow=c(1,1))
with(measure, scatterplot3d(chest, waist, hips,
                            pch = (1:2)[gender], type = "h", angle = 75))
#pch = (1:2)[gender]: pch stands for plotting character. 
#This argument is indexing into the vector c(1, 2) with the gender variable.
#This would result in different plotting characters for different genders in the scatter plot.

#type = "h": This argument specifies the type of plot. 
#"h" indicates that vertical lines should be drawn from the z-axis up to the points.

#angle = 55: This sets the viewing angle of the 3D plot.


#Fig. 2.19. A three-dimensional scatterplot for the air pollution data.
data("USairpollution", package = "HSAUR2")
with(USairpollution,
     scatterplot3d(temp, wind, SO2, type = "h",
                   angle = 55))
#Two observations with high SO2 levels stand out

#If the dimensionality of the data could be reduced in some way 
#with little loss of information, three-dimensional plots might become more useful.

#Trellis graphics
#Trellis graphics is an approach to examining high-dimensional structure 
#in data by means of one-, two-, and three dimensional graphs.

#The essential feature of this approach is the multiple conditioning that
#allows some type of plot to be displayed for different values of a given variable
#(or variables). The aim is to help in understanding both the structure of the
#data and how well proposed models describe the structure.

#Fig. 2.20. Scatterplot of SO2 and temp for light and high winds.
library(lattice)
plot(xyplot(SO2 ~ temp| cut(wind, 2), data = USairpollution))
#It will produce a Trellis plot of SO2 against temp with panels separated 
#by the levels of wind that are divided into two groups (using cut(wind, 2)). 
#Each panel will represent one of the intervals of wind speed.

#It produces scatterplots of SO2 and temp conditioned on values of 
#wind divided into two equal parts that we shall creatively label “Light” and “High”.

#Conclusion: The plot suggests that in cities with light winds, air pollution decreases with
#increasing temperature, but in cities with high winds, air pollution does not
#appear to be strongly related to temperature.


#Fig. 2.21. Three-dimensional plots of temp, wind, and precip conditioned on levels of SO2.
#three-dimensional plots of temp, wind, and precip are shown for four levels of SO2
pollution <- with(USairpollution, equal.count(SO2,4))
plot(cloud(precip ~ temp * wind | pollution, panel.aspect = 0.9,
           data = USairpollution))
#equal.count function is used to divide the SO2 variable into 
#4 groups with approximately the same number of observations in each.

#precip ~ temp * wind | pollution specifies the formula for the plot. 
#precip is plotted against temp and wind, and the plot is conditioned on the 
#pollution factor, meaning separate panels will be created for each level of pollution.

#Conlusion: as there are few points in each of the three, threedimensional displays. 
#This is often a problem with multipanel plots when the sample size is not large.


#Fig. 2.22. Scatterplots of latitude and longitude conditioned on three ranges of depth.
plot(xyplot(lat ~ long| cut(depth, 3), data = quakes,
            layout = c(3, 1), xlab = "Longitude",
            ylab = "Latitude"))
#conditioned on the depth of the earthquakes. 
#The depth is cut into 3 intervals using the cut function.

#The distribution of locations in the latitude-longitude space
#is seen to be different in the three panels, particularly for very deep quakes.


#Fig. 2.23. Scatterplots of latitude and longitude conditioned on magnitude, with
#depth coded by shading.
install.packages("png")
library(png)
Fig.2.23 <- readPNG('E:/HUDM 6122/23.png')

plot(1, type = 'n', xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)
# Create an empty plot with no axes, labels, or ticks


rasterImage(Fig.2.23, 0, 0, 1, 1)
# Display the image within the plot

#Fig. 2.24. Scatterplots of latitude and longitude conditioned on magnitude.
data("quakes")
library(lattice)
Magnitude <- with(quakes, equal.count(mag,4))
plot(cloud(depth ~ lat * long | Magnitude, data = quakes,
           zlim = rev(range(quakes$depth)),
           screen = list(z = 105, x = -70), panel.aspect = 0.9,
           xlab = "Longitude", ylab = "Latitude", zlab = "Depth"))




#Stalactite plots
#It is designed for the detection and identification of multivariate outliers.
#he stalactite plot is based on the generalised distances of observations 
#from the multivariate mean of the data.

#the aim is to reduce the masking effects that can arise due to the influence 
#of outliers on the estimates of means and covariances obtained from all the data

#Fig. 2.25. Stalactite plot of US cities air pollution data.
stalac(USairpollution)

#The lengths are likely compared to some expected value. 
#If a line is longer than the expected value, it suggests that 
#the observed frequency is higher than expected. Conversely, 
#if a line is shorter, the observed frequency is lower than expected.

#Conclusion: The effect of masking is also clear; when all 41 observations are used 
#to calculate the generalised distances, only observations
#Chicago, Phoenix, and Providence are indicated to be outliers.