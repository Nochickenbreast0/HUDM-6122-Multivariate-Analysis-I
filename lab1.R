#####


#Difference 
#Matrix: A matrix in R is a two-dimensional array that holds data of a single basic type 
#(numeric, logical, character, etc.). All elements in a matrix must be of the same type.
#DataFrame: A data frame is more flexible; it's a list of vectors of equal length. 
#Each column in a data frame can hold data of different types (e.g., numeric, character, factor).


class(2.36) ## or using "mode()"
typeof(2.36)
help("class")
help("typeof")

class(2.3636)
typeof(2.3636)

class(5)
typeof(5)
typeof(as.integer(5))

class("HUDM6122")
typeof("HUDM6122")

typeof("2==1")
typeof(2==1)
class(2==1)
#####

c(1,2,3,4,5,6)## A vector
1:6
c(1, "a", 3)
#This creates a vector with mixed types, but in R, 
#a vector can only hold elements of the same type, 
#so this will result in a character vector with "1", "a", "3".

matrix(data = 1:6, nrow = 2, byrow = T)## A matrix
matrix(data = 1:6, nrow = 3, byrow = F)


as.data.frame(matrix(data = 1:6, nrow = 2, byrow = T))## from matrix to dataframe

array(0.0, 3)  # [0.0 0.0 0.0]

array(0.0, c(2,3))  # 2x3 matrix

array(0.0, c(2,2,2)) # 2x2x2 n-array 
#R creates a three-dimensional array with dimensions 2x2x2

list("a",1,2==1)
#A character string "a".
#A numeric value 1.
#A logical value FALSE

list(name=c("Smith","Bob"), age=c(22,23))
#The first element is a vector named "name" containing two character strings, "Smith" and "Bob". 
#The second element is a numeric vector named "age" containing two numbers, 22 and 23. 


#The commands cbind and rbind in R are used to combine vectors, matrices, or data frames. 
#cbind stands for "column-bind," and it combines the elements into a matrix by binding them as columns. 
#rbind stands for "row-bind," and it combines elements into a matrix by binding them as rows.
cbind(1:3,4:6)
rbind(1:3,4:6)

#####
#ls() function lists all objects in the environment, 
#so after the variables are created, calling ls() again would include a and b in the output. 
ls()
a = 1
b = 2
c = 3
ls()

## Exchange the values of two variables
#The rm(c) function call is used to clean up the workspace by removing the temporary variable c
c = a; a = b; b = c; 
rm(c)

## attach and detach
dat = read.csv("Auto.csv")
head(dat)

attach(dat)
search() 
#the search() function is likely called to display the search path 
#and confirm that dat has been attached and is on the search list
mean(mpg)
detach(dat)
#When using attach(), it's important to remember to use detach(dat) when done 
#to avoid conflicts with other objects and functions in the global environment

#alternatively
search()
mean(dat$mpg)


#####
m1 = matrix(1:6, nrow = 2)
m2 = matrix(1:6, nrow = 3)
m1 %*% m2
#####






#####
my.mean <- function(vec){
  # my.mean computes and returns the mean of vec
  # Inputs:
  #  vec	:	vector of numerics
  # Outputs:
  #  mu	: 	mean of vec
  mu <- 0
  for (i in 1:length(vec)){
    mu <- mu + vec[i]/length(vec)
  }
  return(mu)
}


my.mean(1:6)
mean(1:6)

## Consider the case we replace values of outliers with zero 
modify.mean <- function(vec, thr){
  mu <- 0
  for (i in 1:length(vec)){
    if (i<=thr){
      mu <- mu + vec[i]/length(vec)
    }
  }
  return(mu)
}

modify.mean(1:6,6)
modify.mean(1:6,3)

#####
Auto <- read.csv("Auto.csv",header=T,na.strings="?")
#The header=T argument indicates that the first line of the file contains the column names. 
#The na.strings="?" argument specifies that any "?" character in the file 
#should be treated as an NA value, which represents missing data in R
Auto <- na.omit(Auto)
attach(Auto)
names(Auto)
class(mpg)
class(cylinders)
str(Auto)


cylinders <- as.factor(cylinders)
class(cylinders)
levels(cylinders)


class(Auto)
Auto.mat <- as.matrix(Auto) 
#Auto.mat <- as.matrix(Auto) converts the object Auto to a matrix 
#and assigns it to a new variable Auto.mat
class(Auto.mat)
Auto.mat[,2]
class(Auto.mat[,2])
Auto[1:2,2]

Auto.mat[1,2]+Auto.mat[2,2]

Auto[1,2]+Auto[2,2]
#The error "non-numeric argument to binary operator" suggests that 
#when you attempted to add the elements from the Auto.mat matrix, 
#one or both of the elements were not numeric. 
#This is why the addition failed for Auto.mat[1, 2] + Auto.mat[2, 2]. 
#when you tried Auto[1, 2] + Auto[2, 2], it returned 16, 
#which implies that in the original Auto object (likely a data frame), 
#both of these elements were numeric and the addition could be successfully performed

detach(Auto) # For later


rm(list=ls()) # Clears all objects
# Load auto.csv, turn ?’s into NAs, read top row as names
Auto <- read.csv("Auto.csv",header=T,na.strings="?")
Auto <- na.omit(Auto) # Remove NAs
attach(Auto) # Attach dataset



#####
p1 <- as.matrix(read.csv("example1.csv",header=T))
colMeans(p1)
rowMeans(p1)

p1_centered <- p1 - matrix(rep(colMeans(p1),nrow(p1)),
                           ncol=ncol(p1),byrow=T)
# Note that cov(p1), cov(p1_centered), and
# t(p1_centered) \%*\% (p1_centered) / (nrow(p1_centered)-1)# gives you the same matrix.
cov(p1)
cov_p1 <- cov(p1)
eigen_cov_p1 <- eigen(cov_p1)
print(eigen_cov_p1)


eigenval_cov <- eigen_cov_p1$value
eigenvec_cov <- eigen_cov_p1$vector


#Hypo data
hypo <-
  structure(list(individual = 1:10, sex = structure(c(2L, 2L, 2L,
                2L, 2L, 1L, 1L, 1L, 1L, 1L), .Label = c("Female", "Male"), class = "factor"),
                 age = c(21L, 43L, 22L, 86L, 60L, 16L, NA, 43L, 22L, 80L),
                 IQ = c(120L, NA, 135L, 150L, 92L, 130L, 150L, NA, 84L, 70L
                 ), depression = structure(c(2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
                1L, 1L), .Label = c("No", "Yes"), class = "factor"), health = structure(c(3L,3L, 1L, 4L, 2L, 2L, 3L, 1L, 1L, 2L), .Label = c("Average",
                "Good", "Very good", "Very poor"), class = "factor"), weight = c(150L,
                160L, 135L, 140L, 110L, 110L, 120L, 120L, 105L, 100L)), .Names = c("individual",
                "sex", "age", "IQ", "depression", "health", "weight"), class = "data.frame", row.names = c(NA, -10L))

hypo[1:2, c("health", "weight")]
#first 2 rows of column "health" and column "weight"


#measure data
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

#pottery data
data("pottery", package = "HSAUR2")

#exam data
exam <-
  structure(list(subject = 1:5, math = c(60L, 80L, 53L, 85L, 45L), 
                 english = c(70L, 65L, 60L, 79L, 80L), 
                 history = c(75L, 66L, 50L, 71L, 80L), 
                 geography = c(58L, 75L, 48L, 77L, 84L), 
                 chemistry = c(53L, 70L, 45L, 68L, 44L), 
                 physics = c(42L, 76L, 43L, 79L, 46L)),
                 .Names = c("subject", "maths", "english", "history", "geography", "chemistry", "physics"), 
                 class = "data.frame", row.names = c(NA, -5L))

#USairpollution data
data("USairpollution", package = "HSAUR2")

#SO2: SO2 content of air in micrograms per cubic metre;
#temp: average annual temperature in degrees Fahrenheit;
#manu: number of manufacturing enterprises employing 20 or more workers;
#popul: population size (1970 census) in thousands;
#wind: average annual wind speed in miles per hour;
#precip: average annual precipitation in inches;
#predays: average number of days with precipitation per year.

#Covariance
cov(measure[, c("chest", "waist", "hips")])

#If we require the separate covariance matrices of men and women
cov(subset(measure, gender == "female")[, c("chest", "waist", "hips")])
cov(subset(measure, gender == "male")[, c("chest", "waist", "hips")])
#the subset() returns all observations corresponding to 
#females (first statement) or males (second statement)

#Correlations
cor(measure[, c("chest", "waist", "hips")])

#Distance
x <- dist(scale(measure[, c("chest", "waist", "hips")], center = FALSE))
dist(scale(measure[, c("chest", "waist", "hips")], center = FALSE))
#divide each variable by its standard deviation using the function scale()
as.dist(round(as.matrix(x), 2)[1:12, 1:12])

#Figure 1.1 Bivariate normal density function with correlation ρ = 0.5
#An example of a bivariate normal density function with 
#both means equal to zero, both variances equal to one, 
#and correlation equal to 0.5.
library("MVA")
install.packages("mvtnorm")
library("mvtnorm")
x <- y <- seq(from = -3, to = 3, length = 50)
dat <- as.matrix(expand.grid(x, y))
d <- dmvnorm(dat, mean = c(0, 0), 
     sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
d <- matrix(d, ncol = length(x))
persp(x = x, y = y, z = d, xlab = "x1", ylab = "x2", zlab = "f(x)")
help("persp")

#Figure 1.2 Cumulative distribution functions and quantiles
#compares two cumulative distribution functions (CDFs) 
#from different normal distributions
x <- seq(from = -3, to = 3, length = 1000)
Fx <- pnorm(x)
Fy <- pnorm(x, mean = -1)
plot(x, Fx, type = "l", axes = FALSE, xlab = "",
     ylab = "Cumulative distribution function") 
lines(x, Fy, type = "l")

x0 <- which.min((x - 1.2)^2)

x05 <- which.min((x + 0.5)^2)

x08 <- which.min((x + 0.9)^2)
#x0 is the x-value where the CDF is closest to 0.2 (Fx), 
#x05 is closest to 0.5, and x08 is closest to 0.9.

xx <- which.min(abs(Fy - Fx[x0]))

arrows(x0 = c(min(x), x[x0], x[xx], x[x08], x[x08], x[x08]),
       y0 = c(Fx[x0], Fx[x0], Fy[xx], 0, Fx[x08], Fy[x08]), 
       x1 = c(x[x0], x[x0], x[xx], x[x08], -3, -3), 
       y1 = c(Fx[x0], 0, 0, Fy[x08], Fx[x08], Fy[x08]))
mtext(at = c(x[x08], x[xx], x[x0]), side = 1, line = 1, text =
      c(expression(q), expression(q[2](p)), expression(q[1](p))))

mtext(at = c(0, Fx[x08], Fy[x08], Fx[x0], 1), line = 1, side = 2, text =
       c(0, expression(p[1](q)), expression(p[2](q)), expression(p), 1)) 

box()

 
#Figure 1.3 Normal probability plots of chest, waist, and hip measurements
x <- measure[, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, MARGIN = 1, function(x) 
  t(x - cm) %*% solve(S) %*% (x - cm))
#The differences di have to be computed for all units in our data 
#" MARGIN = 1" indicates that the function should be applied over rows
par(mfrow=c(1,3))
qqnorm(measure[,"chest"], main = "chest"); qqline(measure[,"chest"])

qqnorm(measure[,"waist"], main = "waist"); qqline(measure[,"waist"])

qqnorm(measure[,"hips"], main = "hips"); qqline(measure[,"hips"])

#If the points closely follow the straight line (which represents the theoretical distribution), 
#this suggests that the sample data also follow that distribution.

#Figure 1.4 Chi-square plot of generalised distances for body measurements data
par(mfrow=c(1,1))
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d),
     xlab = expression(paste(chi[3]^2, " Quantile")), 
     ylab = "Ordered distances")
abline(a = 0, b = 1)

#Figure 1.5 Normal probability plots for USairpollution data
par(mar=c(2,2,2,2))
layout(matrix(1:8, nc = 2))
sapply(colnames(USairpollution), function(x) {
  qqnorm(USairpollution[[x]], main = x)
  qqline(USairpollution[[x]])
})


#Figure 1.6 χ2 plot of generalised distances for USairpollution data
par(mfrow=c(1,1))
x <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 6),
        sd <- sort(d),
        xlab = expression(paste(chi[6]^2, " Quantile")),
        ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
#computes the ranks of the absolute differences between the theoretical quantiles 
#and the sample quantiles, with ties broken randomly
#find outliers
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)


