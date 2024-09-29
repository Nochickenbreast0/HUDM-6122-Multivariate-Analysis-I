
load("E:/HUDM 6122/Generic Conspiracist Beliefs Scale.RData")
dim(GCBS)

# basic descriptive statistics
library(psych)
describe(GCBS)

# printing and testing correlations
# psych package has a function which only displays
# the lower triangle of the correlation matrix 
lowerCor(GCBS, use = "pairwise.complete.obs")

# p-values created when calculating correlations
round(corr.test(GCBS, use = "pairwise.complete.obs")$p,5)
#The use = "pairwise.complete.obs" argument specifies that the function 
#should handle missing values by using pairwise complete observations. 
#This means that for each pair of variables, only observations without 
#missing values in either variable are used to calculate correlations. 
#This approach allows for the use of all available data without excluding 
#entire rows that have missing values in one or more variables.

#The p-values are very small: If the actual p-values are very small (less than 0.00001), 
#rounding to 5 decimal places would result in 0. This suggests that there is statistically 
#significant evidence of a correlation between the variable pairs, assuming the p-values 
#were indeed very small before rounding.



# confidence intervals created when calculating correlations
round(corr.test(GCBS, use = "pairwise.complete.obs")$ci,3)
#Confidence intervals provide a range of values within which the true correlation 
#coefficient is expected to lie with a certain level of confidence, commonly 95%.



# Perform a single-factor EFA
# measuring each item’s relationship to a single factor  
# Question: how well items relate to the latent factor?

#Determining how well items (observed variables) measure a latent variable (an unobserved construct) 
#in factor analysis involves several key indicators and statistical measures. 
#Factor Loadings
#Factor loadings represent the correlation between the items and the latent variable. 
#They indicate how strongly each item is associated with the factor. High absolute values 
#(usually above 0.4 or 0.5) suggest that the item is a good indicator of the latent variable. 
#Loadings can be observed in the factor loading matrix resulting from the analysis.

#Communalities
#Communalities indicate the proportion of each item's variance that is explained by the 
#factors extracted in the analysis. Higher communalities suggest that a greater portion 
#of an item's variance is accounted for by the latent variables, implying the item is well 
#represented by the underlying factor structure.


EFA.cor <- fa(GCBS)
#It suggests that exploratory factor analysis is performed on the GCBS dataset

# print the results
print(EFA.cor)
#Interpretation:
#Standardized Loadings (Pattern Matrix): The first part shows the factor loadings 
#for each variable (Q1, Q2, ..., Q15) on the extracted factor (labeled MR1). 
#Factor loadings are the correlations between the variables and the factor, 
#indicating how strongly each variable is associated with the factor. 
#A higher absolute value indicates a stronger association. For example, 
#Q4 with a loading of 0.77 has a strong association with the factor MR1.

#Communalities (h2): These values indicate the proportion of each variable's variance 
#that is explained by the factor(s). Higher communalities suggest that the factor explains 
#a large portion of the variable's variance.

#Uniqueness (u2): This is the proportion of variance that is unique to each variable and 
#not explained by the factor. For instance, Q10 has a uniqueness of 0.68, suggesting that 
#a significant portion of its variance is not explained by the factor.

#SS Loadings: The sum of squared loadings for the factor, indicating the total variance explained by the factor.

#Proportion Var: The proportion of total variance explained by the factor. MR1 explains 48% of the variance.

#Mean item complexity = 1: This suggests that, on average, the items (variables) 
#are primarily loading on one factor, which aligns with the extraction of a single factor.

#Root Mean Square of the Residuals (RMSR): The RMSR is a measure of the difference 
#between the observed correlations and the model-implied correlations. A value close 
#to 0, like 0.08 here, indicates a good fit.

#Tucker Lewis Index (TLI) and RMSEA: TLI is a comparative fit index with values 
#closer to 1 indicating a better fit. RMSEA assesses how well the model, with 
#unknown but optimally chosen parameter estimates, would fit the population's 
#covariance matrix. A value of RMSEA below 0.05 indicates a close fit, and 
#up to 0.08 an acceptable one. Here, RMSEA is within 0.142 to 0.149, 
#suggesting a less optimal fit.

#Measures of factor score adequacy are very high, suggesting that the 
#factor scores estimated from this model are reliable.




EFA.cov <- fa(GCBS,covar=TRUE)
# print the results
print(EFA.cov)

mle.varimax <- fa(GCBS,2,rotate="varimax",fm="mle")
print(mle.varimax)
#The standardized loadings (pattern matrix) show how each variable (Q1, Q2, ..., Q15) 
#relates to the two factors (ML1 and ML2). A higher absolute value indicates a stronger 
#relationship with the factor. Generally, loadings above 0.4 or 0.5 are considered significant. 
#For example:
#Q6 has a strong loading on ML1 (0.73), suggesting it has a strong relationship with the first factor.
#Q8 has a strong loading on ML2 (0.86), indicating a strong relationship with the second factor.

#The communalities (h2) show how much of each variable's variance is explained by the
#two factors combined. Higher values indicate that more of the variable's variance is 
#captured by the model. The uniqueness (u2) represents the variance that is unique to 
#each variable and not explained by the extracted factors.

#Root Mean Square of the Residuals (RMSR): A measure of the model's fit, with lower values (close to 0) indicating a better fit. Your result of 0.04 suggests a good fit.

#Tucker Lewis Index (TLI): A comparative fit index where values closer to 1 indicate a better fit. 
#A value of 0.898 is relatively high, suggesting a good model fit.

#Root Mean Square Error of Approximation (RMSEA): Measures the fit per degree of freedom 
#in the model; values below 0.05 indicate a good fit, and up to 0.08 an acceptable fit. 
#Value of 0.095 is slightly above the acceptable range, indicating a less optimal fit.

#SS Loadings: Sum of squared loadings for each factor, indicating the total variance 
#explained by each factor. ML1 explains more variance (5.50) than ML2 (2.97).
#Proportion and Cumulative Variance: ML1 explains 37% of the variance, and 
#ML2 explains an additional 20%, with a cumulative variance of 57% explained 
#by the model. This indicates that the two factors together capture a significant 
#portion of the variance in the dataset.



#Factor loadings represent the strength and directionality of the 
#relationship between each item and the underlying factor, and they can range from -1 to 1.
# Print the factor loadings
EFA.cor$loadings

# Print a diagram of the items' factor loadings
fa.diagram(EFA.cor)
#Factor (MR1): The plot centers around a single factor, MR1, indicating that the analysis 
#or this specific visualization focuses on how each variable (Q1 to Q15) relates to this one factor.

#Variables (Q1 to Q15): Each variable is represented as a point connected by a line to the 
#factor MR1. The length and direction of the line indicate the strength and nature of the 
#relationship between the variable and the factor.

#Loadings (Numerical Values): The numerical values next to each variable indicate the 
#factor loading for each variable on MR1. Factor loadings can range from -1 to 1, where:
#Values closer to 1 or -1 indicate a strong relationship between the variable and the factor. 
#A value close to 1 suggests that the variable strongly and positively correlates with the factor, 
#meaning as the factor increases, the variable tends to increase as well. A value close to -1 would 
#indicate a strong negative correlation, meaning as the factor increases, the variable tends to decrease.
#Values closer to 0 indicate a weak or no relationship between the variable and the factor.

#In the plot, all variables show positive loadings on MR1, indicating that they all positively 
#relate to this factor. The variables with higher loadings (e.g., Q12, Q4, Q6, Q14, etc., with 
#loadings of 0.7 to 0.8) are more strongly associated with the factor MR1. This suggests that 
#MR1 represents a common underlying dimension or construct that these variables are measuring.

#Variables with slightly lower loadings (e.g., Q8, Q3, Q15, Q10 with loadings of 0.6) are still 
#positively correlated with the factor but to a lesser extent compared to others.


fa.diagram(mle.varimax)



# factor scores indicate how much of the factor each person is thought to possess
# total scores for the first 20 respondents
# these values tell you how much of the construct they possess
tot.scores=apply(GCBS, FUN=sum, MARGIN=1)
tot.scores[1:20]

index=which(tot.scores==max(tot.scores))
tot.scores[index]

# first few lines of the response data and their sum scores
# Compare these to illustrate the relationship between responses and factor scores.

head(EFA.cor$scores)

# understand how the factor scores are distributed
summary(EFA.cor$scores)
#The factor scores for MR1 are standardized, with a mean of 0, which is typical 
#in factor analysis. The spread of the scores, from -1.854703 to 1.949580, indicates 
#the range of individual differences in the underlying construct represented by MR1.


# density plot of the estimated factor scores 
plot(density(EFA.cor$scores, na.rm = TRUE), main = "Factor Scores")
#The plot shows a roughly bell-shaped curve, which is indicative of a normal distribution. 
#This suggests that the factor scores are distributed in a way that most scores are around 
#the mean (which is standardized to 0), with fewer individuals having very high or very low scores.

#The peak of the distribution is near the center (around 0), consistent with the 
#mean factor score being 0, as expected in standardized scores from factor analysis.

#The x-axis represents the factor scores, and the y-axis represents the density of these scores at different points.

#The bandwidth (0.1822) provides information about the smoothing parameter used 
#to create the density plot. A smaller bandwidth results in a more detailed plot 
#that may reflect minor fluctuations in the data, while a larger bandwidth produces 
#a smoother plot. The chosen bandwidth here strikes a balance, offering a clear view
#of the distribution's shape without overemphasizing minor fluctuations.



############

# scores of 100 subjects on 8 tests.
load("E:/HUDM 6122/scores.RData")

head(scores)

round(cor(scores), 2)

factanal(x = scores, factors = 1)
#Origin and Dependency: fa() is from the psych package, focusing on psychometrics and 
#offering extensive options for EFA. factanal() is from the base R stats package, mainly 
#for CFA with a more straightforward approach.

#Flexibility and Output: fa() offers more extraction methods, rotation options, and detailed 
#outputs, making it more flexible for complex analyses. factanal() provides a simpler, 
#more streamlined output suitable for basic confirmatory analyses.

#Intended Use: fa() is designed with the flexibility needed for exploratory analysis in 
#psychology and related fields. factanal() is more suited for situations where the researcher 
#has specific hypotheses about the factor structure to be tested.



#Uniquenesses: Uniqueness values represent the proportion of variance in each observed variable 
#that is unique to it, not shared with other variables via the common factor. For example, test.1, 
#test.4, and several other tests have uniquenesses close to 1 (e.g., 1.000 for test.1), indicating 
#that almost all their variance is unique and not explained by the common factor.

#Lower uniquenesses, like 0.174 for test.3, suggest that a significant portion of the variance in 
#these variables is explained by the factor.

#Loadings: Factor loadings show the strength and direction of the relationship between each variable 
#and the extracted factor. Loadings for test.3 to test.8 are significant (ranging from 0.880 to 0.909), 
#indicating a strong relationship with the factor.

#The absence of loadings for test.1, test.2, and test.4 suggests that either they have very low loadings 
#not shown due to rounding or some formatting issue in the output.

#SS Loadings, Proportion Var: Sum of squared loadings (SS Loadings) is 3.216, indicating the total variance 
#explained by the factor.
#Proportion Var (0.402) indicates that the factor explains 40.2% of the total variance among the variables.

#Test of the hypothesis: The Chi-square statistic is 327.47 with 20 degrees of freedom, and the p-value is 
#exceedingly small (1.92e-57), suggesting that the model does not fit the data well. In other words, the 
#hypothesis that a single factor is sufficient to explain the correlations among these variables is strongly rejected.

factanal(x = scores, factors = 2)


cov.mat <- cov(scores)
round(cov.mat, 3)

factanal(covmat=cov.mat, factors = 2, n.obs = nrow(scores))

factors <- factanal(x = scores, factors = 2, scores = 'regression')
head(factors$scores)
#scores = 'regression': This option requests that factor scores be estimated for each observation using the 
#regression method. Factor scores are estimated values that represent an individual's position on each of the 
#latent factors identified by the analysis. The regression method for calculating factor scores involves using 
#a regression equation that predicts the factor scores based on the observed variables.



# orthogonal rotation
factors.varimax <- factanal(scores, factors = 2, rotation="varimax", scores="regression")
cor(factors.varimax$scores)

# oblique rotation
promax <- factanal(scores, factors = 2, rotation="promax", scores="regression")
cor(promax$scores)

scores.fa = fa(scores, # input data
              nfactors = 2, # number of factors
              rotate = "varimax", # rotation
              scores = "regression") # factor score estimation
scores.fa$loadings # factor loadings

scores.fa$uniquenesses
scores.fa$communality


rm(list=ls())
load("E:/HUDM 6122/survey.RData")
dim(survey)

data=survey[,-c(1,14)]

describe(data)
head(data)

cor(data)

library(corrplot)
corrplot(cor(data), method="number")

Y=survey[,14]

# Kaiser-Meyer-Olkin sampling adequacy as a measure of factorability (KMO > 0.6)
KMO(r = cor(data))

# Bartlett’s Test of Sphericity
cortest.bartlett(data)
#Bartlett's test checks the null hypothesis that the correlation matrix is an identity matrix, 
#meaning that all variables are uncorrelated, and therefore, factor analysis is inappropriate. 
#A significant result (p-value < 0.05) indicates that the variables are correlated to some extent, 
#justifying the use of factor analysis by suggesting that there are underlying factors that can 
#explain the correlations among variables.


det(cor(data))
 # it's positive
#This line calculates the determinant of the correlation matrix of your dataset. The determinant 
#of the correlation matrix gives an overall measure of how much the variables are correlated with 
#each other. If the determinant is close to 0, it suggests that the variables are highly intercorrelated, 
#and factor analysis may be suitable. On the other hand, a determinant close to 1 indicates that the 
#variables are not highly correlated, and factor analysis may not be as useful.


# Scree Plot to select the number of factors 

library(ggplot2)

fafitfree <- fa(data,nfactors = ncol(data), rotate = "none")
n.factors <- length(fafitfree$e.values)
scree.plot     <- data.frame(
  Factor.n =  as.factor(1:n.factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree.plot, aes(x = Factor.n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("eigenvalues") +
  labs( title = "Scree Plot", 
        subtitle = "(obtained from the correlation matrix)")

fa.survey <- fa(r=data,nfactors = 4, 
 # covar = FALSE, SMC = TRUE,
 fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
 max.iter=100, # (50 is the default, but we have changed it to 100
 rotate="varimax") # none rotation
print(fa.survey)

fa.diagram(fa.survey)

factanal.survey <- factanal(data, factors=4, scores = c("regression"), rotation = "varimax")
print(factanal.survey)




