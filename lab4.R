library(MVA)


#3.4 Should principal components be extracted from the covariance or the correlation matrix?
bc <- c(
    0.290,           
    0.202,  0.415,       
   -0.055,  0.285,  0.419,       
   -0.105, -0.376, -0.521, -0.877,      
   -0.252, -0.349, -0.441, -0.076,  0.206,
   -0.229, -0.164, -0.145,  0.023,  0.034,  0.192,
    0.058, -0.129, -0.076, -0.131,  0.151,  0.077,  0.423)

blood_sd <- c(rblood = 0.371, plate = 41.253,  wblood = 1.935,
              neut = 0.077, lymph = 0.071, bilir = 4.037,
              sodium = 2.732, potass = 0.297)

blood_corr <- diag(length(blood_sd)) / 2

blood_corr[upper.tri(blood_corr)] <- bc     

blood_corr <- blood_corr + t(blood_corr)
