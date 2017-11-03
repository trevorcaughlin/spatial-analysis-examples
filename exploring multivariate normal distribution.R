# SIMULATING MULTIVARIATE DATA
# https://stat.ethz.ch/pipermail/r-help/2003-September/038314.html
# lets first simulate a bivariate normal sample
library(MASS)
# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix

print(Sigma) #understand that it is a matrix
# > Sigma
# [,1] [,2]
# [1,]  1.0  0.1
# [2,]  0.1  1.0

#arguments to mvrnorm (stands for multivariate random normal):
#1. n=sample size of draws
#2. mu = mean
#3. Sigma = variance/covariance matrix

####TASK 1: EXPERIMENT WITH mvrnorm draws
mvrnorm(1,mu=c(9,9),Sigma=Sigma)
#why does a sample size of 1 give you two variables?

mvrnorm(4,mu=c(9,9),Sigma=Sigma)

mvrnorm(2,mu=c(9,9),Sigma=Sigma)



####TASK 2: explore by randomly drawing a bunch of data
#how many sample sizes are here?
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)                                      

# Calculate kernel density estimate
#this just gives an estimate of density
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package

#what is in the center here, why?

# Contour plot overlayed on heat map image of results
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package

#3d plot1
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package

library(rgl)
col2 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn.kde, col = col2)

