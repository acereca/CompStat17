library(MASS)
library(mnormt)
library(car)

#import data
data <- read.table("bivariate-measurements.txt", header = TRUE, sep = " ")

n_row = dim(data)[1]
n_col = dim(data)[2]

#calculating mean vector
mean <- matrix(0, nrow = 2, ncol = 1)
for(col in 1:n_col) {
    for(row in 1:n_row) {
        mean[col] = mean[col] + data[row, col]
    }
    mean[col] = mean[col] / n_row
}

#calculating difference matrix and variance / deviation
diffMat <- matrix(0, nrow = n_row, ncol = n_col)
var <- matrix(0, nrow = n_col, ncol = 1)
dev <- matrix(0, nrow = n_col, ncol = 1)
for(col in 1:n_col) {
    for(row in 1:n_row) {
        diffMat[row, col] = data[row, col] - mean[col]
        #calculating variance for corrolation matrix
        var[col] = var[col] + (data[row, col] - mean[col])^2
    }
    #calculating deviation
    dev[col] = sqrt(1/(n_row - 1) * var[col])
}

#calculating covariance matrix
covMat <- (n_row - 1)^(-1) * t(diffMat) %*% diffMat
covMat

#calculating corrolation matrix
corMat <- matrix(0, nrow = dim(covMat)[1], ncol = dim(covMat)[2])
for(i in 1:dim(covMat)[1]) {
    for(j in 1:dim(covMat)[2]) {
        corMat[i, j] = covMat[i, j] / (dev[i]*dev[j])
    }
}
corMat

dataplot = mvrnorm(n_row, mu = mean, Sigma = covMat)
#plotting the data
png('dataPlot.png')
plot(dataplot, xlab = "V1", ylab = "V2")

#plotting ellipses with confidence regions
png('ellipsesPlot.png')
dataEllipse(as.matrix(dataplot), levels = c(0.6827, 0.9545),
                lwd = 3, asp = 1, xlab = "V1", ylab = "V2",
                pch = 20)
