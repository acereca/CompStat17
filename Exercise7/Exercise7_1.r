#tableform
#SN    redshift    distancmodulus    sigma
sndata <- read.table("supernovae.csv", header = TRUE, sep = ",")

subset <- seq(1, 580, by = 15)
Nsubset <- length(subset)
sndata <- sndata[subset, ]

H <- function(z, OmegaM, OmegaL) {
    return(sqrt(OmegaM * (1 + z)^3 + OmegaL))
}

ComovingDist <- function(z, OmegaM, OmegaL) {
    integrand <- function(zz) {
        1/H(zz, OmegaM, OmegaL)
    }
    return(integrate(integrand, 0, z)$value)
}

LuminosityDist <- function(z, OmegaM, OmegaL) {
    dC <- ComovingDist(z, OmegaM, OmegaL)
    return((1 + z) * dC)
}

mu <- function(z, OmegaM, OmegaL) {
    return(5 * log10(LuminosityDist(z, OmegaM, OmegaL)))
}

S <- function(n, OmegaM, OmegaL) {
    mui <- sapply(sndata$redshift, function(z)mu(z, OmegaM, OmegaL))
    y <- mui - sndata$distancemodulus

    return(sum(y^n / sndata$sigma^2))
    #return(sum((mu(sndata$redshift, OmegaM, OmegaL) - sndata$distancemodulus)^n / sndata$sigma^2))
}

logLikelihood <- function(theta) {
    OmegaM <- theta[1]
    OmegaL <- theta[2]
    return(-1/2 * (S(2, OmegaM, OmegaL) - S(1, OmegaM, OmegaL)^2 / S(0, OmegaM, OmegaL)))
}

#flat universe
logLflat <- function(Om) {
    logLikelihood(c(Om, 1 - Om))
}

#step in omega m steping from 0 to 0.5
dxvect <- 0.005
xvect <- seq(0, .5, by = dxvect) #definition omega m vector

#computation of the tru log likelihood for vector omega m
yvect <- sapply(xvect, logLflat)

#exterminate max position of the log logLikelihood
maxPos <- which.max(yvect)
OmBF <- xvect[maxPos] #omega m best fit

#relative likelihood vector
yvecDif <- yvect - yvect[maxPos]

#true likelihood
LikD <- exp(yvecDif)

#normalization factor of likelihood (integrate over omega m)
normL <- dxvect * sum(LikD)
meanL <- dxvect * sum(xvect * LikD) / normL
print(meanL)

varL <- dxvect * sum((xvect - meanL)^2 * LikD) / normL
sigmaL <- varL * 1.5

png('figure.png')
plot(xvect, LikD / normL, xlab = "Omega M", ylab = "Likelihood", type = "l")

#fisher matrix
library(numDeriv)
Fisher <- -hessian(logLflat, OmBF) #equation 8
#likelihood predicted by fisher matrix
lines(xvect, dnorm(xvect, mean = OmBF, sd = Fisher^-.5), type = "l", lwd = 1.5, col = "red")
#gaussian likelihood using the statistic of true likelihood
lines(xvect, dnorm(xvect, mean = meanL, sd = sigmaL), type = "l", lwd = 1.5, col = "blue")

print(sigmaL)
print(Fisher^-.5)
