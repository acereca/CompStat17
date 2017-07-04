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
    print(theta)
    return(-1/2 * (S(2, OmegaM, OmegaL) - S(1, OmegaM, OmegaL)^2 / S(0, OmegaM, OmegaL)))
}

#flat universe
logLflat <- function(Om) {
    logLikelihood(c(Om, 1 - Om))
}

library('tidyverse')
dxvect <- 0.005
data = tibble(
    x = seq(0, .5, by = dxvect),
    y = sapply(x, logLflat),
    trueLH = exp(y - max(y)),
    normL = dxvect * sum(trueLH))

g = ggplot(data, aes(x = x, y= trueLH/normL)) + geom_point()
ggsave('plot1.png')

lh = function(Om, Ol) {
    logLikelihood(c(Om, Ol))
}

N = 100
xdata = c()
ydata = c()
for (i in 1:N) {
    for (j in 1:N) {
        xdata = c(xdata, i/N)
        ydata = c(ydata, j/N)
    }
}
data2 = tibble(
    x = xdata,
    y = ydata,
    z = lh(xdata, ydata)
    )
data2
g = ggplot(data2, aes(x = x, y= y, color=z)) + geom_point()
ggsave('plot2.png')
