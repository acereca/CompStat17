sndata <- read.table("supernovae.csv", header = TRUE, sep = ",")

#comoving distance
comovingDistance <- function(z, omegaM, omegaL) {
    integrand <- function(zz) {
        1 / sqrt(omegaM * (1 + zz)^3 + omegaL)
    }

    return(integrate(integrand, 0, z)$value)
}

#luminosity distance
luminosityDistance <- function(z, omegaM, omegaL, omegaK) {
    #here omegaK = 1 - omegaM - omegaL
    #omegaK <- 1 - omegaM - omegaL
    if(omegaK == 0) {
        d_L <- (1 + z) * comovingDistance(z, omegaM, omegaL)
    } else if(omegaK > 0) {
        d_L <- (1 + z) * sinh(sqrt(omegaK) * comovingDistance(z, omegaM, omegaL)) / sqrt(omegaK)
    } else if(omegaK < 0) {
        d_L <- (1 + z) * sin(sqrt(-omegaK) * comovingDistance(z, omegaM, omegaL)) / sqrt(-omegaK)
    }

    return(d_L)
}

#distancemodulus predicted by theory
distancemodulusTheory <- function(z, omegaM, omegaL, omegaK) {
    return(5 * log10(luminosityDistance(z, omegaM, omegaL, omegaK)))
}

#auxiliary function
S_n <- function(n, omegaM, omegaL, omegaK) {
    #distancemodulus theory for redshift z
    mu <- sapply(sndata$redshift, function(z)distancemodulusTheory(z, omegaM, omegaL, omegaK))
    y <- sndata$distance - mu

    return(sum(y^n / sndata$sigma^2))
}

logLikelihood <- function(theta) {
    omegaM = theta[1]
    omegaL = theta[2]
    omegaK = 1 - omegaM - omegaL

    return(-1/2 * (S_n(2, omegaM, omegaL, omegaK) - S_n(1, omegaM, omegaL, omegaK)^2 / S_n(0, omegaM, omegaL, omegaK)))
}

logLflat <- function(om) {
    logLikelihood(c(om, 1 - om))
}

logL <- function(om, ol) {
    logLikelihood(c(om, ol))
}

N = 20
xdata = c()
ydata = c()
zdata = c()
for (i in 1:N){
    for (j in 1:N){
            xdata = c(xdata, i/N)
            ydata = c(ydata, j/N)
            zdata = c(zdata, logL(i/N, j/N))
    }
}
zdata2 <- matrix(zdata, ncol = N, byrow = TRUE)

png('plot1.png')
plot(xdata, ydata, col = abs(zdata))

#png('contour.png')
#contour((1:N)/N, (1:N)/N, zdata2)

png('image1.png')
image(zdata2)


###############################################################################
#exercise 2
###############################################################################

confidenceLevel <- function(omegaM) {
    integrand <- function(mm) {
        0.95 * logLflat(mm) + mm - mm  #r übernimmt nur vektorartige funktionen das der akzeptiert i nicht als wert wenn übergeben voll komisch
    }
    return(integrate(integrand, 0, 1)$value)
}

#step in omega m steping from 0 to 0.5
dxvect <- 0.005
xvect <- seq(0, .5, by = dxvect) #definition omega m vector

#computation of the tru log likelihood for vector omega m
yvect = c()
for(i in 1:length(xvect)) {
    yvect <- c(yvect, confidenceLevel(i))
}
print(max(yvect))
print(min(yvect))
png('exercise2.png')
plot(yvect)
