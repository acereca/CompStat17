library('tidyverse')

data = read_csv('supernovae.csv')

subset = seq(1,580, by=15)
Nsubset = length(subset)
data = data[subset,]

H = function(z, OmegaM){
    return( sqrt( OmegaM*(1+z)**3+1-OmegaM ) )
}

dist_lum = function(z, OmegaM){
    integrand = function(zz){
        return(1/H(zz,OmegaM))
    }
    print(z)
    print(integrate(integrand, 0, z))
    return((1+z)*integrate(integrand, 0, z)$value)
}

dist_mod = function(z, OmegaM){
    return(5*log10(dist_lum(z, OmegaM)))
}

helper_sum = function(n, OmegaM){
    s = 0
    miu = sapply(data$redshift, function(z)dist_mod(z, OmegaM))
    miu
    print(miu-data$distancemodulus)
    return(sum((miu-data$distancemodulus)**n/data$sigma**2))
}

log_lh = function(OmegaM){
    return(-.5*(helper_sum(2, OmegaM)-(helper_sum(1, OmegaM)**2/helper_sum(0, OmegaM))))
}

flat = function(Om){
    log_lh(c(Om, 1-Om))
}

lh = function(Om, Ol){
}

data2 = tibble(x = seq(0,.5,.005), y=flat(x))
g = ggplot(data2, aes(x=x, y=y)) + geom

ggsave('plot.png)
