# redshift coupling eq 1
zD = function(omm, omb, h0){
    B1 = 0.313*(omm*h0^2)^(-.419)*(1+.607*(omm*h0^2)^.674)
    B2 = .238 * (omm *h0^2)^.223

    return(1291*(omm*h0^2)^(-.419)/(1+.659*(omm *h0^2)^.828)*(1+B1*(omm *h0^2)^B2))
}

zD(.24, .048, .73)

# hubble eq 6
hubble = function(a, omm, omb, omr, hn){
    Hn = hn *100
    return(sqrt(Hn^2*(omm*a^-3+omr*a^-4+(1-omm+omr))))
}

# distance measure eq 9
Dv = function(a, omm, omb, omr, hn, c){

    hub = hubble(a, omm, omb, omr, hn)

    dlarg = function(a){
        return(1.0/(a^2*hubble(a, omm, omb, omr, hn)))
    }
    dldv = integrate(dlarg, a, 1.0)$value

    return((1.0/a-1.0)*(dldv^2*z/hub*c^3)^(1.0/3))
}

# comoving sound horizon
rs = function(a, omm, omb, omr, hn, c){
    rsargs = function(a){
        hub = hubble(a, omm, omb, omr, hn)
        return(1.0/(a^2*hub*sqrt(1.0+(31500*omb*hn^2*(2.7255/2.7)^-4))))
    }

    rsdv = integrate(rsargs, 1e-8, a)$value

    return(3/sqrt(2)*rsdv)
}

# BAO chi^2
baochisq = function(omm, omb, omr, hn, baomatr, c){
    a = 1.0/(1.0+ c(.106, .35, .57))

    Dvn = Vectorize(Dv, vectorize.args='a')
    #diff = numeric(3)

    d = rs(1.0/(zD(omm, omb, hn)+1), omm, omb, omr, hn, c) / Dvn(a, omm, omb, omr, hn, c)

    diff = d - c(.336, .1126, .0732)

    chisqbao = diff %*% baomatr
    chisqbao = chisqbao %*% diff

    return(chisqbao)
}

# baomatr
baomatr = matrix(c(.015^2, 0, 0, 0, .0022^2, 0, 0, 0, .0012^2), 3,3,byrow = T)
baomatr = solve(baomatr)

# MCMC PARAMS
accepted = NULL
samples = 1e4
multiplicity = 0.0
misses = 0.0
c = 2.99752458e5 # km/s

# priors
hn_range = c(40, 100)
omm_range = c(.07, .99)
omb_range = c(.005, .08)

#init of mcmc
ommchi = .3
ombchi = .049
hnchi = 73.0
omrchi = ommchi / ( 1.0 + 25000 * ommchi * ( hnchi / 100.0 ) ^2 * ( 2.7255 / 2.7 ) ^-4 )


#MCMC
for (i in 1:samples){
    chi1 = baochisq(ommchi, ombchi, omrchi, hnchi/100, baomatr, c)
    ommtrial = ommchi + rnorm(1, 0.0, .01)
    ombtrial = ombchi + rnorm(1, 0.0, .005)
    hntrial = hnchi + rnorm(1, 0.0, 1)

    omrtrial = ommtrial/(1.0+25000*ommtrial*(hntrial/100)^2*(2.7255/2.7)^-4)

    if (hntrial < hn_range[1] || hntrial > hntrial[2] || ommtrial < omm_range[1] || ommtrial > omm_range[2] || ombtrial < omb_range[1] || ombtrial > omb_range[2]){
        alpha = 0
    } else {
        chi2 = baochisq(ommtrial, ombtrial, omrtrial, hntrial/100, baomatr, c)
        alpha = exp(-.5*chi2 + .5*chi1)

        if (runif(1) < alpha){
            if (multiplicity != 0){
                old = c(multiplicity, chi1, hnchi, ommchi, ombchi)
                accepted = rbind(accepted, old)
            }
            hnchi = hntrial
            ommchi = ommtrial
            ombchi = ombtrial
            omrchi = omrtrial
            multiplicity = 1
        } else {
            misses = misses +1
            multiplicity = multiplicity +1
        }
    }
}
