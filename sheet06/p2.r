<ntests = 1000

# part 1
scatter_prb <- function(theta, g){
    1/(4*pi)*(1-g^2)/(1+g^2-2*g*cos(theta))^(3/2)
}

png('6-2_scatter_prob.png')
xdata = seq(0, pi, length.out=100)
#plot(0,type='n', xlim=c(0,pi), xlab="theta / pi", ylab="p(theta)", xaxt='n')

data = sample(xdata,ntests,rep=TRUE,prob=2*pi*sin(xdata)*scatter_prb(xdata, .5))
h = hist(data, breaks=seq(0,pi, length.out=20))
plot(h$mids, h$density, xaxt='n', xlab="theta / pi", ylab="p(theta)", pch=20)

for (g in seq(.1,.5,.2)) {
    lines(xdata, 2*pi*sin(xdata)*scatter_prb(xdata, g), col=1+g*10)
}
axis(1, at=seq(0,pi,pi/4),labels=seq(0, 1, 1/4), las=2)

#mean and variance
paste('theta_mean = ', mean(data))
paste('theta_var  = ', var(data))

# part 2
scatter_eta <- function(eta, g){
    scatter_prb(acos(eta),g)
}
xdata = seq(-1,1,.01)

png('6-2_scatter_eta.png')
#plot(0,type='n', ylim=c(0,.8*ntests), xlim=c(-1,1), xlab="eta", ylab="p(eta)")
data = sample(xdata,ntests,rep=TRUE,prob=scatter_eta(xdata,.5))
h = hist(data, breaks=seq(-1,1, length.out=20))
plot(h$mids, h$density/4, xlim=c(-1,1), xlab="eta", ylab="p(eta)", pch=20)

lines(xdata, scatter_eta(xdata, .5), col=2)
