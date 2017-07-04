library('ggplot2')
ntests = 1000

# part 1
scatter_prb <- function(theta, g){
    1/(4*pi)*(1-g^2)/(1+g^2-2*g*cos(theta))^(3/2)
}

xdata = seq(0, pi, length.out=100)

data = sample(xdata,ntests,rep=TRUE,prob=2*pi*sin(xdata)*scatter_prb(xdata, .5))
h = hist(data, breaks=seq(0,pi, length.out=20))
h = data.frame(
    matrix(
        c(h$mids, h$density),
        ncol=2,
        dimnames=list(NULL,c('mids', 'density'))
        )
    )

exp_data = list()
for (i in 1:3) {
    g = i*.2-.1
    exp_data[[i]] = data.frame(matrix(
        c(xdata,2*pi*sin(xdata)*scatter_prb(xdata, g)),
        ncol=2,
        dimnames=list(NULL, c('x','y'))
    ))
    exp_data[[i]]$model = paste('g =', g)
}

exp_data = rbind(exp_data[[1]], exp_data[[2]], exp_data[[3]])

ggplt = ggplot() + xlab('theta / rad') + ylab('probability')
ggplt = ggplt + geom_point(data=h, aes(x=mids, y=density))
ggplt = ggplt + geom_line(data=exp_data, aes(x=x, y=y, group=model, col=model))
ggsave('6-2_scatter_prob_b.png', height=1080/300, width=1920/300)

#mean and variance
paste('theta_mean = ', mean(data))
paste('theta_var  = ', var(data))

# part 2
scatter_eta <- function(eta, g){
    2*pi*scatter_prb(acos(eta),g)
}

xdata = seq(-1,1,.01)
data = sample(xdata,ntests,rep=TRUE,prob=scatter_eta(xdata,.5))
h = hist(data, breaks=seq(-1,1, length.out=20))

dat2h = data.frame(
    matrix(
        c(h$mids, h$density),
        ncol=2,
        byrow=F,
        dimnames=list(NULL, c('hx', 'hy'))
        )
    )

dat2e = data.frame(
    matrix(
        c(xdata, scatter_eta(xdata, .5)),
        ncol=2,
        dimnames=list(NULL, c('dx', 'dy'))
        )
    )
gplt2 = ggplot() + xlab('eta') + ylab('probability')
gplt2 = gplt2 + geom_line(data = dat2e, aes(x=dx, y=dy, group='PDF'), colour='#ff0000')
gplt2 = gplt2 + geom_point(data = dat2h, aes(x=hx, y=hy, group='samples'))
ggsave('6-2_scatter_eta_b.png', width=1920/300, height=1080/300)
