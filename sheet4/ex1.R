num_randoms = 10000

orig_set = runif(num_randoms*2, min = -1, max = 1)
dim(orig_set) = c(2,num_randoms)

calc_set = orig_set[1,]**2 + orig_set[2,]**2
used_set = orig_set[,calc_set <= 1]

z1 <- function(vec) {
    return(vec[1]*sqrt(-2*(log(vec[1]**2+vec[2]**2))/(vec[1]**2+vec[2]**2)))
}

z2 <- function(vec) {
    return(vec[2]*sqrt(-2*(log(vec[1]**2+vec[2]**2))/(vec[1]**2+vec[2]**2)))
}

z1_set = 1:length(used_set[1,])
z2_set = 1:length(used_set[1,])
for (i in 1:length(used_set[1,])) {
    z1_set[i] = z1(used_set[,i])
    z2_set[i] = z2(used_set[,i])
}

pos_set = seq(-5,5,.1)

png('ex11.png')
h1 = hist(z1_set, breaks=pos_set, xlab='z1')
yfit1 = dnorm(pos_set, mean=0, sd=sd(z1_set))
yfit1 = yfit1 * diff(h1$mids[1:2])*length(z1_set)
points(pos_set, yfit1, cex=1, pch=20, col='red')


png('ex12.png')
h2 = hist(z2_set, breaks=pos_set, xlab='z2')
yfit2 = dnorm(pos_set, mean=0, sd=sd(z2_set))
yfit2 = yfit2 * diff(h2$mids[1:2])*length(z2_set)
points(pos_set, yfit2, cex=1, pch=20, col='red')
