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

png('ex1.png')
hist(z1_set, breaks=seq(-4,4,.1))
