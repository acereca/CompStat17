rand_walk <- function(nmax) {
    pos = 0
    steps = 0
    while (pos < nmax && pos > -nmax){
        step = sample(0:1,1) *2 -1
        pos = pos + step
        steps = steps +1
    }

    return(steps)
}

dist_rand_walk <- function(n){

    num_tests = 100
    res = 1:num_tests
    for (i in 1:num_tests) {
        res[i] = rand_walk(n)
    }

    return(res)
}

nset = 1:50
means = 1:max(nset)
for (i in nset){
    means[i] = mean(dist_rand_walk(i))
    print(i)
}
png('ex2.png')
plot(nset, means, xlab='n: width of exprimental area', ylab='average number of steps needed', pch=20, cex=1)
