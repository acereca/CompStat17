# solution for sheet2 (marked), probem no. 3

# analytical function given to us
dist_f <- function(a,b) {
  lambda <- a/b
  d <- sqrt(a**2+b**2)
  return(1/15*(a*lambda**2+b/(lambda**2)+d*(3-lambda**2-1/(lambda**2))+5/2*(b/lambda*log((a+d)/b)+a*lambda*log((b+d)/a))))
}

# creation of random datapoints in a 60 x 50 cm rectangle

ssize = 10000

xsamples = runif(ssize, min = 0, max = 60)
xsamples2 = runif(ssize, 0, 60)
ysamples = runif(ssize, min = 0, max = 50)
ysamples2 = runif(ssize, min = 0, max = 50)

dsamples = sqrt((xsamples-xsamples2)**2+(ysamples-ysamples2)**2)


png('p3_sampleplot.png')
hist(
    dsamples,
    breaks = seq(0,sqrt(61)*10, length.out=100)
)
segments(dist_f(60,50),0,dist_f(60,50),250, col=2)

# analytical function given to us
dist_func <- function(a,lambda) {
  b <- a/lambda
  d <- sqrt(a**2+b**2)
  return(1/15*(a*lambda**2+b/(lambda**2)+d*(3-lambda**2-1/(lambda**2))+5/2*(b/lambda*log((a+d)/b)+a*lambda*log((b+d)/a))))
}

ndata = exp(seq(log(.01),log(100),length.out = 100))
print(ndata)

png('p3_anaplot.png')
plot(
  ndata,
  dist_func(1,ndata)/sqrt(1+(1/ndata)**2),
  log = 'x',
  xlab = 'lambda',
  ylab = 'd_1,2 / d',
  pch = 20,
  cex = 1
)
