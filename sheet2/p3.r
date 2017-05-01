dist_func <- function(a,lambda) {
  b <- a/lambda
  d <- sqrt(a**2+b**2)
  return(1/15*(a*lambda**2+b/(lambda**2)+d*(3-lambda**2-1/(lambda**2))+5/2*(b/lambda*log((a+d)/b)+a*lambda*log((b+d)/a))))
}

ndata = exp(seq(log(.01),log(100),length.out = 100))
print(ndata)

png('p3_distplot.png')
plot(
  ndata,
  dist_func(1,ndata)/sqrt(1+(1/ndata)**2),
  log = 'x',
  xlab = 'lambda',
  ylab = 'd_1,2 / d')
