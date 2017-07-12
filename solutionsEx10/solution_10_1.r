data <- read.table("", header = TRUE)
#chi^2 method
intercept = seq(400, 900, by = 1)
slope = seq(0.0, 10, by = 0.01)
grid = matrix(0, length(intercept), length(slope))
for(i in 1:length(intercept)) {
    for(j in 1:length(slope)) {
        diff = (data$metabolic.rate - (data$body.weight * slope[j] + intercept[i]))^2
        grid[i, j] = sqrt(sum(diff) / length(data$body.weight) - 2)
    }
}

minPos = which(grid == min(grid), arr.ind = TRUE)

a = slope[minPos[2]]
b = intercept[minPos[1]]
print(a)
print(b)

lin.fit = lm(metabolic.rate ~ body.weight, data = data)
summary(lin.fit)

#95% confidence limit
conf = confint(lin.fit)
a25 = as.double(conf[2, 1])
b25 = as.double(conf[1, 1])
a975 =  as.double(conf[2, 2])
b975 = as.double(conf[1, 2])

png("")
plot(data$body.weight, data$metabolic.rate)
abline(lin.fit)
lines(data$body.weight, data$body.weight * a25 + b25)
lines(data$body.weight, data$body.weight * a975 + b975)

#quadratic fit
y = data$metabolic.rate
bw = data$body.weight
bw2 = bw^2
bw3 = bw^3

quad = lm(y ~ bw + bw2)
cub = lm(y - bw + bw2 + bw3)
summary(quad)   #bw = Q_0, bw2 = Q_1, bw3 = Q_2 ...
summary(cub)

png("")
bwplot = seq(min(bw) - 10, max(bw) + 10, by = 0.1)
quadplot = predict(quad, list = (bw = bwplot, bw2 = bwplot^2))
cubplot = predict(quad, list = (bw = bwplot, bw2 = bwplot^2, bw3 = bwplot^3))
plot(data$body.weight, data$metabolic.rate)
lines(bwplot, quadplot)
lines(bwplot, cubplot)

#predict body weights
new.weights = data.frame(c(150,200))
predict(lin.fit, newdata = new.weights, se.fit = TRUE)
