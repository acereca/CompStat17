data <- read.table("rmr_ISwR.dat", header = TRUE, sep = " ")
#print(data)

#linear fitting as in the lecture described
#define vectors for slope and intercept
a = c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8,
    3.0, 3.2, 3.4, 3.6, 3.8, 4.0, 4.2, 4.4, 4.6, 4.8, 5.0, 5.2, 5.4, 5.6, 5.8,
    6.2, 6.4, 6.6, 6.8, 7.0, 7.2, 7.4, 7.6, 7.8, 8.0)
b = c(800, 810, 820, 830, 840, 850, 860, 870, 880, 890,
    900, 910, 920, 930, 940, 950, 960, 970, 980, 990,
    1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090,
    1100, 1110, 1120, 1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200)

lengthData = length(data$body.weight)
lengthA = length(a)
lengthB = length(b)
sum = 1000000000
temp = 0
x = data$body.weight
y = data$metabolic.rate
a_final = 0
b_final = 0

#calculate the sigma as given in the sheet
for(i in 1:lengthA) {
    for(j in 1:lengthB) {
        for(k in 1:lengthData) {
            y_fit = a[i] * data$body.weight[k] + b[j]
            temp = temp + (y[k] - y_fit)^2 / (lengthData - 2)
        }
        temp = sqrt(temp)
        if(temp < sum) {
            sum = temp
            a_final = a[i]
            b_final = b[j]
        }
    }
}

print(a_final)
print(b_final)

#linear fitting with lm function
fit_lin <-lm(metabolic.rate ~ body.weight, data = data)
summary(fit_lin)
#quadratic fitting with lm function
fit_quad <-lm(metabolic.rate ~ body.weight + I(body.weight^2), data = data)
#cubic fitting with lm function
fit_cubic <-lm(metabolic.rate ~ body.weight + I(body.weight^2) + I(body.weight^3), data = data)

#plot linear fits with and without lm function
png("exercise_10_1.png")
attach(mtcars)
par(mfrow = c(2, 2))
#data plot with calculated fit
plot(data, main = "data with calculated linear fit")
abline(b_final, a_final, col = "blue")
#linear fit plot with lm function
plot(data, main = "data with linear fit of lm function")
abline(fit_lin, col = "red")
#overlapping linear fits
plot(data, main = "overlapping fits (red lm, blue calculated)",
        col = rgb(0, 0, 0, 0.5))
abline(fit_lin, col = "red")
abline(b_final, a_final, col = "blue")
#plot with confidence limit
plot(data, main = "confidence limit 95%")
confint(fit_lin, level = 0.95)


png("exercise_10_lm.png")
attach(mtcars)
par(mfrow = c(2, 2))
#data plot
plot(data, main = "data")
#linear fit plot with lm function
plot(data, main = "data with linear fit of lm function")
abline(fit_lin, col = "red")
#quadratic fit plot with lm function
plot(data, main = "data with quadratic fit of lm function")
lines(data$body.weight, fitted(fit_quad), col = "red")
#cubic fit plot with lm function
plot(data, main = "data with cubic fit of lm function")
lines(data$body.weight, fitted(fit_cubic), col = "red")


#prediction for 150 and 200 kg
new.data <- data.frame(body.weight = c(150, 200))
predict(fit_lin, new.data, se.fit = TRUE)
