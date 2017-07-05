data <- read.table("rmr_ISwR_errors.dat", header = TRUE, sep = "\t")
print(data)

#define function for calculation of S_0, S_1, S_2, S_d, S_xd
S_n <- function(index, withd = FALSE) {
    lengthData = length(data$body.weight)
    sum = 0
    x = data$body.weight
    d = data$metabolic.rate
    sigma = data$errors
    if(withd == FALSE) {
        for(i in 1:lengthData) {
            sum = sum + x[i]^index / sigma[i]^2
        }
        #return(sum)
    } else {
        for(i in 1:lengthData) {
            sum = sum + (x[i]^index * d[i]) / sigma[i]^2
        }
    }

    return(sum)
}

#define matrices for calculation of set of linear equations
G <- matrix(c(S_n(0), S_n(1), S_n(1), S_n(2)), nrow = 2, ncol = 2, byrow = TRUE)
D <- c(S_n(0, TRUE), S_n(1, TRUE))
#compute set of equations (4.16), (4.17) as in the recent lecture notes
b_a = solve(G, D)

fit_lm <- lm(metabolic.rate ~ body.weight, data = data, weights = errors)

png("exercise10_2.png")
attach(mtcars)
par(mfrow = c(2, 2))
#data plot
plot(data$body.weight, data$metabolic.rate, main = "data",
        ylim = range(c(data$metabolic.rate - data$errors,
                    data$metabolic.rate + data$errors)), pch = ".")
arrows(data$body.weight, data$metabolic.rate - data$errors ,data$body.weight,
        data$metabolic.rate + data$errors, length = 0.05, angle = 90, code = 3)
#data plot with calculated linear fit
plot(data$body.weight, data$metabolic.rate, main = "witch calculated linear fit")
abline(b_a[1], b_a[2], col = "blue")
#data plot with lm function linear fit
plot(data$body.weight, data$metabolic.rate, main = "with lm function linear fit")
abline(fit_lm, col = "red")
#data plot witch both fit for comparison
plot(data$body.weight, data$metabolic.rate, main = "both linear fits for comparison",
        ylim = range(c(data$metabolic.rate - data$errors,
                    data$metabolic.rate + data$errors)),
        col = rgb(0, 0, 0, 0.5), pch = ".")
abline(fit_lm, col = "red")
abline(b_a[1], b_a[2], col = "blue")
arrows(data$body.weight, data$metabolic.rate - data$errors ,data$body.weight,
        data$metabolic.rate + data$errors, length = 0.05, angle = 90, code = 3,
        col = rgb(0, 0, 0, 0.5))
