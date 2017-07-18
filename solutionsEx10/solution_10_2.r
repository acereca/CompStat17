data <- read.table("", header = TRUE)
print(data)

lin.fit = lm(metabolic.rate ~ body.weight, data = data, weights = 1 / data$errors^2)
sumarry(lin.fit)
confint(lin.fit)

## matrix method ##
sds = data$errors
x = data$body.weight
y = data$metabolic.rate

S0 = sum(1 / sds^2)(
S1 = sum(x / sds^2)
S2 = sum(x^2 / sds^2)
Sdx = sum(y * x / sds^2)
sd = sum(y / sds^2)

G = matrix(c(S0, S1, S1, S2), 2, 2, byrow = TRUE)
D = c(Sd, Sdx)
sol = D %*% solve(G)
finf = solve(G)
sqrt(finf[1, 1])
sqrt(finf[2, 2])

## error bar plot ##
png("fitdata.png")
plot(data$body.weight, data$metabolic.rate)
abline(lin.fit)
arrow(data$body.weight, data$metabolic.rate - data$errors,
        data$metabolic.rate + data$errors, length = 0.05, angle = 90, code = 3)

## append exerciese ##
## chi^2 test ##
#z = (y_i - T_i) C_ij^-1 (y_j - T_j)
#z follow the chi^2 distribution with n - m degrees of freedom
#n is number of points, m number of fitting coeffitians
#mean z / (n - m) ~ 1 than its a good fit if not search for another fit
##
