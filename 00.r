#! /usr/bin/Rscript

x = 4
y = "universe"
flag1 = T
flag2 = F
class(y)

y = as.integer(4)
class(y)
typeof(y)

z = as.complex(3+3i)
z

# RNG! FTW

x = runif(10,7,12)
x

y = sample(1:10,5,replace=T)
y

z = rnorm(20,4,.5)
z

length(z)

z[z > 4.5]

sum(z)/length(z)

# structures

j = numeric(50)
j

dim(j) = c(10,5)
j

h = matrix(1:16,4,4)
h

h*2

k = 7:10

h*k
h%*%k
k%*%h

# DataFrames

mtcars

# control sequences

for (i in 2:12){
    print(paste('hi no',i))
}
