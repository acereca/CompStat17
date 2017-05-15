#! /usr/bin/Rscript

# problem 1: survey

nsurvey = 1000
nppl = 120

mat = matrix(,nsurvey,nppl)
mon = matrix(,nsurvey,12)

for (i in 1:nsurvey){
    mat[i,] = sample(1:12,nppl,replace=T)

    for (k in 1:12){
        mon[i,k] = sum(mat[i,]==k)
    }
}

mon = c(mon[1:nsurvey,])

paste('mean:',mean(mon))
paste('std. dev:',sd(mon))

png('01-1.png')
hist(mat, breaks= 0:13)

survey_hist = hist(mat)
png('01-2.png')
barplot(survey_hist$counts/sum(survey_hist$counts),
    main=paste('Histogram for',nsurvey, 'trials of', nppl, 'samples each'),
    xlab='month')

png('01-3.png')
hist(mon, breaks=0:30)


# problem 2: birthday paradow

ndata = 2:50

# analytic vs approximation1 vs approximation2
exactdata = 1-(365-ndata+1)**(ndata-1)/(365**(ndata-1))
assumdata = 1-exp(-ndata*(ndata-1)/365/2)
assumdata2 = 1-exp(-ndata**2/365/2)

png('01-4.png', width=1000, height=500)
plot(ndata,exactdata, pch=20, cex=1, xlab='N: number of ppl', ylab='P(two ppl have same bday)')
points(ndata,assumdata, pch=20, col=2, cex=1)
points(ndata,assumdata2, pch=20, col=3, cex=1)
abline(h=.5, col='#00000050')
legend(1,1,c("exact","assumed exp.","assumed exp. 2"),col=c(1,2,3),pch=c('-','-','-'))

# problem 2.2: birthday simulation

N=50
sims = 5000
bday = 1:365
answer = numeric(N)

for (i in 1:N){
    count = 0

    for (k in 1:sims){
        room = sample(bday, i, replace=T)
        if (length(unique(room)) < length(room)){
            count = count +1
        }
    }

    answer[i] = count/sims
}

png('01-5.png')
plot(1:N, answer, pch=20)
abline(h=.5, col='#00000050')
