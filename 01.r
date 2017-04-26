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

pdf('01-1.pdf')
hist(mat)

survey_hist = hist(mat)
pdf('01-2.pdf')
barplot(survey_hist$counts/sum(survey_hist$counts),
    main=paste('Histogram for',nsurvey, 'trials of', nppl, 'samples each'),
    xlab='month')

pdf('01-3.pdf')
hist(mon)
