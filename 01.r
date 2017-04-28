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
hist(mat, breaks= 0:13)

survey_hist = hist(mat)
pdf('01-2.pdf')
barplot(survey_hist$counts/sum(survey_hist$counts),
    main=paste('Histogram for',nsurvey, 'trials of', nppl, 'samples each'),
    xlab='month')

pdf('01-3.pdf')
hist(mon, breaks=0:30)


# problem 2: birthday paradow

ndata = 2:50
exactdata = 1-(365-ndata+1)**(ndata-1)/(365**(ndata-1))
assumdata = 1-exp(-ndata*(ndata-1)/365/2)
assumdata2 = 1-exp(-ndata**2/365/2)

pdf('01-4.pdf')
plot(ndata,exactdata, pch='.', cex=3)
points(ndata,assumdata, pch='.', col=2, cex=3)
points(ndata,assumdata2, pch='.', col=3, cex=3)
legend(1,1,c("exact","assumed exp.","assumed exp. 2"),col=c(1,2,3),pch=c('-','-','-'))
