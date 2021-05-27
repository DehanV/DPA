PSID = read.csv("C:\\MBA\\Sem 4\\Analytics\\PSID.csv", header = TRUE)
PSID #List all the rows and columns
str(PSID)
summary(PSID)


hist(PSID$age)
hist(PSID$educatn)
hist(PSID$earnings)
hist(PSID$hours)
hist(PSID$kids)

PSID$kids[PSID$kids >= 98] <- 3
PSID$educatn[PSID$educatn >= 98] <- 12

PSID$educatn[PSID$educatn == 'NA'] <- 0
kc = kmeans(PSID[4:8], 3)

plot(PSID[,5:6], col=kc$cluster, cex.main = 0.75)
points(kc$centers[, 1:2], col=1:3, pch=8, cex=2)


table(PSID$married)



hist(PSID$kids)
hist(PSID$educatn)

PSID_train<-PSID[1:3642,]
PSID_test<-PSID[3643:4856,]


#make model
library(rpart)

PSID_Model<-rpart(kids~age+educatn+earnings+hours+married, PSID_train)
#make visualization
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(PSID_Model, digits=3, fallen.leaves = TRUE,type = 3, extra=101)

#LineaRegression
lmTemp = lm(earnings~educatn, data = PSID) #Create the linear regression
plot(PSID[,5:6], col="blue", cex.main = 0.75) #Plot the results
abline(lmTemp) #Add a regression line