install.packages('e1071', dependencies=TRUE)
library(tidyverse) # metapackage of all tidyverse packages
library(DataExplorer) # to make intro_plot()
library(gridExtra) # to make grid.arrange()
library(caret)
library(randomForest)
library(rpart) 
library(rpart.plot)


churndata = read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

attr(churndata, 'spec') <- NULL # just to remove a warning about specifications

str(churndata)
#data set consists of 21 features and 7,043 observations, it's a relatively small dataset which will enable to do cross validation

plot_intro(churndata) #overview on some aspects about the data

#most of data is discrete, there no a lot of missing values but need to deal with them by removing or putting values and no columns missing completely.

head(as.integer(churndata$Churn == 'Yes')) # convert into numerical

hist(as.integer(churndata$Churn == 'Yes') , col = 'darkblue' , main = 'Distribution of the target variable')

#target feature is inbalanced as much more values are 0 so need to check if the model will be baised toward 0 or not

table(churndata$Churn)/nrow(churndata) # seeing the fraction of the zeros and ones in the data

churndata$customerID <- NULL #remove customer ID

head(churndata)

colnames(churndata)[colSums(is.na(churndata)) > 0] # getting the columns that have missing values

churndata2 = churndata %>% select(c(tenure,MonthlyCharges,TotalCharges)) %>% mutate(tenure_TotalCharges = tenure * MonthlyCharges , difference = tenure_TotalCharges -TotalCharges  )
head(churndata2)
summary(churndata2$difference)
sd(na.omit(churndata2$difference))

#multiplying the tenure in the monthly charge will get a value that is very close to the totalCharge with mean difference than TotalCharge = -0.1532, median = 0 and std of 67 so I can replace the NA with the values from this calculated column

churndata2[rowSums(is.na(churndata2)) > 0,]

churndata[rowSums(is.na(churndata2)) > 0,'TotalCharges'] = churndata2[rowSums(is.na(churndata2)) > 0,'MonthlyCharges']
anyNA(churndata) # is there any NA in the data ?

binary_cols = c('Partner','Dependents','PhoneService','PaperlessBilling','Churn')
for(col in binary_cols) # for each column go and convert it to 1 if Yes and 0 if No
{
  churndata[,col] = as.integer(churndata[,col] == 'Yes')
  
}

churndata = churndata %>% mutate_if(is.character,as.factor)  %>% mutate_if(is.factor, as.numeric)
str(churndata) # check if the data is numerical now

# split the data into train, validation and test set
churndata = as.data.frame(churndata)
set.seed(2)
train_index <- createDataPartition(churndata$Churn, p = 0.7 , list = FALSE) # Taking 30% to make a validation and test set
train = churndata[train_index,]
val_test = churndata[-train_index,]
#making the train test data
set.seed(3)
val_index <- createDataPartition(val_test$Churn, p =0.5 , list = FALSE) # # Taking 15% to make a validation and 15% and test set
val = val_test[val_index,]
test = val_test[-val_index,]

nrow(train)
nrow(val)
nrow(test)





# logistic regression

set.seed(123)
glm <- glm(Churn ~ ., data = train, family = "binomial")                 
prob_preds <- predict(glm, newdata = val , type = 'response') # predict and convert predictions into positive only

#calculate optimum cutoff value
cal_performance <- function(cutoff) 
{
  preds <- if_else(prob_preds < cutoff, 0, 1)
  conf <- confusionMatrix(as.factor(preds), as.factor(val$Churn), positive = "1")
  sensitivity <- conf$byClass[1]
  pos_pred_value <- conf$byClass[3] 
  accuray <- conf$overall[1]
  
  out <- t(as.matrix(c(sensitivity, pos_pred_value, accuray))) 
  colnames(out) <- c("sensitivity", "pos_pred_value", "accuracy")
  return(out)
}

cutoffs = seq(0.01,0.80,length=100) # trying 100 different cutoff within the range of 0.01 and 0.8
results = matrix(0,100,3) # initializing a matrix with 100 row and 3 columns one row for each cutoff results 

for(i in 1:100)
{
  results[i,] = cal_performance(cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
} 

# plot the result to determine the best cutoff
options(repr.plot.width =15, repr.plot.height =8)
plot(cutoffs, results[,1],xlab="Cutoffs",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoffs,results[,2],col="darkgreen",lwd=2)
lines(cutoffs,results[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Pos_Pred_Value","Accuracy"))
axis(1, at = seq(0.1, 1, by = 0.1))


# making 0.35 as the cutoff
preds <- if_else(prob_preds < 0.35, 0, 1)

# evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix


# Decision tree


#train and predict
set.seed(123)
Dtree = rpart(Churn ~., data = train, method = "class")  # method = class to make it binary classification
prob_preds <- predict(Dtree,type = "prob", newdata = val)

# plot the result to determine the best cutoff

cal_performance <- function(cutoff) 
{
  preds = ifelse(prob_preds[,2] >= cutoff , 1 , 0) # if the probability if class one is greater than 0.3 then make the class 1 other wise make it o
  conf <- confusionMatrix(as.factor(preds), as.factor(val$Churn), positive = "1")
  sensitivity <- conf$byClass[1]
  pos_pred_value <- conf$byClass[3] 
  accuray <- conf$overall[1]
  
  out <- t(as.matrix(c(sensitivity, pos_pred_value, accuray))) 
  colnames(out) <- c("sensitivity", "pos_pred_value", "accuracy")
  return(out)
}


for(i in 1:100)
{
  results[i,] = cal_performance(cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
}



options(repr.plot.width =15, repr.plot.height =8)
plot(cutoffs, results[,1],xlab="Cutoffs",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoffs,results[,2],col="darkgreen",lwd=2)
lines(cutoffs,results[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Pos_Pred_Value","Accuracy"))
axis(1, at = seq(0.1, 1, by = 0.1))

#cutoff as 0.25

Dtree = rpart(Churn ~., data = train, method = "class")  # method = class to make it binary classification
prob_preds <- predict(Dtree,type = "prob", newdata = val)
DT_preds = ifelse(prob_preds[,2] > 0.25 , 1 , 0) 
#evaluating the model
confusion_table <- table(DT_preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(DT_preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

options(repr.plot.width = 15, repr.plot.height = 8)
rpart.plot(Dtree)

#With this cutoff, there is good sensitivity but overall less accuracy. Still using Logistic Regression with a cutoff = 0.35




# Random forest


set.seed(123)

#train and predict
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=500, do.trace=FALSE )
preds <- predict(RF, newdata=val)
# plot the importance
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=500, do.trace=FALSE )
imp_RF <- importance(RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MeanDecreaseAccuracy = imp_RF[,3])
imp_DF <- imp_DF[order(imp_DF$MeanDecreaseAccuracy, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, fill=MeanDecreaseAccuracy)) + geom_bar(stat = 'identity') + labs(x = 'Features', y= 'Feature Importance') + coord_flip() + theme(legend.position="none")

plot(RF , main = "Error by number of trees")

#Observing the plot, considering less than 100 trees

#plot the result to determine the best cutoff

set.seed(123)

#train and predict
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=100, do.trace=FALSE )
prob_preds <- predict(RF, newdata=val , type = 'prob')

cal_performance <- function(cutoff) 
{
  preds = ifelse(prob_preds[,2] >= cutoff , 1 , 0) 
  conf <- confusionMatrix(as.factor(preds), as.factor(val$Churn), positive = "1")
  sensitivity <- conf$byClass[1]
  pos_pred_value <- conf$byClass[3] 
  accuray <- conf$overall[1]
  
  out <- t(as.matrix(c(sensitivity, pos_pred_value, accuray))) 
  colnames(out) <- c("sensitivity", "pos_pred_value", "accuracy")
  return(out)
}


for(i in 1:100)
{
  results[i,] = cal_performance(cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
} 

options(repr.plot.width =15, repr.plot.height =8)
plot(cutoffs, results[,1],xlab="Cutoffs",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoffs,results[,2],col="darkgreen",lwd=2)
lines(cutoffs,results[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Pos_Pred_Value","Accuracy"))
axis(1, at = seq(0.1, 1, by = 0.1))

#Optimum cutoff is 0.28

set.seed(123)

#train and predict
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=100, do.trace=FALSE )
prob_preds <- predict(RF, newdata=val , type = 'prob')
RF_preds = ifelse(prob_preds[,2] >= 0.28 , 1 , 0)
#evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(RF_preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix


#Logistic Regression shows better overall accuracy and better sensitivity

# logistic regression combined
set.seed(123)
train_full = rbind(train,val) # concating the train and validation to train on both of them
glm <- glm(Churn ~ ., data = train_full, family = "binomial")                 
prob_preds <- predict(glm, newdata = test , type = 'response') # predict and convert predictions into positive only
# making 0.35 as a threshold
glm_preds <- if_else(prob_preds < 0.35, 0, 1)

# evaluating the model
confusion_table <- table(glm_preds, test$Churn)
confusionMatrix = confusionMatrix( as.factor(glm_preds) , as.factor(test$Churn),positive = "1" )

confusion_table
confusionMatrix