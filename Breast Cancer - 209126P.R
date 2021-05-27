
install.packages("readr")
install.packages("gmodels")
install.packages("dplyr")
install.packages("class")


library(readr)
library(class)
library(gmodels)
library(dplyr)


bcd <- read.csv("BreastCancer.csv")
bcd
str(bcd)

bcd <- select(bcd,-id,-X) #Remove ID col and X

table(bcd$diagnosis)
round(prop.table(table(bcd$diagnosis)) * 100, digits = 1)

#357 from Benign Cells 212 from Malignant Cells (B=62.7% and M=37.3%)

sum(is.na(bcd)) #Check for missing values

head(bcd)

#Corelation
cor(select(bcd,-diagnosis))

summary(bcd)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

new_bcd <- as.data.frame(lapply(select(bcd,-diagnosis), normalize))


set.seed(1234)
#Separate Train and Test Data
indx = sample(2, nrow(new_bcd), replace=TRUE, prob=c(0.67, 0.33))

bcd_train = new_bcd[indx==1, 1:30] #Where sample index is 1
bcd_test = new_bcd[indx==2, 1:30] #Where sample index is 2
summary(bcd_train)


#Assign Labels
indxlbl = sample(2, nrow(bcd), replace=TRUE, prob=c(0.67, 0.33))
bcd_train_labels = bcd[indx==1, 1] 
bcd_test_labels = bcd[indx==2, 1]
bcd_train_labels




NROW(bcd_train_labels) 

i=1                          
k.optm=1  
best_acc=1
k.val=1
for (i in 1:NROW(bcd_train_labels)){ 
  knn.mod <-  knn(train=bcd_train, bcd_test, cl=bcd_train_labels, k=i)
  k.optm[i] <- 100 * sum(bcd_test_labels == knn.mod)/NROW(bcd_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
  if(k.optm[i]>=best_acc){
    best_acc <- k.optm[i]
    k.val <- k
    
  }
}

best_acc
k.val

bcd_test_pred <- knn(train = bcd_train, test = bcd_test, cl = bcd_train_labels, k = k.val)

cm =CrossTable(x = bcd_test_labels, y = bcd_test_pred, prop.chisq = FALSE)
cm

(112+71)/189*100


