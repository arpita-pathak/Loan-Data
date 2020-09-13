# check working directory
library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(mice)
library(party)
library(tree)

# Set the data set
loan = read.csv(file.choose(),header = TRUE,na="" )
names(loan)
tail(loan)
str(loan)
summary(loan)
nrow(loan)
ncol(loan)
dim(loan)

##Target Variable
loan$total.revol_bal = as.factor(loan$total.revol_bal)
str(loan)
set.seed(1234)

#split the data into training set and test set
pd = sample(2,nrow(loan),replace = TRUE,prob = c(0.8,0.2))
training_set = loan[pd == 1,]
test_set = loan[pd == 2,]

#Feature Scaling
x=training_set[,c(1,2,5,11,15,16,17,20,21,23,25,26,27,28,29,34,36)] = scale(training_set[,c(1,2,5,11,15,16,17,20,21,23,25,26,27,28,29,34,36)])

y=test_set[,c(1,2,5,11,15,16,17,18,19,20,21,23,25,26,27,28,29,30,34,35,36)] = scale(test_set[,c(1,2,5,11,15,16,17,18,19,20,21,23,25,26,27,28,29,30,34,35,36)])

#Modeling

loan_tree<-tree(loan_amnt~.,data.frame(x))
loan_tree
plot(loan_tree)
plot(loan_tree, type="uniform")
text(loan_tree)

#Prediction Rate for training set
length(predict(loan_tree))
length(data.frame(x)$loan_amnt)
tab=table(predict(loan_tree),data.frame(x)$loan_amnt)
accuracy_train <- sum(diag(tab))/sum(tab)

#Prediction Rate for test set
test_predict <- table(predict(loan_tree, newdata= test_set), test_set$loan_amnt)
print(test_predict)
accuracy_test <- sum(diag(test_predict))/sum(test_predict)

