knitr::opts_chunk$set(cache=TRUE)
options(scipen = 9999)
rm(list=ls())
library(dplyr)
library(Hmisc)
library(caret)
library(partykit)
library(randomForest)

#Import the loan data
loans <- read.csv(file.choose(),na="")
str(loans)
head(loans)

#Exploratory Data Analysis
describe(loans)
attach(loans)
summary(loans[loans$loan_amnt== "yes", "loan_amnt"])

#summary(loans[loans$default == "no", "purpose"])

#create our train and test set by random sampling
set.seed(456)

#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=loans$terms, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-loans[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-loans[-inTrain,]  

prop.table(table(Training$loan_amnt))
prop.table(table(Testing$loan_amnt))

#Using Decision Tree
library(tree)
loans_model_dt <- tree(loan_amnt~.,Training)

#Plotting our decision tree of our credit risk analysis
plot(loans_model_dt)

plot(loans_model_dt, type = "uniform")
text(loans_model_dt)

loans_pred_dt <- predict(loans_model_dt, Testing)
(dt_conft <- table("prediction" = loans_pred_dt,
                   "actual" = Testing$loan_amnt))

accu_dt <- round((dt_conft[1]+dt_conft[4])/sum(dt_conft[1:4]),4)
prec_dt <- round(dt_conft[4]/(dt_conft[2]+dt_conft[4]), 4)
reca_dt <- round(dt_conft[4]/(dt_conft[4]+dt_conft[3]), 4)
spec_dt <- round(dt_conft[1]/(dt_conft[1]+dt_conft[2]), 4)

paste("Accuracy:", accu_dt*100,"%")

paste("Precision:", prec_dt*100,"%")

paste("Recall:", reca_dt*100,"%")

paste("Specitifity:", spec_dt*100,"%")

confusionMatrix(loans_pred_dt, Testing$loan_amnt, positive = "yes")

size()
loans_model_dt2 <- ctree(terms~ ., Training, control = ctree_control(mincriterion = 0.7))
plot(loans_model_dt2)
