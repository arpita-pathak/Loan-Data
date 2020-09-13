# check working directory
library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(mice)

# Set the blank spaces to NA's
loan = read.csv(file.choose(), na = "")
View(loan)  # allows us to view the data set

loan_data = loan %>% select ( member_id, loan_amnt, terms, Rate_of_intrst,grade,
                              Experience, home_ownership, annual_inc, verification_status,
                              debt_income_ratio ,total.revol_bal)
loan_data  # 887379 obs. of 11 variables

loan1 = loan_data %>%
  filter(!is.na(annual_inc) , 
         !(home_ownership %in% c('NONE' , 'ANY')) , 
         Experience != 'n/a')
loan1    #omitted 842412 rows 

loan2<-loan1

loan2$home_ownership<-factor(loan2$home_ownership)
loan2$grade<-factor(loan2$grade)
loan2$terms<-factor(loan2$terms)
loan2$Experience<-factor(loan2$Experience)

set.seed(123) #locks seed for random partitioning

#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=loan2$terms, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-loan2[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-loan2[-inTrain,]  

########## LOGISTIC REGRESSION ######### (need rank to be factor)

#fill=factor(terms,c(0,1),c("w","f"))
M_LOG<-glm(terms~ ., data = Training, family = "binomial")
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint.default(M_LOG))) # It's summarizes the effect the variables have on determining if someone will or will not default on their loan
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)  

#sets the cross validation parameters to be fed to train()

M_CART <- train(terms ~., data = Training, trControl=train_control, tuneLength=10, method = "rpart") 

#increasing tune length increases regularization penalty

print(M_CART)
plot(M_CART) #produces plot of cross-validation results
M_CART$bestTune #returns optimal complexity parameter
summary(M_CART)
# Select only the columns mentioned above.
loan3 = loan2 %>%
  select(loan_amnt , Rate_of_intrst , grade , Experience ,annual_inc , terms )
loan3

#Removed the Experience data (1-9 years) since that data was not statistically significant in determinig whether or not one would default on the loan.

loan3<-subset(loan3, Experience!='1 year' & Experience!='2 years' & Experience!='3 years' & Experience!='4 years' & Experience!='5 years' &  Experience!='6 years' & Experience!='7 years' & Experience!='8 years' & Experience!='9 years'  )
set.seed(123) #locks seed for random partitioning

#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=loan3$terms, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-loan3[inTrain,]

#stores all rows not in the training set in the test/validation set
Testing<-loan3[-inTrain,]  

########## LOGISTIC REGRESSION ##########  (need rank to be factor)
M_LOG2<-glm(terms ~ ., data = Training, family = "binomial")

summary(M_LOG2)

#The table below shows the variables mean and two standard deviation range of results.

exp(cbind(M_LOG2$coefficients, confint.default(M_LOG)))
set.seed(123) #locks seed for random partitioning

#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=loan2$Rate_of_intrst, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-loan2[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-loan2[-inTrain,]  

########## LOGISTIC REGRESSION ########## (need rank to be factor)

M_LOG3<-lm(Rate_of_intrst ~ ., data = Training)
summary(M_LOG3)

#The table below shows the mean and the two standard deviaton range of the variables below.
exp(cbind(M_LOG3$coefficients, confint.default(M_LOG3)))

#Calculate in and out-of sample error

RMSE_IN<-sqrt(sum(M_LOG3$residuals^2)/length(M_LOG3$residuals))
RMSE_OUT<-sqrt(sum((predict(M_LOG3, Testing)-Testing$Rate_of_intrst)^2)/length(Testing))

#In-sample error
RMSE_IN

#Out of sample error
RMSE_OUT

#CART-Classification and Regression Trees 
#rpart package implementation

set.seed(123) #locks seed for random partitioning

#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=loan2$Rate_of_intrst, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-loan2[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-loan2[-inTrain,]  
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)  

#sets the cross validation parameters to be fed to train()
M_CART2 <- train(Rate_of_intrst ~., data = Training, trControl=train_control, tuneLength=10, method = "rpart") 
print(M_CART2)
plot(M_CART2) #produces plot of cross-validation results

M_CART2$bestTune #returns optimal complexity parameter

#Again calculated the in and out of sample error for the cleaned up model

RMSE_IN<-sqrt(sum(M_CART2$residuals^2)/length(M_CART2$residuals))
RMSE_OUT<-sqrt(sum((predict(M_CART2, Testing)-Testing$Rate_of_intrst)^2)/length(Testing))
RMSE_IN
RMSE_OUT



