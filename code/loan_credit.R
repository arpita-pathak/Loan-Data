library(gmodels)
library(lubridate)
library(dplyr)
library(ggplot2)
library(caTools)
library(e1071)
library(ROCR)
library(caret)
#Loading files into R data frame
#Loading data into R data frame
credit_data <- read.csv(file.choose())
attach(credit_data)
head(credit_data)
str(credit_data)
dim(credit_data)
tble <- tbl_df(credit_data)
glimpse(tble)
summary(credit_data)
#Selecting relevant features for model
new_data <- credit_data %>% select("loan_amnt","Rate_of_intrst","annual_inc","debt_income_ratio",delinq_2yrs,numb_credit,total.revol_bal,total_rec_int,total_rec_late_fee,acc_now_delinq,tot_curr_bal)
new_data <- credit_data %>% mutate(Rate_of_intrst = as.numeric(sub("%","", Rate_of_intrst)),
                                   debt_income_ratio = as.numeric(sub("%", "", debt_income_ratio)),
                                   numb_credit = as.numeric(numb_credit),
                                   total.revol_bal = as.numeric(total.revol_bal),
                                   loan_amnt = as.numeric(loan_amnt),
                                   delinq_2yrs = as.numeric(delinq_2yrs),
                                   total_rec_int = as.numeric(total_rec_int),
                                   total_rec_late_fee = as.numeric(total_rec_late_fee),
                                   acc_now_delinq = as.numeric(acc_now_delinq))
memory.size(max = TRUE)
memory.limit(size = NA)
                                   
summary(new_data)
set.seed(2)
s=sample(1:nrow(new_data),0.7*nrow(new_data)) # we have 70 % of data in Train and Test we have 30%.
loan_train=new_data[s,]
loan_test=new_data[-s,]
str(loan_test)
install.packages("mice")
install.packages("VIM")
library("mice")
library("VIM")
md.pattern(new_data)

aggr_plot <- aggr(new_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(new_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

imputed_Data <- mice(loan_train, m=5, maxit = 50, method = 'pmm', seed = 500)
 summary(imputed_Data)
 
 impute_Data <- mice(loan_test, m=5, maxit = 50, method = 'pmm', seed = 500)
 summary(impute_Data)

 summary(new_data) 
 
 fit=lm(total.revol_bal~.,data=loan_train) # we have created fit model and applied linear model on it.
 fit
 summary(fit)#
 fit$fitted.values
 confint(fit)
 pred <- predict(fit)
 mean(fit$residuals)
 sqrt(sum(fit$residuals^2)/nrow(loan_train))  #RMSE
 sqrt(mean(fit$residuals^2))
 confint(fit,level=0.95)
 predict(fit,interval="predict")
 vif(fit)
 plot(fit)
 sort(vif(fit),decreasing = T)
 
 #### PREDICTION ON TEST AND TRAIN DATA#########
 predict_loan_train <- predict(fit, newdata = loan_train)
 predict_loan_test <- predict(fit, newdata = loan_test)
 #nrow and length must be same
 length(predict_loan_test)
 nrow(loan_test)
 
 ########### MODEL EVALUATION ##############
 
 predict_loan_test
 predict_loan_train
 
 # the interest rate on both test and train data set on actual data
 loan_train$total.revol_bal
 loan_test$total.revol_bal
 
 
 
 ######################
 
 ## Added Variable plot to check correlation b/n variables and o/p variable
 fit1=lm(total.revol_bal~total_rec_int,data=loan_train) # we have created fit model and applied linear model on it.
 summary(fit1)# became significant
 fit2=lm(total.revol_bal~total_rec_late_fee,data=loan_train) # we have created fit model and applied linear model on it.
 summary(fit2)# became insignificant
 fit3=lm(total.revol_bal~total_rec_int + total_rec_late_fee,data=loan_train) # we have created fit model and applied linear model on it.
 summary(fit3)## Both became significant
 
 
 avPlots(fit,id.n=2,id.cex=0.7)
 
 
 #### PREDICTION ON TEST AND TRAIN DATA#########
 predict_loan_train <- predict(fit, newdata = loan_train)
 predict_loan_test <- predict(fit, newdata = loan_test)
 #nrow and length must be same
 length(predict_loan_test)
 nrow(loan_test)
 
 ########### MODEL EVALUATION ##############
 
 predict_loan_test
 predict_loan_train
 
 # the interest rate on both test and train data set on actual data
 loan_train$total.revol_bal
 loan_test$total.revol_bal
 
 
 
 panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
 {
   usr<- par("usr"); on.exit(par(usr))
   par(usr=c(0,1,0,1))
   r=(cor(x,y))
   txt<- format(c(r,0.123456789),digits=digits)[1]
   txt<- paste(prefix,txt,sep="")
   if(missing(cex.cor)) cex<-0.4/strwidth(txt)
   text(0.5,0.5,txt,cex=cex)
 }
 
 pairs(fit,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")
 
 # It is Better to delete influential observations rather than deleting entire column which is 
 # costliest process
 # Deletion Diagnostics for identifying influential observations
 
 influence.measures(fit)
 
 library(car)
 ## plotting Influential measures 
 windows()
 
 influenceIndexPlot(fit,id.n=3) # index plots for infuence measures
 influencePlot(fit,id.n=3) # A user friendly representation of the above
 
 # Regression after deleting the 573344 observation, which is influential observation
 model_1<-lm(total.revol_bal~.,-total_rec_int,data=loan_train[,573344])
 summary(model_1)
 
 # Regression after deleting the 573344th & 818820,409941 Observations
 model_2<-lm(total.revol_bal~.,-total_rec_int,data=loan_train[-c(573344,818820,409941),])
 summary(model_2)
 

 
 