# Linear-Regression-Model-Interest-Rate-#


# Data Preperation #


# ------ We are taking real life example for these purpose, I have used CSV file for these purpose.

data <- read.csv(file.choose())
View(data)
loan=read.csv("data.csv",stringsAsFactors = FALSE)
library(dplyr)
library(mice)
library(VIM)
library(dplyr)
library(Hmisc)
library(caret)
library(partykit)
library(randomForest)
#install.packages("VIF")
library(VIF)
class(data)
dim(data)
str(data)
summary(data)
getwd()
glimpse(data)

attach(data)
data = data %>% select ( loan_amnt , Rate_of_intrst , recoveries , State , annual_inc , purpose , debt_income_ratio , total.revol_bal , member_id , inq_last_6mths )

# Data Cleaning and Manipulation #

data=data%>%
  mutate(Rate_of_intrst=as.numeric(gsub("%","",Rate_of_intrst)),
         debt_income_ratio=as.numeric(gsub("%","",debt_income_ratio)),
         loan_amnt=as.numeric(loan_amnt),
         recoveries=as.numeric(recoveries),
         total.revol_bal=as.numeric(total.revol_bal)) #---- we have to convert data to numeric if it is Character.

glimpse(data)

table(data$loan_amnt)

data=data %>%
  mutate(el=ifelse(substr(loan_amnt,1,2)=="10",10,loan_amnt),
         el= ifelse(substr(loan_amnt,1,1)=="<",0,el),
         el=gsub("years","",el),
         el=gsub("year","",el),
         el=as.numeric(el))%>%
  select(-loan_amnt)%>%
  na.omit() #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same and we are omitting Na if they are present.

table(data$el)
glimpse(data)

table(data$recoveries)

data=data%>%
  mutate(Hw_Rent=as.numeric(recoveries=="RENT"),                
         Hw_Mort=as.numeric(recoveries=="MORTGAGE")) %>%
  select(-recoveries)
glimpse(data)  #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.


data=data%>%
  mutate(f1=as.numeric(substr(annual_inc,1,3)),
         f2=as.numeric(substr(annual_inc,5,7)),
         fico=0.5*(f1+f2))%>%
  select(-annual_inc,-f1,-f2) #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.


table(data$purpose)
sort(round(tapply(data$Rate_of_intrst,data$purpose,mean),0))

data=data %>% 
  mutate(lp_14=as.numeric(purpose %in% c("debt_consolidation","moving")),
         lp_13=as.numeric(purpose %in% c("credit_card","other","small_business",'house')),
         lp_12=as.numeric(purpose %in% c("home_improvement","vacation","wedding","medical")))
data=data %>%select(-purpose)  #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.


View(data)

table(data$State)
length(unique(data$State))
table(data$terms)

data=data%>%
  mutate(LL_36=as.numeric(terms=="36 months"))%>%
  select(-terms)  #---- we have to convert data to numeric if it is Character and delete the old column as we have created dummy columns for same.

glimpse(data) 

data=data%>%
  select(-State) #---delete the old column because we dont need these column as they are not useful for future.

#returns the number of columns of a matrix or data frame
ncol(data)
View(data)



#  Data Modelling #


#------Create Test and Train Data--------- #

set.seed(2)
s=sample(1:nrow(data),0.7*nrow(data)) # we have 70 % of data in Train and Test we have 30%.
loan_train=data[s,]
loan_test=data[-s,]
str(loan_test)

memory.size(max = TRUE)
memory.limit(size = 10000)


library(car)
fit=lm(total.revol_bal~. -Rate_of_intrst,data=loan_train) # we have created fit model and applied linear model on it.
fit
#vif(fit)
#sort(vif(fit),decreasing = T)
summary(fit)


fit=lm(total.revol_bal~. -Rate_of_intrst -debt_income_ratio -inq_last_6mths,data=loan_train) # Removing data which does not effect too much on interest rate.
sort(vif(fit),decreasing = T)

summary(fit)

fit=lm(Rate_of_intrst~. -member_id -loan_amnt -recoveries- debt_income_ratio -total.revol_bal -el
       -lp_12-Hw_Rent-Hw_Mort,data=loan_train)  # Removing data which does not effect too much on interest rate.

summary(fit)


# Data Prediction #

#------Training-------#

loan_train$predict=predict(fit,newdata = loan_train,type = "response")
View(loan_train)

rmse_train=sqrt(mean((loan_train$Rate_of_intrst-loan_train$predict)^2))
rmse_train

#----Testing----------#

loan_test$predict=predict(fit,newdata = loan_test,type = "response")
View(loan_test)
head(loan_test,5)

rmse_test=sqrt(mean((loan_test$Rate_of_intrst-loan_test$predict)^2))
rmse_test


# Asssumptions #

plot(fit,which=1) #independent
plot(fit,which=2) #normal distribution
plot(fit,which=3) #constant variance
plot(fit,which=4) #cooks distance


##RANDOM FOREST AND DECISION TREE

set.seed(300)
in_data_train <- sample(nrow(data), nrow(data)*0.75)
data_train <- data[in_data_train, ]
data_test <- data[-in_data_train, ]
##Check the proportion of the train set and test set, make sure they have (almost) the same proportion.

prop.table(table(data_train$total.revol_bal))

prop.table(table(data_test$total.revol_bal))

##Using Decision Tree

data_model_dt <- ctree(total.revol_bal ~ ., data_train)

##Plotting our decision tree of our credit risk analysis
data = data %>% select ( loan_amnt , Rate_of_intrst , recoveries , State , annual_inc , purpose , debt_income_ratio , total.revol_bal , member_id , inq_last_6mths )

plot(data_model_dt)



plot(data_model_dt, type = "simple")


data_model_dt

#Use the model to make prediction
data_pred_dt <- predict(data_model_dt, data_test)
#Accuracy, Precision, and Recall
#Confusion table for the prediction and test set

(dt_conft <- table("prediction" = data_pred_dt,
                   "actual" = data_test$total.revol_bal))
##           actual
## prediction  no yes
##        no  158  50
##        yes  16  26
accu_dt <- round((dt_conft[1]+dt_conft[4])/sum(dt_conft[1:4]),4)
prec_dt <- round(dt_conft[4]/(dt_conft[2]+dt_conft[4]), 4)
reca_dt <- round(dt_conft[4]/(dt_conft[4]+dt_conft[3]), 4)
spec_dt <- round(dt_conft[1]/(dt_conft[1]+dt_conft[2]), 4)
paste("Accuracy:", accu_dt*100,"%")

paste("Precision:", prec_dt*100,"%")

paste("Recall:", reca_dt*100,"%")

paste("Specitifity:", spec_dt*100,"%")
## [1] "Specitifity: 90.8 %"
#Or we can use confusionMatrix() to find out the Accuracy, Recall, Presicion, Specitifity, etc of our model.

confusionMatrix(data_pred_dt, data_test$default, positive = "yes")


loans_model_dt2 <- ctree(default ~ ., loans_train, control = ctree_control(mincriterion = 0.7))

plot(loans_model_dt2)


loans_model_dt2
#From this model, we have 11 inner nodes and 12 terminal nodes. (More than our previous model!)

#Use the model to make prediction

loans_pred_dt2 <- predict(loans_model_dt2, loans_test)

#Accuracy, Precision, and Recall

confusionMatrix(loans_pred_dt2, loans_test$default, positive = "yes")

paste("Accuracy:", round((624+132)/(1000)*100, 2),"%")
## [1] "Accuracy: 75.6 %"
paste("Precision:", round((132)/(132+168)*100, 2),"%")
## [1] "Precision: 44 %"
paste("Recall:", round((132)/(132+76)*100, 2),"%")
## [1] "Recall: 63.46 %"
paste("Specitifity:", round((624)/(624+168)*100, 2), "%")
## [1] "Specitifity: 78.79 %"
##These are the variables that give high significance to our random forest loan model.

varImp(loans_rf)

plot(loans_rf$finalModel)

legend("topright", colnames(loans_rf$finalModel$err.rate),col = 1:6, cex = 0.8, fill = 1:6)


plot(loans_rf)


#From here, we know that we can get the highest Accuracy with only 18 variables, but Recall rate is the parameter that we have to improve in our model.

#From random forest model, we get Recall rate 63.46% but our Precision drops to 44%.

