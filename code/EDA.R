install.packages("shiny")
runApp("~/shinyapp")
# check working directory
library(shiny)
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
names(loan)  # names of the variables 
dim(loan)  # dimension (number of rows= 887379 and columns= 36)
str(loan)  # structure of the data set
summary(loan)
nrow(loan)
ncol(loan)
any(is.na(loan)) # check for missing values
sapply(loan, function(x){sum(is.na(x))}) ## checking columns which have missing values
range(loan)

########## Feature Selection & Engineering ##########

# Select only require columns
attach(loan)
loan_data = loan %>% select ( member_id, loan_amnt, terms, Rate_of_intrst,grade,
                              Experience, home_ownership, annual_inc, verification_status,
                              debt_income_ratio ,total.revol_bal)
loan_data  # 887379 obs. of 11 variables

set.seed(123) #locks seed for random partitioning

#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=loan_data$terms, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-loan_data[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-loan_data[-inTrain,]  

########## Missing Values ##########

sum(complete.cases(loan_data))  #used to identify complete rows of a data frame i.e. rows without NA -887375
sum(!complete.cases(loan_data)) #4

na_count <-sapply(loan_data, function(y) sum((is.na(y)))) #checking columns which have missing values
na_count <- data.frame(na_count) #creating data frame

########## Removing rows ##########

loan1 = loan_data %>%
  filter(!is.na(annual_inc) , 
         !(home_ownership %in% c('NONE' , 'ANY')) , 
         Experience != 'n/a')
loan1    #omitted 842412 rows  
#dim(loan1)

loan1$grade <- as.factor(loan1$grade)
loan1$terms <- as.factor(loan1$terms)
loan1$home_ownership <- as.factor(loan1$home_ownership)
loan1$Rate_of_intrst <- as.factor(loan1$Rate_of_intrst)

loan1$grade          #Levels: A B C D E F G
loan1$terms          #Levels:36 months 60 months
loan1$home_ownership #Levels:MORTGAGE OTHER OWN RENT
loan1$Rate_of_intrst #Levels: 5.32 5.42 5.79 5.93 5.99 6 6.03 6.17 6.24 6.39 6.49 6.54 6.62 6.68 6.76 ... 28.99

########## Exploratory Data Analysis loan data ##########

#Distribution of loan amounts

loan1 <- ggplot(data=loan, aes(loan$loan_amnt)) + 
          geom_histogram(breaks=seq(0, 35000, by=1000), 
                 col="black", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="aliceblue", high="royalblue3")+
  labs(title="Loan Amount", x="Amount", y="Number of Loans")
loan1  #The majority of loans is lower than 20000, the highest frequency is around the 10000 mark.

#Total loan amount based on loan grade

ggplot(loan, aes(x=grade, y=loan_amnt, fill=grade)) +
  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total Loan Amount",title="Total loan amount based on loan grade") 
 #This graph indicates that typically grades of B and C tend to have a higher loan amount.

# grade depends on Rate_of _interest 

ggplot(data=loan, aes(grade,Rate_of_intrst,fill=grade))+geom_boxplot(outlier.color = "black")+labs(title="Box plot of Interest rate")
# This graph indicates that as the grade becomes less the interest rate increases which would make sense as these loans would most likely be more risky.

# distribution of loan amount by grade

ggplot(data=loan,aes(loan_amnt, fill=grade))+
  geom_density(alpha=0.25) + 
  facet_grid(grade ~ .)
#seems that across all the grades for the most part the loan amounts are consistent across grades However it does appear that some of the lower grades have more loans of $30,000+

ggplot(loan[sample(842502,1000 ) , ] , aes(x = annual_inc , y = loan_amnt , color = Rate_of_intrst)) +
  geom_point(alpha = 0.5 , size = 1.00) + 
  geom_smooth(se = F , color = 'darkred' , method = 'loess') +
  xlim(c(0 , 100000)) + 
  labs(x = 'Annual Income' , y = 'Loan Ammount' , color = 'Interest Rate')
# the larger the annual income the larger the demanded amount by the borrower.

#Loan Amount Summary
summary(loan$loan_amnt) #The median loan amount is 13000 and mean is 14755.

ggplot(loan, aes(x=loan_amnt)) + 
geom_density() + 
theme(legend.position="none") +
xlab("Loan Amount") + 
ggtitle("Loan Amount") + 
geom_vline(aes(xintercept=mean(loan_amnt)), color="black", linetype="dashed", size=1) +
geom_vline(aes(xintercept=median(loan_amnt)), color="red", linetype="dashed", size=1) + 
geom_vline(aes(xintercept=quantile(loan_amnt, 0.25)), color="blue", linetype="dashed", size=1) +
geom_vline(aes(xintercept=quantile(loan_amnt, 0.75)), color="yellow", linetype="dashed", size=1)

 #Interest Rate Summary
summary(loan$Rate_of_intrst) #The median interest rate is 12.99 and mean is 13.25.

ggplot(loan, aes(x=Rate_of_intrst)) + 
geom_density() + 
theme(legend.position="none") +
xlab("Interest Rate") + 
ggtitle("Interest Rate") + 
geom_vline(aes(xintercept=mean(Rate_of_intrst)), color="black", linetype="dashed", size=1) +
geom_vline(aes(xintercept=median(Rate_of_intrst)), color="red", linetype="dashed", size=1) +
geom_vline(aes(xintercept=quantile(Rate_of_intrst, 0.25)), color="blue", linetype="dashed", size=1) +
geom_vline(aes(xintercept=quantile(Rate_of_intrst, 0.75)), color="yellow", linetype="dashed", size=1)

########## Exploratory Data Analysis for Borrowers ##########

library(RColorBrewer)

#Borrowers Home Ownership Type
loan_data %>%
group_by(home_ownership) %>%
summarize(freq = n()) %>%
ggplot(aes(reorder(home_ownership, freq), y = freq, fill = freq)) +   
geom_bar(stat = "identity", position = "dodge") +
xlab("home_ownership") +
ylab("Frequency") +
theme_fivethirtyeight() + 
theme(legend.position ='none', axis.text.x = element_text(size = 15)) + 
geom_text(aes(label = freq), vjust = -0.1, size = 4.5) +
scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
ggtitle("Home Ownership")
#In total, there are six type of home ownership types. Most borrowers have mortgages 443557 and 356117 borrowers rent house/apartment.

#Borrowers Experience
loan_data %>%
group_by(Experience) %>%
summarize(freq = n()) %>%
ggplot(aes(reorder(Experience, freq), y = freq, fill = freq)) +   
geom_bar(stat = "identity", position = "dodge") +
xlab("Experience") +
ylab("Frequency") +
coord_flip() +
theme_fivethirtyeight() + 
theme(legend.position ='none', axis.text.y = element_text(size = 12)) + 
scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
ggtitle("Experience")
# Most borrowers have 10-year+ employment length.

#Borrowers' annual income

# set up boundaries for intervals/bins
breaks <- c(0,20000,40000,60000,80000,90000,100000,150000,300000, 9500000)
# specify interval/bin labels
labels <- c("<20000", "20000-40000)", "40000-60000)", "60000-80000)", "80000-90000)", "90000-100000)", "100000-150000)", "150000-300000)", "<=9500000")
# bucketing data points into bins
bins <- cut(loan$annual_inc, breaks, include.lowest = T, right=FALSE, labels=labels)
# inspect bins
bin <- summary(bins)
freq_inc <- c(10204, 129409, 241593, 203729, 7258i, 54965, 123426, 45865, 5603)
annual_income <- data.frame(labels, freq_inc)
plot(bins, main="Annual Income", ylab="Annual Income")

########## correlation Matrix ##########

num_vars <- 
  train %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()

library(funModeling)
meta_train <- funModeling::df_status(Training, print_results = FALSE)

meta_train %>%
select(variable, p_zeros, p_na, unique) %>%
select_(~variable %in% num_vars) %>%
knitr::kable()
 
library(corrplot)
corrplot::corrplot(cor(Training[, num_vars],use = "pairwise.complete.obs"), 
                           method = "pie", type = "lower")








