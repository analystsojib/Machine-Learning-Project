# set working directory

setwd("D:/Naimul Vai work/Indian salary project")
# Required library

library(openxlsx)
library(dplyr)
library(stringr)
library("tidyr")

### Data Load
train <- read.xlsx("Job seekers 913384 records.xlsx")
dim(train)

### Data Cleaning for train_data_LR data set

str(train)

train$DOB <- as.Date(as.integer(train$DOB), origin="1899-12-30")



# age from dob
train$age <- round(as.numeric(difftime(Sys.Date(),train$DOB, units = "weeks"))/52.25,0)
train$age

train <- train %>%
  separate(Salary, c("Currency", "Salary", "lacs"), " ")


## Filter all the dataset by Rs


train_Rs <- train %>% 
  filter(Currency=="Rs." & lacs=="lacs")

table(train_Rs$Currency)
table(train_Rs$lacs)


table(train_Rs$City)

train_Rs$City = tolower(train_Rs$City)


table(train_Rs$City)
   



         

train_Rs_city <- train_Rs %>% 
  filter(City=="bengaluru/ bangalore"|City=="bangalore"|City=="ahmedabad"|City=="chennai"|City=="delhi"|City=="gurgaon"|City=="faridabad"|City=="kolkata"|City=="mumbai"|City=="hyderabad"|City=="jaipur"|City=="surat"|City=="kanpur"|City=="greater noida")


table(train_Rs_city$City)



location <- ifelse(str_detect(train_Rs_city$Preferred_Location, 'Anywhere in India'),"Anywhere in India","Specific Location")

train_Rs_city$Location_preferred <-location 

table(train_Rs_city$Location_preferred)

### dependent variable for logistic regression
train_Rs_city$Location_preferred_num <- ifelse(train_Rs_city$Location_preferred=="Anywhere in India",0,1)
table(train_Rs_city$Location_preferred_num)

## Dependent variable for numerical predictive model

table(train_Rs_city$Salary)
summary(train_Rs_city$Salary)
train_Rs_city$Salary <-as.numeric(train_Rs_city$Salary)
summary(train_Rs_city$Salary)
# age creation
# usual variable model multiple lm
# reason for removing unsual variable
str(train_Rs_city)


train_Rs_city <- train_Rs_city %>%
  separate(Work_Experience, "Work_Experience", " ")










table(train_Rs_city$Work_Experience)

train_Rs_city$Work_Experience <- as.numeric(train_Rs_city$Work_Experience)






str(train_Rs_city)
### selected variable
  
  
  
names(train_Rs_city)



train_Rs_city <- train_Rs_city %>% 
  mutate_if(is.character, as.factor)


str(train_Rs_city)
train_data <- train_Rs_city %>%
  dplyr::select(City, age ,Salary,Work_Experience, Level,  Location_preferred)



str(train_data)

head(train_data)



clean_data <-train_data 

write.xlsx(clean_data,"Job Seekers 913384 records multiple clean data.xlsx")



train_data <- clean_data 
#######

### Data cleaning for testing
test <- read.xlsx("test.xlsx")


str(test)

test$DOB <- as.Date(as.integer(test$DOB), origin="1899-12-30")




# age from dob
test$age <- round(as.numeric(difftime(Sys.Date(),test$DOB, units = "weeks"))/52.25,0)
test$age

test <- test %>%
  separate(Salary, c("Currency", "Salary", "lacs"), " ")


## Filter all the dataset by Rs


test_Rs <- test %>% 
  filter(Currency=="Rs."& lacs=="lacs")

table(test_Rs$Currency)

tail(test_Rs)


table(test_Rs$City)

test_Rs$City <-  tolower(test_Rs$City)


table(test_Rs$City)






test_Rs_city <- test_Rs %>% 
  filter(City=="bengaluru/ bangalore"|City=="bangalore"|City=="ahmedabad"|City=="chennai"|City=="delhi"|City=="gurgaon"|City=="faridabad"|City=="kolkata"|City=="mumbai"|City=="hyderabad"|City=="jaipur"|City=="surat"|City=="kanpur"|City=="greater noida")









location <- ifelse(str_detect(test_Rs_city$Preferred_Location, 'Anywhere in India'),"Anywhere in India","Specific Location")

test_Rs_city$Location_preferred <-location 

table(test_Rs_city$Location_preferred)

### dependent variable for logistic regression
test_Rs_city$Location_preferred_num <- ifelse(test_Rs_city$Location_preferred=="Anywhere in India",0,1)
table(test_Rs_city$Location_preferred_num)

## Dependent variable for numerical predictive model

table(test_Rs_city$Salary)
summary(test_Rs_city$Salary)
test_Rs_city$Salary <-as.numeric(test_Rs_city$Salary)
summary(test_Rs_city$Salary)
# age creation
# usual variable model multiple lm
# reason for removing unsual variable
str(test_Rs_city)


test_Rs_city <- test_Rs_city %>%
  separate(Work_Experience, "Work_Experience", " ")

table(test_Rs_city$Work_Experience)

test_Rs_city$Work_Experience <- as.numeric(test_Rs_city$Work_Experience)



### selected variable



names(test_Rs_city)



test_Rs_city <- test_Rs_city %>% 
  mutate_if(is.character, as.factor)


str(test_Rs_city)
test_data <- test_Rs_city %>%
  dplyr::select(City, age ,Salary,Work_Experience, Level,  Location_preferred)



str(test_data)

head(test_data)

#### Multiple linear model



MLM_model_84 <- lm(Salary~., data = train_data) 
summary(MLM_model_84)




MLM_model_84_2 <- lm(Salary~age, data = train_data) 
summary(MLM_model_84_2)




require(broom)    
c1 <- tidy(MLM_model_84)



is.data.frame(c1)


library(flextable)

c1$p.value <- round(c1$p.value,2)
c1$estimate <- round(c1$estimate,2)
c1$std.error <- round(c1$std.error,2)
c1$statistic <- round(c1$statistic,2)
flextable(c1)

write.xlsx(c1,"c1.xlsx")
print(c1, preview="c1.dc1ox")
par(mfrow=c(2,2))
plot(MLM_model_84_2,1)
plot(MLM_model_84_2,2)
plot(MLM_model_84_2,3)
plot(MLM_model_84_2,5)

## Prediction

pred <- predict(MLM_model_84_2, newdata = test_data)

predct_actual <- cbind(actual=test_data$Salary, pred)
head(predct_actual)

#MSE(pred = pred, test_data$Salary)


R2 = cor(test_data$Salary, pred)^2
MSE = mean((test_data$Salary - pred)^2)
RMSE = sqrt(MSE)
MAE = mean(abs(test_data$Salary - pred))
R2
MSE
RMSE
MAE


### Prepossessing for logistic model


names(train_Rs_city)


#
train_data_LR <-  train_Rs_city %>%
  dplyr::select(City, age ,Salary,Work_Experience, Level,  Location_preferred_num)



str(train_data_LR)
table(train_data_LR$Location_preferred_num)

train_data_LR$Location_preferred_num <- as.factor(train_data_LR$Location_preferred_num)

class(train_data_LR$Location_preferred_num)


dim(train_data_LR)
### Prepossessing for logistic model

table(train_data_LR$Location_preferred_num)



#
test_data_LR <-  test_Rs_city %>%
  dplyr::select(City, age ,Salary,Work_Experience, Level,  Location_preferred_num)



str(test_data_LR)
table(test_data_LR$Location_preferred_num)

test_data_LR$Location_preferred_num <- as.factor(test_data_LR$Location_preferred_num)

class(test_data_LR$Location_preferred_num)




#####




#Fixing Imbalance


table(train_data_LR$Location_preferred_num)
class(train_data_LR$Location_preferred_num)
dim(train_data_LR)
class(train_data_LR)


### oversampling 



####


#colSums(is.na(train_data_LR))
#train_data_LR$Location_preferred_num[is.na(train_data_LR$Location_preferred_num)] <- 0

table(train_data_LR$Location_preferred_num)
#n0<-sum(train_data_LR$Location_preferred_num==0,na.rm = T)
#n1<-sum(train_data_LR$Location_preferred_num==1, na.rm = T)



n0 <- train_data_LR %>% 
  dplyr::filter(Location_preferred_num==0)

n1 <- train_data_LR %>% 
  dplyr::filter(Location_preferred_num==1)


dim(n1)
dim(n0)



c <- rep(c(1:635),times=16.08504)
d <- c(1:54)
length(c)
length(d)
x <- c(c,d)
length(x)

n01<- n0[x,] 

train_data_LR_v2 <- rbind(n01,n1)
class(train_data_LR_v2)
table(train_data_LR_v2$Location_preferred_num)
dim(train_data_LR_v2)


write.xlsx(train_data_LR_v2,"Job Seekers 913384 records logistic clean data.xlsx")

#library(tidymodels) # for bootstrapping

#library(broom)
#d <- bootstraps(n0,times=139900,apparent=T)




library(MASS)
LR_model <- glm(Location_preferred_num ~.,family=binomial(link='logit'),data=train_data_LR_v2) %>% stepAIC(trace = T)
summary(LR_model)


d2 <- tidy(LR_model)

write.csv(d2, "d2.csv")

fitted.results <- predict(LR_model,newdata=test_data_LR,type='response')

hist(fitted.results)

fitted.results <- ifelse(fitted.results > 0.5,1,0)
tab <- table(fitted.results, actual=test_data_LR$Location_preferred_num)


library(caret)
confusionMatrix(tab)












