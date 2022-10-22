# Set working directory


## load library

library(MASS)
library(ggplot2)
library(tidyverse)
library(caret)
library(Metrics)

url <- "https://storage.googleapis.com/kagglesdsdata/datasets/269522/560057/audit_data.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20221010%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20221010T042236Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=9d9efc4a7156a8be0120518b6fa8b33c852faf38dd3de07630f99b2583e0e71a87072bb2d958921b4e870c847ac5c9d0b077cb1a9bd961fee79d4489484ea5f3264d6fa4c6eb4aea550d7b341bfdfbe585fc9ac14298762567bf1927738e0801912a6efd91ac3ac207169068d996d6e0ae180e639fcf3a1b4d2a30d97599eebf707918248e1e648b8aa7172fea17ff8bc5c7a4ec0514e1bdb681a7c940d0d01cfcb0b7b269800871da19df8f6f915135d86ba9101bba69bf2abbb3e5085868845cae754f3d23377a47d6d27b6e87ebefeac98d9c178305c31ad021c83234f7e146878aff0fa8b35b8047c717ad7dc6c6777d357f2db7e8bc662a0fce263d7879"


Audit_data <- read.csv(url)

## 
Audit_data <- Audit_data %>% 
  dplyr::select(-c("LOCATION_ID","Detection_Risk"))


head(Audit_data)


table(Audit_data$numbers)#f
table(Audit_data$District_Loss)#f
table(Audit_data$History)#f
table(Audit_data$Score_B.1)#f
table(Audit_data$Risk_B)
table(Audit_data$Risk_A)
table(Audit_data$PARA_B)
table(Audit_data$Score_B)#f
table(Audit_data$Score_MV)# f
table(Audit_data$TOTAL)
table(Audit_data$Risk_C)#f
table(Audit_data$Money_Value)
table(Audit_data$Risk_D)
table(Audit_data$Prob)#f
table(Audit_data$PROB)#f
table(Audit_data$Risk_F)#f
table(Audit_data$Score)#f
table(Audit_data$Inherent_Risk)
table(Audit_data$CONTROL_RISK)#f
table(Audit_data$Audit_Risk)

summary(Audit_data$Score)
class(Audit_data$Score)



Audit_data$Risk_C


Audit_data$CONTROL_RISK_new <- ifelse(Audit_data$CONTROL_RISK==0.4,"0.4",">0.4")
table(Audit_data$CONTROL_RISK_new)
Audit_data$Risk_C_new <- ifelse(Audit_data$Risk_C==1,"1",">1")
table(Audit_data$Risk_C_new)
Audit_data$History_new <- ifelse(Audit_data$History==0,"0",">=1")
table(Audit_data$History_new)
table(Audit_data$Risk_F)
Audit_data$Risk_F_new <- ifelse(Audit_data$Risk_F==0,"0",">=1")
table(Audit_data$Risk_F_new)
table(Audit_data$numbers)
Audit_data$numbers_new <- ifelse(Audit_data$numbers <=5,"<=5",">5")
table(Audit_data$numbers_new)
Audit_data$Score_new <- ifelse(Audit_data$Score <3,"<3",">=3")

table(Audit_data$Score_new)
names(Audit_data)
summary(Audit_data)

vars <- c("numbers_new","District_Loss","History_new","Score_B.1","Score_B","Score_MV","Risk_C_new",
          "Prob","PROB","Risk_F_new","CONTROL_RISK_new", "Score_new","Risk")






Audit_data[ ,vars]<- lapply(Audit_data[ ,vars], as.factor)

str(Audit_data)




Audit_data2 <- Audit_data %>% 
  dplyr::select(Sector_score,PARA_A,Score_A,Risk_A,PARA_B, Score_B, Risk_B, TOTAL, numbers_new, Score_B.1, Risk_C_new,Money_Value,
         Score_MV, Risk_D,District_Loss, PROB, RiSk_E, History_new, Risk, Risk_F_new)


names(Audit_data2)


str(Audit_data2)




Audit_data2  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()



## Delete missing observation form missing column

Audit_data2 <- na.omit(Audit_data2)


## Let's see the plot for checking any missing value exists or not


Audit_data2  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()


## So, there have no missing value in the column variable.


view(Audit_data2)

numerical_variable <- Audit_data2 %>% 
  select_if(is.numeric)


view(numerical_variable)



### Correlation matrix shows that some independent variable have strongly correlated each other, which arises multicolinearity issue. SO, we remove for those variable from model building data.
## There are only three variable arises multicolinearity problem: TOTAL,PARA_B,RISK_B, So remove them from the dataset.




names(Audit_data2)
Audit_data2 <- Audit_data2 %>% 
  select(-c("TOTAL","PARA_B","Risk_B"))

names(Audit_data2)

##  Now our data fit for the model and further analysis

view(Audit_data2)




## Data partition



set.seed(555)
ind <- sample(2, nrow(Audit_data2),
              replace = TRUE,
              prob = c(0.9, 0.1))

training <- Audit_data2[ind==1,]
testing <- Audit_data2[ind==2,]



####

names(training)
#fit logistic regression model 
model_lr <- glm(Risk~., family="binomial", data=training) %>% stepAIC(trace = T)

###


pred <- predict(model_lr, testing, type = "response")

predicted_category <- ifelse(pred >0.5,1,0)

table(predicted_category)



accuracy(testing$Risk,predicted_category)

tab <- table(predicted_category, testing$Risk)

confusionMatrix(tab)


### decision tree

library(rpart.plot)
library(rpart)

fatalityModel <- rpart(Risk~., training, method = "class")



rpart.plot(fatalityModel, type = 0, extra = 0)

predicted_class <- predict(fatalityModel, testing, type = "class")
table(predicted_class)
tab <- table(predicted_class, testing$Risk)

confusionMatrix(predicted_class, reference = testing$Risk)

library(party)

library(pROC)
library(mlbench)
library(e1071)

classifier_cl <- naiveBayes(Risk ~ ., data =training)
summary(classifier_cl)

# Predicting on train data'
y_pred <- predict(classifier_cl, newdata = testing)



table(y_pred)


naive_tab <- table(testing$Risk, y_pred)
# Confusion Matrix
confusionMatrix(testing$Risk, y_pred)


### Accuracy Table for three Model

nive_acuracy <- accuracy(testing$Risk, y_pred);nive_acuracy
logistic_accuracy <-accuracy(testing$Risk, predicted_category);logistic_accuracy
DT_accuracy <- accuracy(testing$Risk, predicted_class);DT_accuracy



###








