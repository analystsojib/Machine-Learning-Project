### set working directory 

setwd("D:/Naimul Vai work/Bank Project")


### required library
library(tidyverse)
library(psych)
library(caret)
library(randomForest)





library(class)
library(mlbench)

library(dplyr)
library(MASS)
library(caret)
library(e1071)
library(ipred)
library(C50)
library(rpart)
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(randomForest)


dt<-read.csv("bank.csv",sep = ";")



head(dt)
View(dt)

#structure of the dataset
str(dt)

#summary of dataset
summary(dt)

#checking to see if NA 
dt  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>% 
  filter(missing=="FALSE") %>% 
  dplyr::select(variables,n) %>% 
  rename(Non.Missing=n)

#Bar Charts of Categorical Variables
t<-as.data.frame(table(dt$job))

### plot

ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Job")+
  coord_flip()


t<-as.data.frame(table(dt$marital))

###
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Marital Status")+
  coord_flip()

t<-as.data.frame(table(dt$education))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Education")+
  coord_flip()


t<-as.data.frame(table(dt$default))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Has credit in default?")+
  coord_flip()


t<-as.data.frame(table(dt$housing))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Has housing loan?")+
  coord_flip()

t<-as.data.frame(table(dt$loan))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Has personal loan?")+
  coord_flip()

t<-as.data.frame(table(dt$contact))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Contact Communication Type")+
  coord_flip()


t<-as.data.frame(table(dt$month))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Last contact month of Year")+
  coord_flip()


t<-as.data.frame(table(dt$poutcome))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Previous Marketing Campaign")+
  coord_flip()

t<-as.data.frame(table(dt$y))
ggplot(t,aes(x=reorder(Var1,Freq),y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Subscribed a Term Deposit?")+
  coord_flip()

#Scatterplot Matrix of Numeric Variables
pairs.panels(dt[,c(1,6,10,12:15)])


#Variable Recoding
#Previous Marketing Campaign
dt$poutcome<-ifelse(dt$poutcome=="other","unknown",dt$poutcome)


#Sampling

names(dt[,-c(1,6,10,12:15)])

dt[,-c(1,6,10,12:15)]<-lapply(dt[,-c(1,6,10,12:15)],as.factor)
dt1<-dt

dt1$Index<-c(1:4521)

set.seed(2022)
train <- dt1 %>%
  group_by(job,marital,education,default,housing,loan,contact,month,poutcome,y) %>%
  sample_frac(size=.70)


test<-dt[-train$Index,]

names(train)
names(test)

train<-train[,-18]




###



#Naive Bayes classifier ####

classifier_cl <- naiveBayes(y ~ ., data =train)


summary(classifier_cl)

# Predicting on train data'
y_pred <- predict(classifier_cl, newdata = test)


# Confusion Matrix
cm <- table(test$y, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)


#### Boosted C5.0#####


fit <- C5.0(y~., data=train, trials=10)
# summarize the fit
print(fit)
# make predictions
predictions <-  predict(fit,test)
# summarize accuracy
tab <- table(predictions, test$y)
confusionMatrix(tab)




####


# Bagging CART######


# load the package

# load data
# fit model
fit <- bagging(y~., data=train)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, test, type="class")
# summarize accuracy
tab <- table(predictions, test$y)
confusionMatrix(tab)


# Dt #########


model<- ctree(y ~ .,train)
plot(model)

# and those who are not
predict_model<-predict(model,test)

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test$y, predict_model)
m_at
confusionMatrix(m_at)



# LDA model#######



modelda <- lda(y~., data=train)
summary(modelda)


p <- predict(modelda,newdata = test)
p$class
tab <- table(test$y, p$class)


confusionMatrix(tab)



# Random Forest Model#######

# Create a Random Forest model with default parameters

RF <- randomForest(y ~ ., data = train, importance = TRUE)
RF

# Running on Validation Set
model_pred = predict(RF, newdata =test)


table(model_pred)
tab <- table(model_pred, test$y)

confusionMatrix(tab)


#### Cart ####


# load the package
library(rpart)
# fit model
fit <- rpart(y~., data=train)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, test, type="class")
# summarize accuracy
tab <- table(predictions, test$y)


confusionMatrix(tab)


### GBM####

library(caret)


fitControl <- trainControl(method = "cv",
                           number = 10)

tune_Grid <-  expand.grid(interaction.depth = 2,
                          n.trees = 500,
                          shrinkage = 0.1,
                          n.minobsinnode = 10)



set.seed(825)
fit <- train(y ~ ., data = train,
             method = "gbm",
             trControl = fitControl,
             verbose = FALSE)



predicted= predict(fit,test,type= "prob")[,2] 

pred <- ifelse(predicted>=0.5,"yes","no")
table(pred)

tab <- table(test$y,pred)
confusionMatrix(tab)







###Svn ####


library(e1071)

classifier = svm(formula = y ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test set results
y_pred = predict(classifier, newdata = test)
y_pred


tab <- table(y_pred, test$y)
confusionMatrix(tab)









### LR ####
library(MASS)

model = glm(formula=y ~ . , family = binomial(link='logit'),data = train) %>% stepAIC()
summary(model)

probabilities = predict(model,newdata=test,type='response')


prob <- ifelse(probabilities >= 0.5, "yes","no")
table(prob)
tab <- table(prob, test$y)
confusionMatrix(tab)







