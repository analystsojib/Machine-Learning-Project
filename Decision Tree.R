##### Set working directory ####

####Libraries####
library(rpart)
library(rpart.plot)
library(rattle)


####Data####
#load("Churn.Rdata")
#write.csv(data,"churn.csv")
data <- read.csv("churn.csv",stringsAsFactors = T)
str(data)
summary(data)
names(data)
#data$Churn<-as.factor(data$Churn)
table(data$Churn)
#Data Split
set.seed(2021)
ind <- sample(nrow(data),nrow(data)*0.70)
train <- data[ind,]
test <- data[-ind,]

#Base Model
d_model <- rpart(Churn~., data = train, method = "class",
                 control = rpart.control(cp = 0))
printcp(d_model)

#Minimum xerror=0.80374, CP=0.00623053

#Base Model Prediction and Accuracy
pred<- predict(d_model, test, type = "class")
base_accuracy <- mean(pred == test$Churn)
table(pred,test$Churn)

#Model Pruned
d_model_pruned <- prune(d_model, cp = 0.0046728972)
#Final Decision Tree
fancyRpartPlot(d_model_pruned)

#Pruned Model Prediction and Accuracy
pred1 <- predict(d_model_pruned, test, type = "class")
accuracy_pruned <- mean(pred1 == test$Churn)
table(pred1,test$Churn)

data.frame(base_accuracy, accuracy_pruned)

colnames(data$Churn)


