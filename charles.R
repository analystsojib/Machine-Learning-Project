##### Set working directory #####

setwd("D:/Naimul Vai work/Charlesbook project")


### required library




library(caret)




### Load the data set


data <- read.csv("CharlesBookClub.csv")




#For checking the dimensions of our training data frame and validation data frame, we can use these:




# Preprocessing & Training


anyNA(data)



##$# 

summary(data)


### structure


str(data)

## change the data type

data[,-c(2,3,4,5)]<-lapply(data[,-c(2,3,4,5)],as.factor)




####







### Data Slicing


set.seed(1)
intrain <- createDataPartition(y = data$Florence, p= 0.6, list = FALSE)
training <- data[intrain,]
validation <- data[-intrain,]


 
 
 

# Training the Knn model
 
 trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
 
 set.seed(1)
 
 
 
 knn_fit <- train(Florence ~., data = training, method = "knn",
                  trControl=trctrl,
                  preProcess = c("center", "scale"),
                  tuneLength = 10)
 
 
 
 
 
 
 # Trained Knn model result
 

knn_fit

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was k = 11.
 
 
# Test Set Prediction

 test_pred <- predict(knn_fit, newdata = validation)
 test_pred

 
 
#  How Accurately our model is working?
   
 
knn_cm <-  confusionMatrix(test_pred, validation$Florence )


knn_cm



accuracy(test_pred,validation$Florence)

### DT
 
 
 # fit the model
 
 cl_tree_fit <- train(Florence ~., data = training, 
                  method = "rpart",
                  trControl = trainControl(method = "cv"))
 
 
 cl_tree_fit 
 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.0007037298.
 

 # Test Set Prediction
 
 
 dt_test_pred <- predict(cl_tree_fit, newdata = validation)
 dt_test_pred
 
 
 #  How Accurately our model 

 
classification_tree <-  confusionMatrix(dt_test_pred, validation$Florence )
 
classification_tree

accuracy(dt_test_pred,validation$Florence)




### LR ####





default_glm_mod = train(
  form = Florence ~ .,
  data =training,
  method = "glm",
  family = "binomial"
)


default_glm_mod
default_glm_mod$results
default_glm_mod$finalModel



pred_glm <- predict(default_glm_mod, newdata = validation)



logistic_cm <- confusionMatrix(pred_glm, validation$Florence)


logistic_cm

accuracy(pred_glm,validation$Florence)

# Random Forest Model#######


###

library(randomForest)

RF <- randomForest(Florence ~ ., data = training, importance = TRUE)
RF

# Running on Validation Set
model_pred = predict(RF, newdata =validation)


random_cm <- confusionMatrix(model_pred,validation$Florence)

random_cm

accuracy(model_pred,validation$Florence)

### Accuracy comparison


accuracy(model_pred,validation$Florence)

accuracy(pred_glm,validation$Florence)

accuracy(dt_test_pred,validation$Florence)


accuracy(test_pred,validation$Florence)





# From the result, Based on the accuracy we can claim that the random forest, Logistic regression, KNN, are among gives same accuracy 0.9155722.


