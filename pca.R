### Set working directory
setwd("D:/Naimul Vai work/Audit Risk Project")
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



### Visualizations ####


##BARplot
###ScoreB1

library(dplyr)


Score_B.1 <- Audit_data2%>%group_by(Score_B.1)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=Score_B.1, y=per, fill=Score_B.1))+
  geom_bar(stat="identity", width=1)+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Score_B.1")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")


###ScoreB
Score_B <- Audit_data2%>%group_by(Score_B)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=Score_B, y=per, fill=Score_B))+
  geom_bar(stat="identity", width=1)+coord_polar()+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Score_B")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")

###Score_MV
Score_MV <- Audit_data2%>%group_by(Score_MV)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=Score_MV, y=per, fill=Score_MV))+
  geom_bar(stat="identity", width=1)+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Score_MV")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")

####District_loss
District_loss <- Audit_data2%>%group_by(District_Loss)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=District_Loss, y=per, fill=District_Loss))+
  geom_bar(stat="identity", width=1)+coord_polar()+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "District_loss")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")

####PROB
PROB <- Audit_data2%>%group_by(PROB)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=PROB, y=per, fill=PROB))+
  geom_bar(stat="identity", width=1)+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "PROB")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")


####Risk
Risk <- Audit_data2%>%group_by(Risk)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=Risk, y=per, fill=Risk))+
  geom_bar(stat="identity", width=1)+coord_polar()+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Risk")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")




###History_new
History_new <- Audit_data2%>%group_by(History_new)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=History_new, y=per, fill=History_new))+
  geom_bar(stat="identity", width=1)+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "History")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")





###Risk_C_New
Risk_C <- Audit_data2%>%group_by(Risk_C_new)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=Risk_C_new, y=per, fill=Risk_C_new))+
  geom_bar(stat="identity", width=1)+coord_polar()+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Risk_C")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")

##Risk_F_New
Risk_F <- Audit_data2%>%group_by(Risk_F_new)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=Risk_F_new, y=per, fill=Risk_F_new))+
  geom_bar(stat="identity", width=1)+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Risk_F")+theme(plot.title = element_text(hjust=0.5),legend.position = "None")

###numbers_new
numbers <- Audit_data2%>%group_by(numbers_new)%>%
  summarise(count = n()) %>%
  mutate(per = round(count/sum(count),2)*100)%>%
  ggplot(aes(x=numbers_new, y=per, fill=numbers_new))+
  geom_bar(stat="identity", width=1)+coord_polar()+
  theme_void()+
  geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust=0.5))+
  labs(title = "Number")+theme(plot.title = element_text(hjust=0.5), legend.position = "None")

library(ggpubr)


### Visualization
ggarrange(Score_B.1,Score_B, Score_MV,District_loss,PROB,Risk,History_new,Risk_C,Risk_F,numbers,legend = NULL,ncol = 5, nrow = 2)




### Scatter Plot & Correlations
library(psych)


# First will check the correlation between independent variables. Let’s remove the factor variable from the dataset for correlation data analysis.


pairs.panels(Audit_data2[,c(1,2,3,4,5,7,8,12,14,17)])

#Lower triangles provide scatter plots and upper triangles provide correlation values.

#This leads to multicollinearity issues. So if we predict the model based on this dataset may be erroneous.

#One way handling these kinds of issues is based on PCA.



#Principal Component Analysis
#Principal Component Analysis is based on only independent variables. So we removed the fifth variable from the dataset.

pc <- prcomp(Audit_data2[,c(1,2,3,4,5,7,8,12,14,17)],
             center = TRUE,
             scale. = TRUE)


attributes(pc)


#Scale function is used for normalization



#Print the results stored in pc.


#while printing pc you will get standard deviations and loadings.

print(pc)
#For example, PC1 increases when Sector_score is increased and it is positively correlated 
#whereas PC1 increases PARA_A,Score_A,Risk_A,PARA_B,Risk_B,TOTAL,Money_Value,Risk_D,RiSk_E  decrease because these values are negatively correlated.

summary(pc)




#Orthogonality of PCs


# Let’s create the scatterplot based on PC and see the multicollinearity issue is addressed or not?.
library(psych)


pairs.panels(pc$x)


# Now the correlation coefficients are zero, so we can get rid of multicollinearity issues.



#$## 

library(factoextra)


options(scipen=999)

eigen_values <- get_eigenvalue(pc)

pc_comp<- pc$x


pc_comp <- pc_comp[,c(1,2,3)]








###


Pca_data <- data.frame(pc_comp, Audit_data2[,-c(1,2,3,4,5,7,8,12,14,17)])

head(Pca_data)
###
## Data partition




set.seed(555)
ind <- sample(2, nrow(Pca_data),
              replace = TRUE,
              prob = c(0.9, 0.1))

training <- Pca_data[ind==1,]
testing <- Pca_data[ind==2,]


####

names(training)
names(testing)


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
accuracy(predicted_class,testing$Risk)
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





library(randomForest)

rf <- randomForest(Risk~., data=training, proximity=TRUE) 

print(rf)


p1 <- predict(rf, testing)

confusionMatrix(p1, testing$Risk)

### Accuracy Table for three Model

nive_acuracy <- accuracy(testing$Risk, y_pred);nive_acuracy
logistic_accuracy <-accuracy(testing$Risk, predicted_category);logistic_accuracy
DT_accuracy <- accuracy(testing$Risk, predicted_class);DT_accuracy

RF_category <- accuracy(p1, testing$Risk);RF_category




