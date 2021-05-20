#Loading required packages
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('caret')
#install.packages('caretEnsemble')
#install.packages('psych')
#install.packages('Amelia')
#install.packages('mice')
#install.packages('GGally')
#install.packages('rpart')
#install.packages('randomForest')
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(modelr)
library(broom)
library(regclass)
library(e1071)


setwd("C:/Users/Remi/Documents/Semester 1/Data Mining and Machine Learning 1/Project/Dataset/")
credit = read.csv("creditcard.csv", header = T, na.strings = c(""), stringsAsFactors = T)
str(credit)

all_data = credit[,-1]

categorical = c('SEX','EDUCATION', 'MARRIAGE', 'default.payment.next.month',
                colnames(all_data)[grep("PAY_\\d", colnames(all_data))])

# Changing variables type
all_data[,categorical] = lapply(all_data[,categorical], function(var) as.factor(var))
str(all_data)

##########################################
# # Data pre-processing for visualization
##########################################
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}


#Creating the Training and Testing data set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(all_data), replace = T, prob = c(0.7,0.3))
train <- all_data[sample, ]
test <- all_data[!sample, ]

#create objects x which holds the predictor variables 
#and y which holds the response variables
x = train[,-24]
y = train$default.payment.next.month



###############################################################
#Naive Bayes model
###############################################################

#Model on training dataset
#model_nb_credit = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model_nb_credit = naiveBayes(default.payment.next.month~.,train)

#Predict of test dataset
#pr = predict(model_nb_credit, newdata = test)

pred_nb <- test %>%
  mutate(pred = predict(model_nb_credit, test, type = "class"))

cm_nb_credit <- confusionMatrix(table(pred_nb$pred, pred_nb$default.payment.next.month))
tibble("Accuracy" = cm_nb_credit$overall[[1]], "Sensitivity" = cm_nb_credit$byClass["Sensitivity"],
       "Specificity" = cm_nb_credit$byClass["Specificity"])
cm_nb_credit

###############################################################
#Logistic regression model
###############################################################
#Train logistic regresion model
model_lr_credit <- glm(default.payment.next.month ~., family = "binomial", data = train)
summary(model_lr_credit)

pred_lr <- test %>%
  mutate(pred = predict(model_lr_credit, newdata =  test))

pred_lr$pred <- ifelse(pred_lr$pred > 0.5,1,0)

cm_lr_credit <- confusionMatrix(table(pred_lr$pred, test$default.payment.next.month))
tibble("Accuracy" = cm_lr_credit$overall[[1]], "Sensitivity" = cm_lr_credit$byClass["Sensitivity"],
       "Specificity" = cm_lr_credit$byClass["Specificity"])
cm_lr_credit



##########################################################
# Compare The Result from Both Models
##########################################################

result  <- tibble("Model" = c("Naive Bayes", "Logistic Regression"),
                  "Accuracy" = c(cm_nb_credit$overall["Accuracy"], cm_lr_credit$overall['Accuracy']),
                  "Sensitivity" = c(cm_nb_credit$byClass["Sensitivity"], cm_lr_credit$byClass['Sensitivity']),
                  "Specificity" = c(cm_nb_credit$byClass["Specificity"], cm_lr_credit$byClass['Specificity'])) %>%
  pivot_longer(2:4, names_to = "type", values_to = "value")

fig(20, 8)
result %>% ggplot(aes(type, value, fill = factor(Model, levels = c("Naive Bayes", "Logistic Regression")))) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "Model") +
  labs(x = "Performance",
       y = NULL,
       title = "Comparison of Model Performance on Credit Default")



