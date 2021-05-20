#loading Packages
install.packages("tidyverse")
install.packages("regclass")
library(tidyverse)
library(modelr)
library(broom)
library(Amelia)
library(regclass)
library(caret)


setwd("C:/Users/Remi/Documents/Semester 1/Data Mining and Machine Learning 1/Project/Dataset/")
credit = read.csv("creditcard.csv", header = T, na.strings = c(""), stringsAsFactors = T)

str(credit)

#remove id column from dataset
all_data = credit[,-1]

categorical = c('SEX','EDUCATION', 'MARRIAGE', 'default.payment.next.month',
                colnames(all_data)[grep("PAY_\\d", colnames(all_data))])

# Changing variables type
all_data[,categorical] = lapply(all_data[,categorical], function(var) as.factor(var))
str(all_data)
summary(all_data)


sapply(all_data,function(x) sum(is.na(x)))

missmap(credit, main = "Missing values vs observed")

#Creating the Training and Testing data set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(all_data), replace = T, prob = c(0.7,0.3))
train <- all_data[sample, ]
test <- all_data[!sample, ]

#Fitting a logistic regression model
lrtrain <- glm(default.payment.next.month ~., family = "binomial", data = train)
summary(lrtrain)

#traincm = data.frame(confusion_matrix(lrtrain))
#summary(traincm)

#x=traincm$Predicted.0
#y=traincm$Predicted.1
#total=traincm$Total

#ggplot(data = traincm,
#       mapping = aes(x = Predicted.0,
#                     y = Predicted.1)) +
#  geom_tile(aes(fill = Freq)) +
#  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
#  scale_fill_gradient(low = "blue",
#                      high = "red",
#                      trans = "log")


lrtest = glm(default.payment.next.month ~., family = "binomial", data = test)
summary(lrtest)
#confusion_matrix(lrtest)


#plot confusion matrix for the test model


#we see that limit_bal, sex, marriage, pay_0, pay_6, pay_amt1, pay_amt2, pay_amt5 are all stat sig to d model


#Plotting a graph: Probability of default Vs Balance
all_data %>%
  mutate(prob = ifelse(default.payment.next.month == 1, 1, 0)) %>%
  ggplot(aes(LIMIT_BAL, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")



fitted.results <- predict(lrtest,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
confusion_matrix(lrtest)

misClasificError <- mean(fitted.results != test$default.payment.next.month)
print(paste('Accuracy = ',1-misClasificError))


install.packages("ROCR")
library(ROCR)
library(ggplot2)
p <- predict(lrtrain, newdata=subset(test,select=c(1:24)), type="response")
pr <- prediction(p, test$default.payment.next.month)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

tpr.lrm <- prf@y.values[[1]]
fpr.lrm <- prf@x.values[[1]]

df <- data.frame(tpr=c(tpr.rf), fpr=c(fpr.rf))#,
                # Method=c(rep("LRM",each=length(tpr.rf))) )

ggplot(aes(x=fpr, y=tpr), data=df)+ggtitle("ROC Plot") +
  xlab("False Positive Rates") + ylab("True Positive Rate") + geom_line()

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

