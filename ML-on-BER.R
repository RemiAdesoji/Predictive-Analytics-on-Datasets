#install.packages("class")
library(class)
library(naniar)
library(caret)
library(sp)
library(ggplot2)
library(randomForest)
library(tidyverse)

setwd("C:/Users/Remi/Documents/Semester 1/Data Mining and Machine Learning 1/Project/Dataset/")
BERdata <- read.csv("BER1.csv", header = T, na.strings = c(""), stringsAsFactors = T)

##Pre-processing
#random selection of 100000 rows of data
set.seed(1)
ber.subset <- BERdata[sample(nrow(BERdata),100000),]

##Here we take only the columns we will be working with
ber.subset = ber.subset[c("EnergyRating","Year_of_Construction","GroundFloorArea.sq.m.",
                          "WallArea","RoofArea","FloorArea","WindowArea","DoorArea","NoStoreys",
                          "CO2Rating")]

#check number of missing values in each column
sapply(ber.subset,function(x) sum(is.na(x)))

##visualise to see missing values
vis_miss(ber.subset, warn_large_data = FALSE)

##Lets look at the summary and decide further what columns to further remove
str(ber.subset)

#convert year of construction to a factor
ber.subset$Year_of_Construction = as.factor(ber.subset$Year_of_Construction)


##Data Normalisation
normalize = function(x){
  return((x-min(x)) / (max(x) - min(x)))
}

ber.subset.norm = as.data.frame(lapply(ber.subset[,3:10], normalize))


##########################################
# # Data pre-processing for visualization
##########################################
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

##Data Splicing
set.seed(1)
dat.d = sample(1:nrow(ber.subset.norm), size = nrow(ber.subset.norm) *0.7, replace=FALSE)

train.ber = ber.subset.norm[dat.d,] # 70% training data
test.ber = ber.subset.norm[-dat.d,] # 30% testing data

#next seperate dataframe for "EnergyRating" for predicting
train.ber.labels = ber.subset[dat.d,1]
test.ber.labels = ber.subset[-dat.d,1]



#################################################
#K Nearest Neighbour on Building Energy Rating
################################################
sqrt(NROW(train.ber.labels))

#knn.1 = knn(train = train.ber, test = test.ber, cl = train.ber.labels, k = 1)

knn.256 = knn(train.ber, test.ber, train.ber.labels, k=265)



##Calculate the percentage of correct classification
#acc.1 = 100 * sum(test.ber.labels == knn.1)/NROW(test.ber.labels)
acc.265 = 100 * sum(test.ber.labels == knn.256)/NROW(test.ber.labels)

##check prediction against actual values
table(knn.256, test.ber.labels)


##use confusion matrix to calculate accuracy
cm_knn_ber = confusionMatrix(table(knn.256, test.ber.labels))
tibble("Accuracy" = cm_knn_ber$overall[[1]])
cm_knn_ber


#################################################
#Random Forest on Building Energy Rating
################################################

set.seed(1)
ber.subset2 = ber.subset[,-2]
str(ber.subset2)
train = sample(nrow(ber.subset2), 0.7*nrow(ber.subset2), replace=FALSE)
train.set = ber.subset2[train,]
test.set=ber.subset2[-train,]

str(train.set)

#modelling random forest on BER
model_ber_rf = randomForest(EnergyRating~., data = train.set, importance=TRUE)



#predict of training data set
predTrain = predict(model_ber_rf, train.set, type = "class")
#check classification accuracy
table(predTrain, train.set$EnergyRating)

#predicting on test data set
predTest = predict(model_ber_rf, test.set, type = "class")
#check classification accuracy
mean(predTest == test.set$EnergyRating)
table(predTest, test.set$EnergyRating)

#check important variables
importance(model_ber_rf)
varImpPlot(model_ber_rf)

# Using For loop to identify the right mtry for model
a=c()
i=1
for (i in 1:8) {
  model3 <- randomForest(EnergyRating~ ., data = train.set, ntree = 500, mtry = i, importance = TRUE)
  predTest <- predict(model3, test.set, type = "class")
  a[i-2] = mean(predTest == test.set$EnergyRating)
}

a

plot(2:8,a)

valid_pred <- test.set %>%
  mutate(pred = predict(model_ber_rf, test.set, type = "class"))

cm_rf_ber <- confusionMatrix(table(valid_pred$pred, valid_pred$EnergyRating))
tibble("Accuracy" = cm_rf_ber$overall[[1]])
cm_rf_ber

##########################################################
# Compare The Result from Both Models
##########################################################

result  <- tibble("Model" = c("K Nearest Neighbour", "Random Forest"),
                  "Accuracy" = c(cm_knn_ber$overall["Accuracy"], cm_rf_ber$overall['Accuracy'])) %>%
  pivot_longer(2:2, names_to = "type", values_to = "value")

fig(20, 8)
result %>% ggplot(aes(type, value, fill = factor(Model, levels = c("K Nearest Neighbour", "Random Forest")))) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "Model") +
  labs(x = "Performance",
       y = NULL,
       title = "Comparison of Model Performance on BER")
