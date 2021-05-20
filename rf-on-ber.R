##Random Forest
library(randomForest)
library(MASS)
install.packages("caTools")
library(caTools)

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

set.seed(1)
train = sample(nrow(ber.subset), 0.7*nrow(ber.subset), replace=FALSE)
train.set = ber.subset[train,]
test.set=ber.subset[-train,]

set.seed(1)

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
for (i in 1:9) {
  model3 <- randomForest(EnergyRating~ ., data = train.set, ntree = 500, mtry = i, importance = TRUE)
  predTest <- predict(model3, test.set, type = "class")
  a[i-2] = mean(predTest == test.set$EnergyRating)
}

a

plot(3:9,a)
