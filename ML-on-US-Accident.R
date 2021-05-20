library(tidyverse)
library(scales)
library(lubridate)
library(plotly)
library(gridExtra)
#install.packages("tidytext")
library(tidytext)
library(tidyselect)
library(modelr)
library(caret)
#install.packages("ROSE")
library(ROSE)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
options(warn = -1)

setwd("C:/Users/Remi/Documents/Semester 1/Data Mining and Machine Learning 1/Project/Dataset/")
df1 <- read_csv("US_Accidents_Dec19.csv", col_types = cols(.default = col_character())) %>% 
  type_convert()


##very large data so we take a sample size of 200,000 rows
set.seed(1)
df <- df1[sample(nrow(df1),200000),]

# Let's take a look at the dateset:
df %>% head(5)

##########################################
# # Data pre-processing for visualization
##########################################
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}


# ## 1. Drop variables with more than 50% NA proportion
df %>% summarise_all(~ mean(is.na(.))) %>% 
  pivot_longer(1:49, names_to = "Variables to drop", values_to = "NA proportion") %>% 
  filter(`NA proportion` >= 0.5)

drop_na_cols <- c("End_Lat", "End_Lng", "Number", "Wind_Chill(F)", "Precipitation(in)")

# ## 2. Drop unuseful variables
# "ID", "Source" and "Timezone" will not be useful in predicting severity levels, 
#so we can drop these variables
not_useful <- c("ID", "Source", "Timezone", "Airport_Code", "Weather_Timestamp", 
                "Wind_Direction", "Description")

df %>% select(not_useful) %>% head(5)
df_drop <- df %>% select(-(drop_na_cols), -(not_useful))


#rename column names
df_drop <-  df_drop %>%
  rename("Distance" = `Distance(mi)`, "Temperature" = `Temperature(F)`, "Humidity" = `Humidity(%)`, 
         "Pressure" = `Pressure(in)`, "Visibility" = `Visibility(mi)`, "Wind_Speed" = `Wind_Speed(mph)`)

#Transform time related variables
df_drop %>% select(Start_Time, End_Time) %>% head(5)

df_time <- df_drop %>%
  mutate(Duration = as.numeric(End_Time - Start_Time)) %>%
  # accident duration should be positive
  filter(!(Duration < 0)) %>%
  separate(Start_Time, into = c("Date", "Time"), sep = " ") %>%
  mutate("Year" = str_sub(Date, 1, 4), "Month" = str_sub(Date, 6, 7), "Day" = str_sub(Date, 9, 10), 
         "Wday" = as.character(wday(Date)), "Hour" = str_sub(Time, 1, 2)) %>%
  select(-c("Date", "Time", "End_Time")) %>%
  select(TMC, Severity, Year, Month, Day, Hour, Wday, Duration, everything())

# * After transformation:
df_time %>%
  select(Year, Month, Day, Hour, Wday, Duration) %>%
  head(5)

# ## 5. Drop weather condition NA level
df_time %>% filter(is.na(Weather_Condition)) %>% select(Temperature:Weather_Condition) %>%
  head(10) 

# So it should be safe to remove all records containing NA weather condition level.
df_weather <- df_time %>% filter(!is.na(Weather_Condition))

# ## 6. Handle TMC NA values
df_TMC <- df_weather %>%
  mutate(TMC = replace_na(TMC, "NA_TMC"))

# ## 7. Location related variables
address <- c("Country", "City", "County", "Street", "Zipcode")
df_TMC %>%
  select(address) %>%
  head(5)

df_add <- df_TMC %>% select(-(address))

# ## 8. Modify variable type
df_add <- df_add %>% 
  mutate(TMC = as.character(TMC), Severity = as.character(Severity)) %>% 
  mutate_if(is.logical, as.character)

# ## 9. Handle NA values in continuous variables
df_mean <- df_add %>%
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = T)))

summary(df_mean %>% select_if(is.numeric))

# ## 10. Handle NA values in categorical variables
df_mean %>% summarise_all(~sum(is.na(.))) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_count") %>% filter(NA_count > 0)

df_final <- df_mean %>%
  filter(!is.na(Side)) %>%
  filter(!is.na(Sunrise_Sunset))

## 11. Write into a csv file for future use
write_csv(df_final, "neat_acc_data")
######################################################################################
#VISUALISATION COMES HERE
######################################################################################
states <- map_data("state") %>% as_tibble() %>% select(long, lat, group, region)
states_abb <- read_csv("data.csv") %>%
  mutate(State = tolower(State)) %>%
  select(State, Code) %>%
  rename("State_full" = State)
accident_count <- df %>%
  count(State) %>%
  left_join(states_abb, by = c("State" = "Code"))

states <- states %>%
  left_join(accident_count, by = c("region" = "State_full"))
# top 10 states
top_10 <- accident_count %>%
  arrange(desc(n)) %>%
  head(10)
top_10 <- top_10$State %>% unlist()

top_10_map <- states %>%
  filter(State %in% top_10)
top_10_label <- top_10_map %>%
  group_by(region, State) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n), color = "#636363", size = 0.1) +
  geom_polygon(data = top_10_map, color = "#fee5d9", fill = NA, size = 0.8) +
  scale_fill_gradient(low = "#fee5d9", high = "Blue",
                      name = "Accident Count", labels = unit_format(unit = "K", scale = 1e-03)) +
  ggrepel::geom_label_repel(mapping = aes(label = State, group = 1), data = top_10_label) +
  theme_minimal() +
  coord_quickmap() +
  labs(title = "Accident distribution in the U.S.",
       x = "Longitude",
       y = "Latitude")
#####################################################################
#FURTHER VISUALISATION TO SHOW CALIFORNIA AS COUNTY WITH HIGHEST ACCIDENT
# df %>% 
#   filter(State %in% top_10) %>%
#   count(State) %>%
#   ggplot(aes(reorder(State, n), n)) +
#   geom_col() +
#   geom_label(aes(label = n), nudge_y = -30000) +
#   labs(x = NULL, y = "Number of accidents",
#        title = "Top 10 States with the most accidents") +
#   scale_x_discrete(labels = rev(c("California", "Texas", "Florida", "South Carolina",
#                                   "North Carolina", "New York", "Pennsylvania",
#                                   "Michigan", "Illinois", "Georgia"))) +
#   scale_y_continuous(breaks = seq(0, 700000, 100000), labels = unit_format(unit = "K", scale = 1e-03)) +
#   coord_flip()
################################################################################################

#######################################################################################
#Modelling Continued
#######################################################################################
df_CA <- df_final %>% filter(State == "CA") %>% select(-State)
df_CA %>%
  head(5)

# ## 2. Drop weather condition or TMC levels
df_CA %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition, n)

drop_weather <- df_CA %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition)
drop_weather <- drop_weather$Weather_Condition %>% unlist()
df_CA <- df_CA %>% 
  filter(!(Weather_Condition %in% drop_weather)) %>% 
  mutate(Weather_Condition = factor(Weather_Condition))

df_CA %>% count(TMC) %>% filter(n < 10)

drop_TMC <- df_CA %>% count(TMC) %>% filter(n < 10) %>% select(TMC)
drop_TMC <- drop_TMC$TMC %>% unlist()
df_CA <- df_CA %>% filter(!TMC %in% drop_TMC) %>% mutate(TMC = factor(TMC))

# ## 3. Group 4 severity levels into 2 levels
# After grouping:
df_label <- df_CA %>%
  mutate("Status" = factor(ifelse(Severity == "3" | Severity == "4", "Severe", "Not Severe"), 
                           levels = c("Not Severe", "Severe")))
ggplot(df_label, aes(Status, fill = !Status == "Severe")) +
  geom_bar() +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
  scale_fill_discrete(name = "Severity", labels = c("Severe", "Not Severe")) +
  labs(y = "Count",
       x = "Severity",
       title = "More balanced severity levels")
# ## 4. Near Zero-Variance Predictors
nzv <- nearZeroVar(df_label, saveMetrics = T)
nzv[nzv$nzv,]

nzv_cols <- rownames(nzv[nzv$nzv,])
df_label <- df_label %>%
  select(-(nzv_cols))

# ## 5. Partition
# Here, we follow the typical data analysis workflow by splitting the dataset into 3 sub datasets: 
#training(60%), validation(20%) and test(20%).
set.seed(1)
df_parts <- resample_partition(df_label, c(train = 0.6, valid = 0.2, test = 0.2))
train_set <- as_tibble(df_parts$train)
valid_set <- as_tibble(df_parts$valid)
test_set <- as_tibble(df_parts$test)

# # Modeling
# ## 1. Sampling
# Just one more step before we start building models. By grouping the 4 severity levels into 2 levels, now the dataset is more balanced in severity levels. However, from the plot below, we can see the records in each severity level are still not equal. 
ggplot(train_set, aes(Status)) +
  geom_bar(aes(fill = Status)) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
  labs(y = "Count",
       title = "Unbalanced severity levels")

#Create balanced set
new_train <- ovun.sample(Status ~ ., 
                         data = train_set, 
                         method = "both", p = 0.5, N = 90000, seed = 1)$data %>% as_tibble()

ggplot(new_train, aes(Status)) +
  geom_bar(aes(fill = Status)) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
  labs(y = "Count",
       title = "Balanced severity levels")

new_train <- new_train %>% select(-Severity)
###############################################
# Logistic regression
#################################################
model_aic <- glm(Status ~ ., data = new_train, family = "binomial")
model_aic <- step(model_aic)

# These variables are dropped:
model_aic$anova[2:nrow(model_aic$anova), c(1, 6)] %>% as_tibble() %>% mutate(Step = str_sub(Step, start = 3)) %>%
  rename("Vaiables to drop" = Step)

# The final formula based on AIC value:
model_aic$call

# Make predictions on validation dataset. 
# Here, we choose 0.6 as the cutoff (transform probability to response variable levels)
#to gain a higher total accuracy.
# We can see the performance of logistic regresson by using confusion matrix:
valid_pred <- 0
valid_pred <- new_train %>%
  mutate(pred = ifelse(model_aic$fitted.values > 0.6, "Severe", "Not Severe"))
#  mutate(pred = ifelse(Status > 0.6, "Severe", "Not Severe"))
cm_lr <- confusionMatrix(table(valid_pred$pred, valid_pred$Status))
tibble("Accuracy" = cm_lr$overall[[1]], "Sensitivity" = cm_lr$byClass[[1]],
       "Specificity" = cm_lr$byClass[[2]], "Positive term" = cm_lr$positive)
cm_lr

###########################################################################
#Decision trees
############################################################################
model_decision <- rpart(Status~ ., data = new_train, method = "class", minsplit = 20, cp = 0.001)
# fig(16, 8)
# rpart.plot(model_decision, box.palette = "RdBu", shadow.col = "grey", )

valid_pred <- test_set %>%
  mutate(pred = predict(model_decision, test_set, type = "class"))

cm_dt <- confusionMatrix(table(valid_pred$pred, valid_pred$Status))
tibble("Accuracy" = cm_dt$overall[[1]], "Sensitivity" = cm_dt$byClass[[1]],
       "Specificity" = cm_dt$byClass[[2]], "Positive term" = cm_dt$positive)
cm_dt

#######################################################
# ## 5. Random forest
# #####################################################
##randomForest will not accept chr format so we convert all chr to factor
for (i in 1:ncol(new_train)) { # For every column...
  if (typeof(new_train[[i]]) == 'character') { # if the column type is character...
    new_train[[i]] <- as.factor(new_train[[i]]) # Convert it to factor. 
  }
}
 
# Let's see if random forest can imporve the accuracy even better.
model_rf <- randomForest(Status~., new_train, mtry=6, ntree=500)
model_rf

model_rf1 <- randomForest(Status~., new_train, mtry=18, ntree=500)
model_rf1


cm_rf <- confusionMatrix(table(model_rf1$predicted, new_train$Status))
tibble("Accuracy" = cm_rf$overall[[1]], "Sensitivity" = cm_rf$byClass[[1]],
       "Specificity" = cm_rf$byClass[[2]], "Positive term" = cm_rf$positive)
cm_rf

##########################################################
# Compare The Result from Both Models
##########################################################

result  <- tibble("Model" = c("Logistic Regression","Decision Tree", "Random Forest"),
                  "Accuracy" = c(cm_lr$overall["Accuracy"],cm_dt$overall["Accuracy"], cm_rf$overall['Accuracy']),
                  "Sensitivity" = c(cm_lr$byClass["Sensitivity"], cm_dt$byClass["Sensitivity"], cm_rf$byClass["Sensitivity"]),
                  "Specificity" = c(cm_lr$byClass["Specificity"], cm_dt$byClass["Specificity"], cm_rf$byClass["Specificity"])) %>%
  pivot_longer(2:4, names_to = "type", values_to = "value")

fig(15, 8)
result %>% ggplot(aes(type, value, fill = factor(Model, levels = c("Logistic Regression", "Decision Tree", "Random Forest")))) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "Model") +
  labs(x = "Performance",
       y = NULL,
       title = "Comparison of model performance")
