#Libraries
library(readr)
library(nnet)


#1) Data Import

df <- read.csv("Data/02/auto-mpg.csv")
#MPG stands for miles per gallon
#Engines are measured by displacement, usually expressed in liters (L) or 
#cubic centimeters (cc). Displacement is the total volume of all the cylinders 
#in an engine.

#2) Modeling
LinearModel <- multinom(cylinders ~., data = df)
print(LinearModel)
summary(LinearModel)

################################################################################

#3) Quick Overview & Data Cleaning
str(df)

sum(is.na(df))

library(dplyr)
df %>%
  group_by(horsepower) %>%
  summarise(n()) #%>%
  #View()

n_distinct(df$horsepower)

df <- df %>%
  mutate(horsepower = as.numeric(ifelse(horsepower == "?", 0, horsepower)))

library(purrr)
map_df(df, function(element) {
  
  n_distinct(element)
  
})

df %>%
  group_by(cylinders) %>%
  summarise(n()) #%>%
#View()


df$origin <- as.factor(df$origin)
df$model.year <- as.factor(df$model.year)
df$cylinders <- as.factor(df$cylinders)

df$car.name <- NULL

LinearModel <- multinom(cylinders ~., data = df)
print(LinearModel)
summary(LinearModel)

################################################################################
library(caret)
sum(is.na(df))

#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- preProcess(df, method = c("center","scale"))

df_processed <- predict(preProcValues, df)

################################################################################
df_processed$cylinders <- as.numeric(df_processed$cylinders)

#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = df_processed, fullRank = T)
df_transformed <- data.frame(predict(dmy, newdata = df_processed))

df_transformed$cylinders <- as.factor(df_transformed$cylinders)

################################################################################

#4)  Train Test Split
#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(df_transformed$cylinders, p = 0.75, list = FALSE)
train <- df_transformed[ index,]
test <- df_transformed[-index,]

library(skimr)
skim(train)

str(train)

################################################################################
#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

outcomeName <- 'cylinders'
predictors <- names(train)[!names(train) %in% outcomeName]

Best_Cylnder_Predictors <- rfe(train[, predictors], train[, outcomeName],
                         rfeControl = control)

#Taking only the top 4 predictors
predictors<-c("displacement", "weight", "mpg", "acceleration")
################################################################################

model_multinom <- train(train[, predictors], train[,outcomeName], method = 'multinom')
model_gbm <- train(train[, predictors], train[,outcomeName], method = 'gbm')
model_rf <- train(train[, predictors], train[,outcomeName], method = 'rf')
model_nnet <- train(train[, predictors], train[,outcomeName], method = 'nnet')

################################################################################
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)

modelLookup(model='gbm')

#tuneGrid
#Creating grid
#grid <- expand.grid(n.trees=c(20,100,1000),
#                    shrinkage=c(0.01,0.1,0.5),
#                    n.minobsinnode = c(3,5,10),
#                    interaction.depth=c(1,5,10))

# training the model
#model_gbm <- train(train[,predictors],
#                   train[,outcomeName],
#                   method='gbm',
#                   trControl=fitControl,
#                   tuneGrid=grid)

# summarizing the model
#print(model_gbm)

#plot(model_gbm)
################################################################################

#tuneLength
#use any number of possible values for each tuning parameter through tuneLength
model_gbm <- train(train[, predictors],
                   train[, outcomeName],
                   method = 'gbm',
                   trControl = fitControl,
                   tuneLength = 5)

print(model_gbm)
plot(model_gbm)

################################################################################
#Variable importance 

varImp(object=model_multinom)
plot(varImp(object=model_multinom),main="MULTINOM  - Variable Importance")


varImp(object = model_gbm)
plot(varImp(object=model_gbm),main="GBM - Variable Importance")


varImp(object=model_rf)
plot(varImp(object=model_rf),main="RF - Variable Importance")


varImp(object=model_nnet)
plot(varImp(object=model_nnet),main="NNET - Variable Importance")

################################################################################
#Predictions
predictions <- predict.train(object = model_gbm,
                             test[, predictors],
                             type="raw")
table(predictions)

str(test$cylinders)

unique(test$cylinders)
unique(train$cylinders)

################################################################################
confusionMatrix(predictions,test[,outcomeName])

predictions <- predict.train(object = model_gbm,
                             test[, predictors],
                             type="prob")

options(scipen = 999)


