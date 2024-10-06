#trying to create a classification machine learning algorithm that can seperate between the german and danish harbur porpoise clicks
library(readxl)
library(dplyr)
library(caret)

#readin data from german reference stations and cmbine to on dataframe
Ref_DE_Aschau <- read_excel("AS_78 2023 02 28 FPOD_6889_train_details_filtered.xlsx")
Ref_DE_BoEck <- read_excel("BoEck 2023 02 07 FPOD_6877_train_details.xlsx")
Ref_DE_Langholz <- mutate(read_excel("LA_6888_ref23_traindetails_filtered_240617.xlsx"), Country = "DE", Std = "LA")
#bind all dataframes from german ref stations
Ref_DE_all <- rbind(Ref_DE_Aschau, Ref_DE_BoEck, Ref_DE_Langholz)

#read data from danish refernece stations and combine to one dataframe
Ref_DK_6621 <- read_excel("2023 02 21 FPOD_6621 file0_train_details.xlsx")
Ref_DK_6623 <- read_excel("2023 02 21  2023 02 21 FPOD_6623 file0 train details.xlsx")
Ref_DK_6625 <- read_excel("20230221 2023 02 21 FPOD_6625 file0 train details.xlsx")
#bind all dataframes from danish ref stations
Ref_DK_all <- rbind(Ref_DK_6621, Ref_DK_6623, Ref_DK_6625)

#we only want to look at these five columns: "nActualClx","medianKHz","AvPRF","MinICI_us", "NofClstrs", when classifying the clicks
#seperate all necessary columns from the dataframes
Ref_DE <- Ref_DE[,c("nActualClx","medianKHz","AvPRF","MinICI_us", "NofClstrs")]
Ref_DK <- Ref_DK[,c("nActualClx","medianKHz","AvPRF","MinICI_us", "NofClstrs")]

#add a new column to the dataframes to indicate the country of origin
Ref_DE$Country <- as.factor(Ref_DE_all$Country)
Ref_DK$Country <- as.factor(Ref_DK_all$Country)

#combine the two dataframes into one
Ref_all <- rbind(Ref_DE, Ref_DK)

#plot a scatter matrix to get a general idea of the data, without the country of origin
colorlist = c("red", "blue")
pairs(Ref_all[,1:5], col = colorlist[as.numeric(Ref_all$Country)], pch = 19, cex = 0.2, alpha = 0.3)

#count number of classes in the data, to check is the data is balanced
table(Ref_all$Country)

#scale data
Ref_all[,1:5] <- scale(Ref_all[,1:5])

#split data into training and test data
set.seed(123)
trainIndex <- createDataPartition(Ref_all$Country, p = .8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- Ref_all[ trainIndex,]
data_test  <- Ref_all[-trainIndex,]

#fit a decision tree model
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(rattle)
library(RColorBrewer)

model <- rpart(Country ~ ., data = data_train)
rpart.plot(model)

#predict the test data
pred <- predict(model, data_test, type = "class")

#confusion matrix
confusionMatrix(pred, data_test$Country)

#fit a random forest model
library(randomForest)

model_rf <- randomForest(Country ~ ., data = data_train)
pred_rf <- predict(model_rf, data_test)

confusionMatrix(pred_rf, data_test$Country)

#fit a support vector machine model
library(e1071)

model_svm <- svm(Country ~ ., data = data_train)
pred_svm <- predict(model_svm, data_test)

confusionMatrix(pred_svm, data_test$Country)

#fit a neural network model
library(neuralnet)

model_nn <- neuralnet(Country ~ ., data = data_train, hidden = 3)
pred_nn <- predict(model_nn, data_test)

confusionMatrix(pred_nn, data_test$Country)

#fit a gradient boosting model
library(xgboost)

model_xgb <- xgboost(data = data_train[,1:5], label = data_train$Country, nrounds = 100, print_every_n = 10)
pred_xgb <- predict(model_xgb, data_test[,1:5])

confusionMatrix(pred_xgb, data_test$Country)