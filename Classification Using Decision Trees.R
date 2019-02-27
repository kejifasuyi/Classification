library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)

#Importing the Dataset
car_data = read.csv('cardata.csv')

summary(car_data)

#Splitting the dataset into Training Set and Test Set
library(caTools)
set.seed(5580)

split = sample.split(car_data$shouldBuy, SplitRatio = 0.7)
training_set = subset(car_data, split == TRUE)
test_set = subset(car_data, split == FALSE)


########### Algorithm 1: Decision Tree ##################
#Fitting Decision Tree Classifier

DTControl <- trainControl(method = "repeatedcv", number = 10, repeats =3)
set.seed(5580)
DTclassifier <- train(shouldBuy ~., data = training_set,
                method = "rpart",
                parms = list(split = "information"),
                trControl = DTControl,
                tuneLength = 10)
DTclassifier

#Plotting the Results
prp(DTclassifier$finalModel, box.palette = "Blues", tweak = 2)

#Predicting the Test Set Results
DT_pred = predict(DTclassifier, newdata = test_set[-7])

#Making the Confusion Matrix
DTcm = confusionMatrix(DT_pred, test_set$shouldBuy)
DTcm

#Using the Gini Index Parameter
DT_gini <- train(shouldBuy ~., data = training_set, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=DTControl,
                        tuneLength = 10)
DT_gini

#Plotting the Results
prp(DT_gini$finalModel, box.palette = "Purples", tweak = 1.5)

#Predicting the Test Set Results
DT_gini_pred = predict(DT_gini, newdata = test_set[-7])

#Making the Confusion Matrix
DT_gini_cm = confusionMatrix(DT_gini_pred, test_set$shouldBuy)
DT_gini_cm

DTclassifier = rpart(formula = shouldBuy ~ .,
                     data = training_set,
                     parms = list(split = "gini"),
                     control = rpart.control(minsplit = 10))
DTclassifier
#Plotting the ROC
#install.packages('pROC')
DT_gini_prob = predict(DT_gini, newdata = test_set[-7], type = 'prob')
roc.multi <- multiclass.roc(test_set[, 7], DT_gini_prob[,2])
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]], print.auc = TRUE)
sapply(2:length(rs), function(i)
  lines.roc(rs[[i]], col = i))


############# Algorithm 2: Random Forest ###################
#Fitting Random Forest Classifier
#install.packages('randomForest')
RFclassifier = randomForest(training_set[-7],
                            y = training_set$shouldBuy,
                            ntree = 500,
                            mtry = 6)
RFclassifier
#Predicting the test set result
RF_pred = predict(RFclassifier, newdata = test_set[-7])

RFcm = table(test_set[, 7], RF_pred)
RFcm

# k-Fold Cross Validation for Random Forest
#install.packages('caret')

RFfolds = createFolds(training_set$shouldBuy, k = 10)
RFcv = lapply(RFfolds, function(x) {
  RFtraining_fold = training_set[-x,]
  RFtest_fold = training_set[x,]
  RFclassifier = randomForest(training_set[-7],
                              y = training_set$shouldBuy,
                              ntree = 500,
                              mtry = 6)
  RF_pred = predict(RFclassifier, newdata = RFtest_fold[-7])
  RFcm = table(RFtest_fold[, 7], RF_pred)
  RFaccuracy = (RFcm[1, 1] + RFcm[2, 2] + RFcm[3, 3] + RFcm[4, 4]) / sum(RFcm)
  return(RFaccuracy)
})

RFaccuracy = mean(as.numeric(RFcv))
RFaccuracy

