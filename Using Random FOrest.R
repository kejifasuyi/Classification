#Random Forest Classification

car_data = read.csv("cardata.csv")
#Encoding Categroical Variables 
car_data$price = factor(car_data$price,
                        levels = c('low', 'med', 'high', 'vhigh'),
                        labels = c(1, 2, 3, 4))

car_data$maintenance = factor(car_data$maintenance,
                              levels = c('low', 'med', 'high', 'vhigh'),
                              labels = c(1, 2, 3, 4))

car_data$doors = factor(car_data$doors,
                        levels = c('2', '3', '4', '5more'),
                        labels = c(2, 3, 4, 5))

car_data$seats = factor(car_data$seats,
                        levels = c('2', '4', 'more'),
                        labels = c(2, 4, 6))

car_data$storage = factor(car_data$storage,
                          levels = c('big', 'med', 'small'),
                          labels = c(3, 2, 1))

car_data$safety = factor(car_data$safety,
                         levels = c('high', 'med', 'low'),
                         labels = c(3, 2, 1))

car_data$shouldBuy = factor(car_data$shouldBuy,
                            levels = c('unacc', 'acc', 'good', 'vgood'),
                            labels = c(0, 1, 2, 3))


summary(car_data)

#Splitting the dataset into Training Set and Test Set
library(caTools)
set.seed(5580)

split = sample.split(car_data$shouldBuy, SplitRatio = 0.7)
training_set = subset(car_data, split == TRUE)
test_set = subset(car_data, split == FALSE)


#Splitting the Test set into validation and test
split = sample.split(test_set$shouldBuy, SplitRatio = 0.5)
test_set = subset(test_set, split == TRUE)
validation_set = subset(test_set, split == FALSE)

#Fitting Random Forest Classifier
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(training_set[-7],
                          y = training_set$shouldBuy,
                          ntree = 10)

#Predicting the test set result
y_pred = predict(classifier, newdata = test_set[-7])

cm = table(test_set[, 7], y_pred)
cm


# k-Fold Cross Validation
#install.packages('caret')
library(caret)
folds = createFolds(training_set$shouldBuy, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = randomForest(training_set[-7],
                            y = training_set$shouldBuy,
                            ntree = 10)
  y_pred = predict(classifier, newdata = test_fold[-7])
  cm = table(test_fold[, 7], y_pred)
  accuracy = (cm[1,1] + cm[2,2] + cm[3,3] + cm[4,4]) / sum(cm)
  return(accuracy)
  })

accuracy = mean(as.numeric(cv))





