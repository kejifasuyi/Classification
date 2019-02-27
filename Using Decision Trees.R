#Using Decision Trees

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


#Fitting Decision Tree Classifier
library(rpart)
classifier = rpart(formula = shouldBuy ~ .,
                   data = training_set)

#Predicting the Test Set Results
y_pred = predict(classifier, newdata = test_set[-7], type = 'class')

#Making the Confusion Matrix
cm = table(test_set[, 7], y_pred)
cm

vald_pred = predict(classifier, newdata = validation_set[-7], type = 'class')
cm2= table(validation_set[, 7], vald_pred)
cm2

#Plotting the Decision Tree
plot(classifier)
text(classifier)
