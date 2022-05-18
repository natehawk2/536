letters = read.table(file = "letters.txt", sep = ",", header = TRUE)
head(letters)
library(caret)
library(corrplot)
library(tidyverse)

table(letters$letter)
corrplot(cor(letters[,-1]))

set.seed(536)


## Bagging
trCtrl <- trainControl(method = "cv", number = 10)

samp = sample(1:nrow(letters), 5000)
train = letters[-samp,]
test = letters[samp,]

cr.fit <- train(letter~., data = train, method = "treebag", nbagg = 100,
                trControl = trCtrl, metric = "Accuracy")
preds = predict(cr.fit, test)
table(preds, test$letter)
cr.fit$results
conf_mat <- confusionMatrix(
  data = as.factor(preds),
  reference = as.factor(test$letter),
  positive = "1"
)
conf_mat

mean(preds == test$letter)

varImp(cr.fit)

# KNN

knn.grid = expand.grid(.k = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
knn.fit <- train(letter~., data = train, method = "knn", tuneGrid = knn.grid,
                trControl = trCtrl, metric = "Accuracy")
knn.preds = predict(knn.fit, test)
knn.fit$results
mean(test$letter == knn.preds)
conf_mat <- confusionMatrix(
  data = as.factor(knn.preds),
  reference = as.factor(test$letter),
  positive = "1"
)
conf_mat
