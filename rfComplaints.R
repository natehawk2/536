xtest = read.csv("X_test.csv")
library(caret)

ytrain = read.csv("")


rf.fit <- train(Department~., data = train, method = "ranger", num.trees = 5,
                metric = "Accuracy",
                trControl = trainControl(
                  method = "cv",
                  number = 2,
                  classProbs = TRUE))

rf.fit$results

preds.rf = predict(xtest, rf.fit)

confusionMatrix(data = as.factor(xtest$Department), reference = as.factor(preds.rf))




