complaints = read.csv("CompanyComplaints.csv")
head(complaints)


library(stringr)
library(tokenizers)

tokens = sapply(complaints$Complaint, tokenize_words)
tally.word = function(X, target.word){
  sum(stringr::str_count(X, pattern = target.word))
}

table(complaints$Department)

complaints$debt = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'debt'))
complaints$collect = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'collect'))
complaints$credit = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'credit'))
complaints$loan = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'loan'))
complaints$mortgage = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'mortgage'))
complaints$checking = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'checking'))
complaints$student = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'student'))
complaints$car = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'car'))
complaints$lease = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'lease'))
complaints$studnet = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'student'))
complaints$currency = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'currency'))
complaints$payday = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'payday'))
complaints$vehicle = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'vehicle'))
complaints$title = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'title'))
complaints$saving = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'payment'))
complaints$company = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'company'))
complaints$report = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'report'))
complaints$card = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'card'))
complaints$consumer = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'consumer'))
complaints$bank = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'bank'))
complaints$litigation = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'litigation'))
complaints$equifax = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'equifax'))


complaints$length = as.vector(sapply(X = tokens, FUN = length))




# Analysis
library(caret)
library(corrplot)
library(tidyverse)
library(beepr)

table(complaints$Department)

set.seed(536)

#############
## Bagging ##
#############
complaints$Department <- as.factor(complaints$Department)
complaints$Department = make.names(complaints$Department)
samp = sample(1:nrow(complaints), round(nrow(complaints)*0.2,1))
train = complaints[-samp,c(1,3:19)]
test = complaints[samp,]


cr.fit <- train(as.factor(Department)~., data = train, method = "treebag", nbagg = 5,
                 metric = "Accuracy", 
                trControl = trainControl(
                  method = "cv", 
                  number = 10, 
                  classProbs = TRUE))

preds.bag = predict(cr.fit, test)

confusionMatrix(data = as.factor(test$Department), reference = as.factor(preds.bag))

# 0.6612
# 0.75

#########
## GBM ##
#########

gbm.fit <- train(Department~., data = train, method = "gbm",
                metric = "ROC", 
                trControl = trainControl(
                  method = "cv", 
                  number = 5, 
                  classProbs = TRUE),
                tuneGrid = data.frame(interaction.depth = 15,
                                      n.trees = 10,
                                      shrinkage = .1, 
                                      n.minobsinnode = 20))
                                      
preds.gbm = predict(gbm.fit, test)
beep(8)

confusionMatrix(data = as.factor(test$Department), reference = as.factor(preds.gbm))

#0.6628
#0/6601
#0.6744
# 0.6847 with 15 interactions depth

#########
## RF  ##
#########
rf.fit <- train(Department~., data = train, method = "ranger", num.trees = 50,
                 metric = "Accuracy",
                 trControl = trainControl(
                   method = "cv",
                   number = 5,
                   classProbs = TRUE))
rf.fit$results
preds.rf = predict(rf.fit, test)
beep(10)

confusionMatrix(data = as.factor(test$Department), reference = as.factor(preds.rf))

#0.7019, 20 trees
# 0.7074, 30


library(vip)
vip(rf.fit)

###########
## N NET ##
###########

net.fit <- train(Department~., data = train, method = "nnet",
                 linout = F, maxit = 1000,
                 trControl = trainControl(
                   method = "cv", 
                   number = 2, 
                   classProbs = TRUE))
preds.net = predict(net.fit, test)

confusionMatrix(data = as.factor(test$Department), reference = as.factor(preds.net))

# 0.6679
beep(sound = 3)


###################
## naive bayes   ##
###################

nb.fit <- train(Department~., data = train, method = "naive_bayes",
                 trControl = trainControl(
                   method = "cv", 
                   number = 10, 
                   classProbs = TRUE))
preds.nb = predict(nb.fit, test)

confusionMatrix(data = as.factor(test$Department), reference = as.factor(preds.nb))

beep(sound = 3)

# 23%

#########
## KNN ##
#########
knn.grid = expand.grid(.k = c(5,10,15))

knn.fit <- train(Department~., data = train, method = "knn",
                 metric = "accuracy", tuneGrid = knn.grid,
                 trControl = trainControl(
                   method = "cv", 
                   number = 5, 
                   classProbs = TRUE))

preds.knn = predict(knn.fit, test)
beep(8)
knn.fit$results
confusionMatrix(data = as.factor(test$Department), reference = as.factor(preds.knn))

# 0.6435

