
  
library(tidyverse)
library(vroom)
library(ggplot2)
library(e1071)
library(randomForest)
library(ranger)
cc <- vroom("CCFraud.csv")
head(cc)



samp = sample(1:nrow(cc), round(nrow(cc)*0.5))

train = cc[-samp,]
test = cc[samp,]

K = 10
possibilities = 1:nrow(cc)
this.many = round(nrow(cc)/K)

splits <- list()
already.used <- NA
for(i in 2:K){
  samp <- sample(possibilities, this.many, replace = FALSE)
  splits[[i]] <- samp
  possibilities = possibilities[!(possibilities %in% splits[[i]]) ]
}
splits[[1]] = possibilities
```



library(pROC)
auc_linear = NA
i = 1
for(i in 1:10){
  train = cc[splits[[i]],]
  if(i == 10){
    test = cc[splits[[1]],]
  }
  else{
    test = cc[splits[[i+1]],]
  }
  
  model1 = svm(Class ~ ., data=train, kernel = "linear")
  p1 = predict(model1,newdata = test)
  
  auc_linear[i] = auc(test$Class, p1 )
  
}
mean(auc_linear)


auc_polynomial = NA
i = 1
for(i in 1:10){
  train = cc[splits[[i]],]
  if(i == 10){
    test = cc[splits[[1]],]
  }
  else{
    test = cc[splits[[i+1]],]
  }
  
  model1 = svm(Class ~ ., data=train, kernel = "polynomial")
  p1 = predict(model1,newdata = test)
  
  auc_polynomial[i] = auc(test$Class, p1 )
  preds1 = ifelse(p1 > 0.05, 1, 0)
  
}
mean(auc_polynomial)

auc_radial = NA
i = 1
for(i in 1:10){
  train = cc[splits[[i]],]
  if(i == 10){
    test = cc[splits[[1]],]
  }
  else{
    test = cc[splits[[i+1]],]
  }
  
  model1 = svm(Class ~ ., data=train, kernel = "radial")
  p1 = predict(model1,newdata = test)
  
  auc_radial[i] = auc(test$Class, p1 )
  
}
mean(auc_radial)


auc_sigmoid = NA
i = 1
for(i in 1:10){
  train = cc[splits[[i]],]
  if(i == 10){
    test = cc[splits[[1]],]
  }
  else{
    test = cc[splits[[i+1]],]
  }
  
  model1 = svm(Class ~ ., data=train, kernel = "sigmoid")
  p1 = predict(model1,newdata = test)
  
  auc_sigmoid[i] = auc(test$Class, p1 )
  
}

mean(auc_sigmoid)


#model1 = svm(Class ~ ., data=cc, kernel = "sigmoid")


