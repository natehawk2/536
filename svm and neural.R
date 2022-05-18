ad <- read.csv("Admissions.csv")
ad$Status <- ifelse(ad$Status == "Admitted", 1, 0)


head(ad)

nrow(ad)
samp <- sample(1:500, 100)
test <- ad[samp,]
train <- ad[-samp]

ad$Status <- as.factor(ad$Status)

library(e1071)
model = svm(Status ~ ., data=train, kernel = "polynomial")
summary(model)
plot(svm,test,GRE.Score ~ CGPA,col=c(3,4,12))
p = predict(model,newdata = test)


plot(p, test$Status)


Threshold = seq(0,1,by=0.001)
false.negative.rate <- false.positive.rate <- true.positive.rate <- error.rate <- (0*Threshold-99)
model = model
y = ad$Status
for(i in 1:length(Threshold)){
  y.hat = ifelse(model$fitted < Threshold[i],0,1)
  error.rate[i] = mean(y!=y.hat)
  TP = sum(y==1 & y.hat==1)
  TN = sum(y==0 & y.hat==0)
  FP = sum(y==0 & y.hat==1)
  FN = sum(y==1 & y.hat==0)
  false.negative.rate[i] = FN/(FN+TP) # 1-sensitivity
  false.positive.rate[i] = FP/(FP+TN) # 1-specificity
}

## Errors plot from slide
plot(Threshold,error.rate,ylim=c(0,1),ylab='Error Rate',type='l',lwd=2)
lines(Threshold,false.negative.rate,col=4,lty=2,lwd=2)
lines(Threshold,false.positive.rate,col=2,lty=3,lwd=2)
abline(v = best.threshold)

## ROC
plot(false.positive.rate,1 - false.negative.rate
     ,xlab = "False Positive Rate (1-Specificity)",ylab = "True Positive Rate (Sensitivity)"
     ,col=4,type='l',lwd=2
)
abline(0,1,lty=3)

best.threshold = which.min(abs(false.negative.rate - false.positive.rate))

1-error.rate[best.threshold]
false.negative.rate[best.threshold]
false.positive.rate[best.threshold]

library(pROC)
p = predict(model,newdata = test)




binary_predicitons <- ifelse(p > best.threshold/1000, 1, 0)
table(binary_predicitons, test$Status)
table(binary_predicitons)
mean(binary_predicitons == test$Status)
binary.fitted = ifelse(model$fitted > best.threshold/1000, 1, 0)
mean(binary.fitted == train$Status)

auc(test$Status, p)








# Neural Net

library(neuralnet)


K = 5
possibilities = 1:nrow(ad)
this.many = round(nrow(ad)/K)

splits <- list()
already.used <- NA
for(i in 2:K){
  samp <- sample(possibilities, this.many, replace = FALSE)
  splits[[i]] <- samp
  possibilities = possibilities[!(possibilities %in% splits[[i]]) ]
}
splits[[1]] = possibilities




set.seed(824)
library(caret)
ad$Status = as.factor(ad$Status)
samp <- sample(1:500, 400)
test <- ad[-samp,]
train <- ad[samp,]

nn <- train(Status ~ ., train, method = "nnet", linout = F, maxit = 1000)
summary(nn)
p = predict(nn)
auc(train$Status, as.numeric(p))

mean(p == train$Status)

pr = predict(nn, newdata = test)
mean(pr == test$Status)
auc(as.numeric(pr),test$Status)







