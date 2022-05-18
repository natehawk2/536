crash <- read.table(file = "crash.txt", header = TRUE)
head(crash)
dim(crash)

corrplot::corrplot(cor(crash))

crash$LGT_COND = as.factor(crash$LGT_COND)
crash$WEATHER = as.factor(crash$WEATHER)
crash$ALCOHOL = as.factor(crash$ALCOHOL)
crash$REST_USE = as.factor(crash$REST_USE)
crash$TYP_INT = as.factor(crash$TYP_INT)
crash$VTRAFWAY = as.factor(crash$VTRAFWAY)
crash$VALIGN = as.factor(crash$VALIGN)
crash$VSURCOND = as.factor(crash$VSURCOND)
crash$AIR_BAG = as.factor(crash$AIR_BAG)



mylogit <- glm(SEVERITY ~ . -CASENUM, data = crash, family = "binomial")
summary(mylogit)

library(bestglm)
bestglm(crash[c(2:8,10:14,9)], family = binomial, method = "backward", IC = "AIC")

model = SEVERITY ~ . -CASENUM
fit <- lm(model, kbb)
test <- olsrr::ols_step_all_possible(fit)

mylogit.best <- glm(SEVERITY ~ HOUR + LGT_COND + WEATHER + ALCOHOL + TYP_INT + REST_USE + AIR_BAG + VNUM_LAN + VSPD_LIM + VALIGN, data = crash, family = "binomial")
summary(mylogit.best)
AIC(mylogit.best)



# Plot AUC curve
Threshold = seq(0,1,by=0.001)
false.negative.rate <- false.positive.rate <- true.positive.rate <- error.rate <- (0*Threshold-99)
model = mylogit.best
y = crash$SEVERITY
for(i in 1:length(Threshold)){
  y.hat = ifelse(model$fitted.values < Threshold[i],0,1)
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

## ROC
plot(false.positive.rate,1 - false.negative.rate
     ,xlab = "False Positive Rate (1-Specificity)",ylab = "True Positive Rate (Sensitivity)"
     ,col=4,type='l',lwd=2
)
abline(0,1,lty=3)

#0.441 is the best threshold

1-error.rate[441]
false.negative.rate[441]
false.positive.rate[441]

which.min(abs(false.negative.rate - false.positive.rate))


fitted.values = ifelse(mylogit.best$fitted.values > 0.441, 1, 0)

predictions <- predict(mylogit.best, type = "response")

hist(predictions)
hist(mylogit.best$fitted.values)

table(fitted.values, crash$SEVERITY)



summary(mylogit.best)

library(car)
#avPlots(mylogit.best)
colnames(crash)
library(pROC)
library(ggplot2)
ggplot(mapping = aes(x = predictions)) + 
  geom_histogram()

auc(crash$SEVERITY, fitted.values)

summary(mylogit.best)
