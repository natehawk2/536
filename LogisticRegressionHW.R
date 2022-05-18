---
title: "Targeted Marketing"
author: "Nathan Hawkins"
date: "11/5/2021"
output: html_document
---

```{r include = FALSE}
mark <- read.csv("TargetedMarketing.csv", header = TRUE, sep = ";")

mark$job = as.factor(mark$job)
mark$marital = as.factor(mark$marital)
mark$education = as.factor(mark$education)
mark$default = as.factor(mark$default)
mark$housing = as.factor(mark$housing)
mark$loan = as.factor(mark$loan)
mark$contact = as.factor(mark$contact)
mark$month = as.factor(mark$month)
mark$day_of_week = as.factor(mark$day_of_week)
mark$poutcome = as.factor(mark$poutcome)
mark$y = ifelse(mark$y == "no", 0, 1)
mark$y = as.factor(mark$y)
mark$pdays <- ifelse(mark$pdays == "999", 0,
                     ifelse(mark$pdays < 5, 1, 
                          ifelse(mark$pdays > 4 & mark$pdays < 10, 2, 
                                 ifelse(mark$pdays > 9 & mark$pdays < 15, 3, 
                                        ifelse(mark$pdays > 14 & mark$pdays < 20, 4, 
                                               ifelse(mark$pdays > 19, 5, 6))))))

```

#Introduction

• Does the report sufficiently describe the background of the problem?
• Does the report sufficiently describe and plot the data?
• Does the report sufficiently point out any problems/issues that you can see or anticipate with the
problem and/or data? (e.g. why might we not use standard regression?)
• Does the report explain what goes wrong when the above issues are not accounted for?
• Are the goals of the analysis clearly stated?


Targeted marketing campaigns are essential for companies that are trying to optimize their outreach efforts. To reach a target audience, marketing directors should have a good understanding of which groups could be profitable. In this analysis, we will attempt to understand which characteristics make members of a bank more likely to open an account with a credit card. At our disposal we have a data set with demographic information for 41,000 bank members and whether or not they opened a new account. In addition to understanding which variables are most conducive to opening a new account, we are also interested in understanding whether social media or personal contact are more effective in marketing, and if repeated contacting increases the likelihood of a person opening an account. First we will explore the data. Below we show cross tables of a view variables of interest:

```{r}
par(mfrow= c(2,1))
c1 <- crosstable(mark, c(contact), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

mark1 <- mark
mark1$previous <- as.factor(mark$previous)
c2 <- crosstable(mark1, c(previous), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

print(c1)
print(c2)
```

Initially it appears that members that are contacted over the phone rather than over social media are less likely to open a new account. Also, members that have been contacted more appear more likely to open an account. Because we are dealing with a binary response variable (the member either opens an account or doesn't) we will need to use classification techniques instead of regression. To do this, we will use logisitic regression.




# EDA
```{r}
library(ggplot2)
library(dplyr)
scatter.smooth(jitter(mark$previous,amount=.5),jitter(as.numeric(mark$y)-1,amount=.1),pch=19,cex=.5,xlab="Previous",ylab="Diabetes", main = "Diabetes vs. Age Density Graph")


library(forcats)
mark <- mark %>%
  mutate(day_of_week = fct_relevel(day_of_week, 
            "mon", "tue", "wed", 
            "thu", "fri"))

mark$month
mark <- mark %>%
  mutate(month = fct_relevel(month, 
            "mar", 
            "apr", "may", "jun",
            "jul", "aug", "sep", 
            "oct", "nov", "dec"))

library(crosstable)
crosstable(mark, c(y), by=day_of_week, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(day_of_week), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(job), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(marital), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(education), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(month), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(default), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(contact), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

crosstable(mark, c(loan), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)

mark1 <- mark
mark1$previous <- as.factor(mark$previous)
crosstable(mark1, c(previous), by=y, percent_digits=0) %>% 
  as_flextable(keep_id=FALSE)


```




```{r}
mylogit <- glm(y ~ . , data = mark, family = "binomial")
summary(mylogit)
AIC(mylogit)
```

```{r}
mylogit <- glm(y ~ . - job, data = mark, family = "binomial")
summary(mylogit)
AIC(mylogit)
```


```{r}
mylogit <- glm(y ~ . - job, data = mark, family = "binomial")
summary(mylogit)
AIC(mylogit)
```

```{r}
mylogit <- glm(y ~ age  + job + marital + education + default + contact + month + day_of_week + campaign + as.factor(pdays) + previous + poutcome, data = mark, family = binomial(link = "logit"))
#summary(mylogit)
AIC(mylogit)
BIC(mylogit)
```



```{r}
Threshold = seq(0,1,by=0.001)
false.negative.rate <- false.positive.rate <- true.positive.rate <- error.rate <- (0*Threshold-99)
model = mylogit
y = mark$y
hist(model$fitted.values)
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
abline(v = 0.097)

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

```

```{r}
fitted.values = ifelse(mylogit$fitted.values > 0.15, 1, 0)
table(fitted.values)

predictions <- predict(mylogit, type = "response")

hist(predictions)
hist(mylogit$fitted.values)

table(fitted.values, mark$y)




library(car)
#avPlots(mylogit.best)
colnames(crash)
library(pROC)
library(ggplot2)
ggplot(mapping = aes(x = predictions)) + 
  geom_histogram()

auc(mark$y, fitted.values)
```



# Cross validate
```{r}
best.threshold <- 0.097
K = 10
possibilities = 1:nrow(mark)
this.many = round(nrow(mark)/K)

splits <- list()
already.used <- NA
for(i in 2:K){
  samp <- sample(possibilities, this.many, replace = FALSE)
  splits[[i]] <- samp
  possibilities = possibilities[!(possibilities %in% splits[[i]]) ]
}
splits[[1]] = possibilities

error.rate = NA
for(i in 1:K){
  train.data <- mark[-splits[[i]],]
  test.data <- mark[splits[[i]],]
  mylogit <- glm(y ~ age + loan + job + marital + education + default + contact + month + day_of_week + campaign + as.factor(pdays) + previous + poutcome, data = train.data, family = binomial(link = "logit"))
  predictions <- predict(mylogit, type = "response", newdata = test.data)
  preds = ifelse(predictions > best.threshold, 1, 0)

  error.rate[i] <- sum(preds != test.data$y)/nrow(test.data)
}

mean(error.rate)



error.rate.prob = NA
for(i in 1:K){
  train.data <- mark[-splits[[i]],]
  test.data <- mark[splits[[i]],]
  mylogit <- glm(y ~ age + loan + job + marital + education + default + contact + month + day_of_week + campaign + as.factor(pdays) + 
                   previous + poutcome, data = train.data, family = binomial(link = "probit"))
  predictions <- predict(mylogit, type = "response", newdata = test.data)
  preds = ifelse(predictions > best.threshold, 1, 0)

  error.rate.prob[i] <- sum(preds != test.data$y)/nrow(test.data)
}

mean(error.rate.prob)


error.rate.clog = NA
for(i in 1:K){
  train.data <- mark[-splits[[i]],]
  test.data <- mark[splits[[i]],]
  mylogit <- glm(y ~ age + loan + job + marital + education + default + contact + month + day_of_week + campaign + as.factor(pdays) + 
                   previous + poutcome, data = train.data, family = binomial(link = "cloglog"))
  predictions <- predict(mylogit, type = "response", newdata = test.data)
  preds = ifelse(predictions > best.threshold, 1, 0)

  error.rate.clog[i] <- sum(preds != test.data$y)/nrow(test.data)
}

mean(error.rate.clog)
```


