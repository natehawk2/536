install.packages("bestglm")
mark <- read.csv("TargetedMarketing.csv", header = TRUE, sep = ";")
head(mark)

print("yes")

mark$job = as.factor(mark$job)
mark$marital = as.factor(mark$marital)
mark$education = as.factor(mark$education)
mark$default = as.factor(mark$default)
mark$housing = as.factor(mark$housing)
mark$loan = as.factor(mark$loan)
mark$contact = as.factor(mark$contact)
mark$month = as.factor(mark$month)
mark$day_of_week = as.factor(mark$day_of_week)
mark$campaign = as.factor(mark$campaign)
mark$pdays = as.factor(mark$pdays)
mark$poutcome = as.factor(mark$poutcome)
mark$y = as.factor(mark$y)





library(bestglm)
best.glm <- bestglm(mark, family = binomial, method = "backward", IC = "AIC")

summary(best.glm)



