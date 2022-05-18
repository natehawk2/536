ore = read.csv('oregon.csv')
samp = sample(1:nrow(ore), round(nrow(ore)*0.8))
train = ore[samp,]
test = ore[-samp,]


library(ppmSuite)
simParms <- c(0.0, 1.0, 0.1, 1.0, 2.0, 0.1)
draws <- 5000
burn <- 300
thin <- 50
co <- c(-100000, -.5, .5, 100000)
nout <- (draws - burn)/thin

train1 = train[1:1500,]

train1$age <- scale(train1$age)
train1$income <- scale(train1$age)
fit_1 <- ordinal_ppmx(y = train1$enrolled,
                      co = co,
                      X = train1[,-1],
                      M = 1,
                      Xpred = test[,-1],
                      similarity_function = 1,
                      consim = 1,
                      calibrate = 0,
                      simParms = simParms,
                      draws = draws,
                      burn = burn,
                      thin = thin,
                      verbose = TRUE)


auc(test$enrolled, apply(fit_1$ppred, 2, median))
auc(train1$enrolled, apply(fit_1$ord.fitted.values, 2, median))
