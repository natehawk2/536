# Time series
library(astsa)

JohnsonJohnson
plot.ts(JohnsonJohnson)
plot.ts(diff(JohnsonJohnson))

gen.dat = arima.sim(n = 4000, model = list(ar = c(0.5), ma = 0.6))
plot.ts(gen.dat)

acf(gen.dat)


pacf(gen.dat)
acf2(gen.dat)


library(forecast)

auto.arima(gen.dat)

model1 <- sarima(gen.dat, p = 2, d = 0, q = 1)

model2 <- sarima(log(JohnsonJohnson), p = 1, d=1, q = 1, S = 4, Q = 1, D = 1)







##
# Time-Series CV
##

# option 1
N = length(JohnsonJohnson)
train = 1:round(N*0.75)
test = (round(N*0.8+1)):N

#build model on train set
#check forecast errors on test set

sarima.for(JohnsonJohnson,8,1,1,2)


## Option 2 - non-overlapping
k = 8
for(i in 1:3){
  cut.point = (60 + (i -1)*k) 
  train = 1:cut.point
  test = cut.point+ 1:k
  
  #build model on train
  #test model on test
}  
  


## Option 3  -  overlapping
k = 8
for(i in 1:17){
  cut.point = 60 + (i -1) 
  train = 1:cut.point
  test = cut.point + 1:k
  
  #build model on train
  #test model on test
}







  