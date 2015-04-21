data(iris)
set.seed(2)
irisShuffled <- iris[sample(nrow(iris)),]
irisShuffled[,5] <- unclass(irisShuffled[,5])
(irisShuffled)
tra.iris <- irisShuffled[1:105,] 
tst.iris <- irisShuffled[106:nrow(irisShuffled),1:4] 
real.iris <- matrix(irisShuffled[106:nrow(irisShuffled),5], ncol = 1)
range.data.input <- matrix(c(4.3, 7.9, 2.0, 4.4, 1.0, 6.9, 0.1, 2.5), nrow=2)
#install.packages('frbs')
library('frbs')
data("frbsData")
summary(frbsData)
data.train <- frbsData$GasFurnance.dt[1 : 204, ]
data.tst <- frbsData$GasFurnance.dt[205 : 292, 1 : 2] 
real.val <- matrix(frbsData$GasFurnance.dt[205 : 292, 3], ncol = 1) 
summary(data.tst)

range.data<-matrix(c(-2.716, 2.834, 45.6, 60.5, 45.6, 60.5), nrow=2)
method.type <- "ANFIS"
control <- list(num.labels = 5, max.iter = 100, step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", name = "GasFur") 
object.ANFIS <- frbs.learn(data.train, range.data, method.type, control)
pred.ANFIS <- predict(object.ANFIS, data.tst)
bench <- cbind(real.val, pred.ANFIS) 
colnames(bench) <- c("real", "ANFIS")
residuals.ANFIS <- (real.val - pred.ANFIS)
MSE.ANFIS <- mean(residuals.ANFIS^2)
RMSE.ANFIS <- sqrt(mean(residuals.ANFIS^2)) 
SMAPE.ANFIS <- mean(abs(residuals.ANFIS)/(abs(real.val) + abs(pred.ANFIS))/2)*100 
err.ANFIS <- c(MSE.ANFIS, RMSE.ANFIS, SMAPE.ANFIS) 
names(err.ANFIS) <- c("MSE", "RMSE", "SMAPE") 
print("ANFIS Error Measurement: ")
print(err.ANFIS) 
summary(object.ANFIS)


