#
#
rm(list = ls())
setwd("/Users/KevQuant/Downloads/Project")
mpg.data <- read.table('auto-mpg.data')

colnames(mpg.data) <- c("mpg","cylinders","displacement","horsepower",
                        "weight","acceleration","model_year","origin",
                        "car_name")
View(mpg.data)
summary(mpg.data)
nrow(mpg.data)
sapply(mpg.data, class)

#horsepower is listed as a categorical variable (factor), when it should be
#a numerical. This is because some of the values contain the text "?".
#remove all instances where horsepower="?", and transform into numerical type:

mpg.data$horsepower <- lapply(mpg.data$horsepower, function(x) {
  gsub("?", "", x)
})
mpg.data$horsepower <- as.numeric(mpg.data$horsepower)
attach(mpg.data)

#Omit na on the train and test set data
mpgTrain <- na.omit(mpg.data[0:300,])
mpgTest <- na.omit(mpg.data[301:nrow(mpg.data),])


#Examine shape of variable distributions:
library(reshape2)
library(ggplot2)
d <- melt(mpgTrain[,-c(2,8,9)])
d.2 <- melt(mpgTrain[,c(2,8)])

ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()
#mpg, displacement, horsepower, and weight don't seem particularly normal. 
#they appear more right tailed. Might be preferable to transform them (logs perhaps),
#but for now we will keep them as is.
#acceleration appears approximately normal.
#model year appears uniformly distributed - probably better as categorical

ggplot(d.2,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()
#cylanders,origin,and year are probabily better treated as categorical variables


#transform right tailed variables
#log.displacement <- log10(displacement)
#hist(log.displacement)

#Let's go with mpg as the response variable, and use displacement, 
#horsepower, weight, and acceleration as the independent variables.


#plot individual predictors against response variable, Log.views
par(mar=c(2,2,2,2))
par(mfrow=c(2,2))
plot(mpgTrain$displacement, mpgTrain$mpg, xlab = "Displacement",
     ylab = "MPG", main = "MPG vs. Displacement")
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement), col="red")
plot(mpgTrain$horsepower, mpgTrain$mpg, xlab = "Horsepower",
     ylab = "MPG", main = "MPG vs. Horsepower")
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower), col="red")
plot(mpgTrain$weight, mpgTrain$mpg, xlab = "Weight",
     ylab = "MPG", main = "MPG vs. Weight")
abline(lm(mpgTrain$mpg ~ mpgTrain$weight), col="red")
plot(mpgTrain$acceleration, mpgTrain$mpg, xlab = "Acceleration",
     ylab = "MPG", main = "MPG vs. Acceleration")
abline(lm(mpgTrain$mpg ~ mpgTrain$acceleration), col="red")
dev.off()

#run simple regressions on combination of 2 variables on 6 cases:
par(mar=c(2,2,2,2))
par(mfrow=c(2,2))
lm.mpg.disp_hp <- lm(mpg ~ displacement + horsepower, data = mpgTrain)
summary(lm.mpg.disp_hp)
plot(lm.mpg.disp_hp)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement + mpgTrain$horsepower), col="red")

lm.mpg.disp_weight<- lm(mpg ~ displacement + weight, data = mpgTrain)
summary(lm.mpg.disp_weight)
plot(lm.mpg.disp_weight)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement + mpgTrain$weight), col="red")

lm.mpg.disp_acc<- lm(mpg ~ displacement + acceleration, data = mpgTrain)
summary(lm.mpg.disp_acc)
plot(lm.mpg.disp_acc)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement + mpgTrain$acceleration), col="red")

lm.mpg.hp_weight <- lm(mpg ~ horsepower + weight, data = mpgTrain)
summary(lm.mpg.hp_weight)
plot(lm.mpg.hp_weight)
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower + mpgTrain$weight), col="red")

lm.mpg.hp_acc <- lm(mpg ~ horsepower + acceleration, data = mpgTrain)
summary(lm.mpg.hp_acc)
plot(lm.mpg.hp_acc)
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower + mpgTrain$acceleration), col="red")

lm.mpg.weight_acc <- lm(mpg ~ weight + acceleration, data = mpgTrain)
summary(lm.mpg.weight_acc)
plot(lm.mpg.weight_acc)
abline(lm(mpgTrain$mpg ~ mpgTrain$weight + mpgTrain$acceleration), col="red")


#Residuals vs. Individual Predictors:
res.disp_hp=residuals(lm.mpg.disp_hp)
res.disp_weight=residuals(lm.mpg.disp_weight)
res.disp_acc=residuals(lm.mpg.disp_acc)
res.hp_weight=residuals(lm.mpg.hp_weight)
res.hp_acc=residuals(lm.mpg.hp_acc)
res.weight_acc=residuals(lm.mpg.weight_acc)

#Plotting residuals vs fitted 2-variables combinations predictors:
par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(fitted(lm.mpg.disp_hp), res.disp_hp, xlab = "Displacement",
     ylab = "Residuals: Displacement_horsepower", main = "Residuals vs. Displacement Horsepower")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.disp_weight), res.disp_weight, xlab = "Displacement",
     ylab = "Residuals: Displacement+Acceration", main = "Residuals vs. Displacement + Acceleration")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.disp_acc), res.disp_acc, xlab = "Displacement+Weight",
     ylab = "Residuals: Displacement+weight", main = "Residuals vs. Displacement + Weight")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.hp_weight), res.hp_weight, xlab = "Horsepower",
     ylab = "Residuals: Horsepower+Weight", main = "Residuals vs. Horsepower")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.hp_acc), res.hp_acc, xlab = "Horsepower",
     ylab = "Residuals: Horsepower+Acceration", main = "Residuals vs. Horsepower")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.weight_acc), res.weight_acc, xlab = "Weight",
     ylab = "Residuals: Weight", main = "Residuals vs. Weight")
abline(h = 0, lty=3)


#Absolute Values of Residuals vs. 2-variables combination Predictors:
abs.res.disp_hp=abs(residuals(lm.mpg.disp_hp))
abs.res.disp_weight=abs(residuals(lm.mpg.disp_weight))
abs.res.disp_acc=abs(residuals(lm.mpg.disp_acc))
abs.res.hp_weight=abs(residuals(lm.mpg.hp_weight))
abs.res.hp_acc=abs(residuals(lm.mpg.hp_acc))
abs.res.weight_acc=abs(residuals(lm.mpg.weight_acc))


#Plotting Absolute Values of Residuals vs. 2-variables combination Predictors:
par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(fitted(lm.mpg.disp_hp), abs.res.disp_hp, xlab = "Displacement",
     ylab = "Residuals: Displacement_horsepower", main = "Residuals vs. Displacement Horsepower")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.disp_weight), abs.res.disp_weight, xlab = "Displacement",
     ylab = "Residuals: Displacement+Acceration", main = "Residuals vs. Displacement + Acceleration")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.disp_acc), abs.res.disp_acc, xlab = "Displacement+Weight",
     ylab = "Residuals: Displacement+weight", main = "Residuals vs. Displacement + Weight")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.hp_weight), abs.res.hp_weight, xlab = "Horsepower",
     ylab = "Residuals: Horsepower+Weight", main = "Residuals vs. Horsepower")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.hp_acc), abs.res.hp_acc, xlab = "Horsepower",
     ylab = "Residuals: Horsepower+Acceration", main = "Residuals vs. Horsepower")
abline(h = 0, lty=3)

plot(fitted(lm.mpg.weight_acc), abs.res.weight_acc, xlab = "Weight",
     ylab = "Residuals: Weight", main = "Residuals vs. Weight")
abline(h = 0, lty=3)


#Histograms of residuals:
hist(res.disp_hp)
hist(res.disp_weight)
hist(res.disp_acc)
hist(res.hp_weight)
hist(res.hp_acc)
hist(res.weight_acc)




#Evaluate models:
mpgTest <- na.omit(mpgTest)
best.guess <- mean(mpgTrain$mpg)
RMSE.baseline <- sqrt(mean((best.guess - mpgTest$mpg)^2)) #root mean squared error
MAE.baseline <- mean(abs(best.guess - mpgTest$mpg)) #mean absolute error
Rsquared.baseline <- 0 #assumes the proposed model does not improve over the 
#baseline (mean) model

#TWwo-Variables Model-Displacement + Horsepower:
new <- data.frame(displacement=mpgTest$displacement,horsepower = mpgTest$horsepower)
test.pred.disp_hp <- predict(lm.mpg.disp_hp, newdata = new)
mpgTest$pred_disp_hp <- test.pred.disp_hp
RMSE.disp_hp <- sqrt(mean((test.pred.disp_hp-mpgTest$mpg)^2))
MAE.dis_hp <- mean(abs(test.pred.disp_hp-mpgTest$mpg))
Rsquared.disp_hp <- summary(lm.mpg.disp_hp)$r.squared

#Two-Variables Model-Displacement + weight:
new <- data.frame(displacement=mpgTest$displacement,weight = mpgTest$weight)
test.pred.disp_weight <- predict(lm.mpg.disp_weight, newdata = new)
mpgTest$pred_disp_weight <- test.pred.disp_weight
RMSE.disp_weight <- sqrt(mean((test.pred.disp_weight-mpgTest$mpg)^2))
MAE.dis_weight <- mean(abs(test.pred.disp_weight-mpgTest$mpg))
Rsquared.disp_weight <- summary(lm.mpg.disp_weight)$r.squared

#Two-Variables Model-Displacement + acceleration:
new <- data.frame(displacement=mpgTest$displacement, acceleration = mpgTest$acceleration)
test.pred.disp_acc <- predict(lm.mpg.disp_acc, newdata = new)
mpgTest$pred_disp_acc <- test.pred.disp_acc
RMSE.disp_acc <- sqrt(mean((test.pred.disp_acc-mpgTest$mpg)^2))
MAE.dis_acct <- mean(abs(test.pred.disp_acc-mpgTest$mpg))
Rsquared.disp_acc <- summary(lm.mpg.disp_acc)$r.squared

#Two-Variables Model-horsepower + weight:
new <- data.frame(horsepower=mpgTest$horsepower, weight = mpgTest$weight)
test.pred.hp_weight <- predict(lm.mpg.hp_weight, newdata = new)
mpgTest$pred_hp_weight <- test.pred.hp_weight
RMSE.hp_weight <- sqrt(mean((test.pred.hp_weight-mpgTest$mpg)^2))
MAE.hp_weight <- mean(abs(test.pred.hp_weight-mpgTest$mpg))
Rsquared.hp_weight <- summary(lm.mpg.hp_weight)$r.squared

#Two-Variables Model-horsepower + acceleration:
new <- data.frame(horsepower=mpgTest$horsepower, acceleration = mpgTest$acceleration)
test.pred.hp_acc <- predict(lm.mpg.hp_acc, newdata = new)
mpgTest$pred_hp_acc <- test.pred.hp_acc
RMSE.hp_acc <- sqrt(mean((test.pred.hp_acc-mpgTest$mpg)^2))
MAE.hp_acc <- mean(abs(test.pred.hp_acc-mpgTest$mpg))
Rsquared.hp_acc <- summary(lm.mpg.hp_acc)$r.squared

#Two-Variables Model-weight + acceleration:
new <- data.frame(weight=mpgTest$weight, acceleration = mpgTest$acceleration)
test.pred.weight_acc <- predict(lm.mpg.weight_acc, newdata = new)
mpgTest$pred_weight_acc <- test.pred.weight_acc
RMSE.weight_acc <- sqrt(mean((test.pred.weight_acc-mpgTest$mpg)^2))
MAE.weight_acc <- mean(abs(test.pred.weight_acc-mpgTest$mpg))
Rsquared.weight_acc <- summary(lm.mpg.weight_acc)$r.squared


write.csv(mpgTest, file = "mpgTest-2var")

# Create a data frame with the error metrics for each method
accuracy <- data.frame(Model = c("Baseline","Displacement",
                                 "Horsepower", "Weight","Acceleration"),
                       RMSE   = c(RMSE.baseline,RMSE.weight_acc,RMSE.hp_acc,RMSE.hp_weight,
                                  RMSE.disp_weight,RMSE.disp_hp,RMSE.disp_weight),
                       MAE    = c(MAE.baseline,MAE.weight_acc,MAE.hp_acc,MAE.hp_weight,
                                  RMSE.disp_weight,RMSE.weight_acc,RMSE.disp_weight),
                       RSQ = c(Rsquared.baseline,Rsquared.weight_acc,Rsquared.hp_acc,
                               Rsquared.hp_weight, Rsquared.disp_weight,Rsquared.disp_hp,
                               Rsquared.disp_acc)) 

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy$RSQ <- round(accuracy$RSQ,2)
accuracy <- accuracy[order(accuracy$RMSE),]
accuracy

write.csv(accuracy, file = "accuracy-2var.csv")