#
#
rm(list = ls())
setwd("~/Desktop/SPU/Intro_to_data_science_F2016/Project_1")
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

#run simple regressions on individual variables

lm.mpg.lgdisp <- lm(mpg ~ displacement, data = mpgTrain)
summary(lm.mpg.lgdisp)
plot(lm.mpg.lgdisp)

lm.mpg.hp <- lm(mpg ~ horsepower, data = mpgTrain)
summary(lm.mpg.hp)
plot(lm.mpg.hp)

lm.mpg.weight <- lm(mpg ~ weight, data = mpgTrain)
summary(lm.mpg.weight)
plot(lm.mpg.weight)

lm.mpg.acc <- lm(mpg ~ acceleration, data = mpgTrain)
summary(lm.mpg.acc)
plot(lm.mpg.acc)

#Residual Plots
#

#Residuals vs. Individual Predictors:
res.disp=residuals(lm.mpg.lgdisp)
res.hp=residuals(lm.mpg.hp)
res.weight=residuals(lm.mpg.weight)
res.acc=residuals(lm.mpg.acc)

par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(mpgTrain$displacement, res.disp, xlab = "Displacement",
     ylab = "Residuals: Displacement", main = "Residuals vs. Displacement")
abline(h = 0, lty=3)
plot(mpgTrain$horsepower, res.hp, xlab = "Horsepower",
     ylab = "Residuals: Horsepower", main = "Residuals vs. Horsepower")
abline(h = 0, lty=3)
plot(mpgTrain$weight, res.weight, xlab = "Weight",
     ylab = "Residuals: Weight", main = "Residuals vs. Weight")
abline(h = 0, lty=3)
plot(mpgTrain$acceleration, res.acc, xlab = "Acceleration",
     ylab = "Residuals: Acceleration", main = "Residuals vs. Acceleration")
abline(h = 0, lty=3)


#Abs. Residuals vs. Individual Predictors:
ab.res.disp= abs(residuals(lm.mpg.lgdisp))
ab.res.hp=abs(residuals(lm.mpg.hp))
ab.res.weight=abs(residuals(lm.mpg.weight))
ab.res.acc=abs(residuals(lm.mpg.acc))

par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(mpgTrain$displacement, ab.res.disp, xlab = "Displacement",
     ylab = "Residuals: Displacement", main = "Abs. Residuals vs. Displacement")
abline(h = 0, lty=3)
plot(mpgTrain$horsepower, ab.res.hp, xlab = "Horsepower",
     ylab = "Residuals: Horsepower", main = "Abs. Residuals vs. Horsepower")
abline(h = 0, lty=3)
plot(mpgTrain$weight, ab.res.weight, xlab = "Weight",
     ylab = "Residuals: Weight", main = "Abs. Residuals vs. Weight")
abline(h = 0, lty=3)
plot(mpgTrain$acceleration, ab.res.acc, xlab = "Acceleration",
     ylab = "Residuals: Acceleration", main = "Abs. Residuals vs. Acceleration")
abline(h = 0, lty=3)

#Histograms of residuals:
hist(res.disp)
hist(res.hp)
hist(res.weight)
hist(res.acc)

#Evaluate models:
mpgTest <- na.omit(mpgTest)
best.guess <- mean(mpgTrain$mpg)
RMSE.baseline <- sqrt(mean((best.guess-mpgTest$mpg)^2)) #root mean squared error
MAE.baseline <- mean(abs(best.guess-mpgTest$mpg)) #mean absolute error
Rsquared.baseline <- 0 #assumes the proposed model does not improve over the 
#baseline (mean) model

#Displacement Model:
new <- data.frame(displacement = mpgTest$displacement)
test.pred.disp <- predict(lm.mpg.lgdisp, newdata = new)
mpgTest$pred_disp <- test.pred.disp
RMSE.disp <- sqrt(mean((test.pred.disp-mpgTest$mpg)^2))
MAE.disp <- mean(abs(test.pred.disp-mpgTest$mpg))
Rsquared.disp <- summary(lm.mpg.lgdisp)$r.squared

#Horsepower Model:
new <- data.frame(horsepower = mpgTest$horsepower)
test.pred.hp <- predict(lm.mpg.hp, newdata = new)
mpgTest$pred_hp <- test.pred.hp
RMSE.hp <- sqrt(mean((test.pred.hp-mpgTest$mpg)^2))
MAE.hp <- mean(abs(test.pred.hp-mpgTest$mpg))
Rsquared.hp <- summary(lm.mpg.hp)$r.squared

#Weight Model:
new <- data.frame(weight = mpgTest$weight)
test.pred.wt <- predict(lm.mpg.weight, newdata = new)
mpgTest$pred_wt <- test.pred.wt
RMSE.wt <- sqrt(mean((test.pred.wt-mpgTest$mpg)^2))
MAE.wt <- mean(abs(test.pred.wt-mpgTest$mpg))
Rsquared.wt <- summary(lm.mpg.weight)$r.squared

#Acceleration Model:
new <- data.frame(acceleration = mpgTest$acceleration)
test.pred.acc <- predict(lm.mpg.acc, newdata = new)
mpgTest$pred_acc <- test.pred.acc
RMSE.acc <- sqrt(mean((test.pred.acc-mpgTest$mpg)^2))
MAE.acc <- mean(abs(test.pred.acc-mpgTest$mpg))
Rsquared.acc <- summary(lm.mpg.acc)$r.squared

write.csv(mpgTest, file = "mpgTest")

# Create a data frame with the error metrics for each method
accuracy <- data.frame(Model = c("Baseline","Displacement",
                                 "Horsepower", "Weight","Acceleration"),
                       RMSE   = c(RMSE.baseline,RMSE.disp,RMSE.hp,RMSE.wt,RMSE.acc),
                       MAE    = c(MAE.baseline,MAE.disp,MAE.hp,MAE.wt,RMSE.acc),
                       RSQ = c(Rsquared.baseline,Rsquared.disp,Rsquared.hp,Rsquared.wt,
                               Rsquared.acc)) 

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy$RSQ <- round(accuracy$RSQ,2)
accuracy <- accuracy[order(accuracy$RMSE),]
accuracy

write.csv(accuracy, file = "accuracy.csv")