############################################################################
#PROJECT 1
############################################################################

#INITIALIZING VARIABLES
rm(list = ls()) #clearing
setwd("/home/amcdowald/Downloads")
#pdf(file = "ProjectOnePlots.pdf", onefile = TRUE)

#INITIALIZING DATASET AND LABELING COLUMNS
mpg.data <- read.table('auto-mpg.data')
colnames(mpg.data) <-
  c(
    "mpg",
    "cylinders",
    "displacement",
    "horsepower",
    "weight",
    "acceleration",
    "model_year",
    "origin",
    "car_name"
  )
View(mpg.data)
summary(mpg.data)
nrow(mpg.data)
sapply(mpg.data, class)

#CLEANING DATA
#horsepower is listed as a categorical variable (factor), when it should be
#a numerical. This is because some of the values contain the text "?".
#remove all instances where horsepower="?", and transform into numerical type:

mpg.data$horsepower <- lapply(mpg.data$horsepower, function(x) {
  gsub("?", "", x)
})
mpg.data$horsepower <- as.numeric(mpg.data$horsepower)
attach(mpg.data)

#CREATING TEST AND TRAINING DATA
mpgTrain <- na.omit(mpg.data[0:300, ])
mpgTest <- na.omit(mpg.data[301:nrow(mpg.data), ])



#Examine shape of variable distributions:
library(reshape2)
library(ggplot2)
d <- melt(mpgTrain[, -c(2, 8, 9)])
d.2 <- melt(mpgTrain[, c(2, 8)])

ggplot(d, aes(x = value)) +
  facet_wrap( ~ variable, scales = "free_x") +
  geom_histogram()
#mpg, displacement, horsepower, and weight don't seem particularly normal.
#they appear more right tailed. Might be preferable to transform them (logs perhaps),
#but for now we will keep them as is.
#acceleration appears approximately normal.
#model year appears uniformly distributed - probably better as categorical

ggplot(d.2, aes(x = value)) +
  facet_wrap( ~ variable, scales = "free_x") +
  geom_histogram()
#cylanders,origin,and year are probabily better treated as categorical variables



#Let's go with mpg as the response variable, and use displacement,
#horsepower, weight, and acceleration as the independent variables.
############################################################################
#OUTLIER CHECK
############################################################################

outlierCheck <- function(dataObject, columnVariable) {
  #https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
  name <- deparse(substitute(columnVariable))
  var_name <- eval(substitute(columnVariable), eval(dataObject))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers", horizontal = TRUE)
  text(x = fivenum(var_name),
       labels = fivenum(var_name),
       y = 1.25)
  outlier_values <- boxplot.stats(var_name)$out
  mtext(
    paste("Outliers: ", paste(outlier_values, collapse = ", ")),
    cex = 0.9,
    side = 2 ,
    col = "red"
  )
  hist(var_name,
       main = "With outliers",
       xlab = NA,
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main = "Without outliers", horizontal = TRUE)
  text(x = fivenum(var_name),
       labels = fivenum(var_name),
       y = 1.25)
  hist(var_name,
       main = "Without outliers",
       xlab = NA,
       ylab = NA)
  title(paste("Outlier Check:", name), outer = TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "\n")
  cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name)) *
                                             100, 1), "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "\n")
  cat("Mean if we remove outliers:", round(m2, 2), "\n")
  response <-
    readline(prompt = "Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if (response == "y" | response == "yes") {
    dataObject[as.character(substitute(columnVariable))] <-
      invisible(var_name)
    assign(as.character(as.list(match.call())$dataObject), dataObject, envir = .GlobalEnv)
    cat("Outliers successfully removed", "\n")
    return(invisible(dataObject))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

#FINDING OUTLIERS
Acceleration = mpgTrain$acceleration
Displacement = mpgTrain$displacement
Horsepower = mpgTrain$horsepower
Weight = mpgTrain$weight
outlierCheck(mpgTrain, Acceleration)
outlierCheck(mpgTrain, Displacement)
outlierCheck(mpgTrain, Weight)
outlierCheck(mpgTrain, Horsepower)

#Remove rows with NA in colums
mpgTrain = mpgTrain[rowSums(is.na(mpgTrain)) == 0, ]

############################################################################
#MODEL 1
#TWO VARIABLE REGRESSION MODEL
#EXPLANATORY:MPG
#RESPONSE:Displacement, Horsepower, Weight, Acceleration
############################################################################

#plot individual predictors against response variable, Log.views
par(mar = c(2, 2, 2, 2))
par(mfrow = c(2, 2))
plot(
  mpgTrain$displacement,
  mpgTrain$mpg,
  xlab = "Displacement",
  ylab = "MPG",
  main = "MPG vs. Displacement"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement), col = "red")
plot(
  mpgTrain$horsepower,
  mpgTrain$mpg,
  xlab = "Horsepower",
  ylab = "MPG",
  main = "MPG vs. Horsepower"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower), col = "red")
plot(
  mpgTrain$weight,
  mpgTrain$mpg,
  xlab = "Weight",
  ylab = "MPG",
  main = "MPG vs. Weight"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$weight), col = "red")
plot(
  mpgTrain$acceleration,
  mpgTrain$mpg,
  xlab = "Acceleration",
  ylab = "MPG",
  main = "MPG vs. Acceleration"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$acceleration), col = "red")
#dev.off()

#run simple regressions on individual variables

lm.mpg.lgdisp <- lm(mpg ~ displacement, data = mpgTrain)
summary(lm.mpg.lgdisp)
plot(lm.mpg.lgdisp)
##########################################################################
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
res.disp = residuals(lm.mpg.lgdisp)
res.hp = residuals(lm.mpg.hp)
res.weight = residuals(lm.mpg.weight)
res.acc = residuals(lm.mpg.acc)

par(mar = c(3, 3, 3, 3))
par(mfrow = c(2, 2))
plot(
  mpgTrain$displacement,
  res.disp,
  xlab = "Displacement",
  ylab = "Residuals: Displacement",
  main = "Residuals vs. Displacement"
)
abline(h = 0, lty = 3)
plot(
  mpgTrain$horsepower,
  res.hp,
  xlab = "Horsepower",
  ylab = "Residuals: Horsepower",
  main = "Residuals vs. Horsepower"
)
abline(h = 0, lty = 3)
plot(
  mpgTrain$weight,
  res.weight,
  xlab = "Weight",
  ylab = "Residuals: Weight",
  main = "Residuals vs. Weight"
)
abline(h = 0, lty = 3)
plot(
  mpgTrain$acceleration,
  res.acc,
  xlab = "Acceleration",
  ylab = "Residuals: Acceleration",
  main = "Residuals vs. Acceleration"
)
abline(h = 0, lty = 3)


#Abs. Residuals vs. Individual Predictors:
ab.res.disp = abs(residuals(lm.mpg.lgdisp))
ab.res.hp = abs(residuals(lm.mpg.hp))
ab.res.weight = abs(residuals(lm.mpg.weight))
ab.res.acc = abs(residuals(lm.mpg.acc))

par(mar = c(3, 3, 3, 3))
par(mfrow = c(2, 2))
plot(
  mpgTrain$displacement,
  ab.res.disp,
  xlab = "Displacement",
  ylab = "Residuals: Displacement",
  main = "Abs. Residuals vs. Displacement"
)
abline(h = 0, lty = 3)
plot(
  mpgTrain$horsepower,
  ab.res.hp,
  xlab = "Horsepower",
  ylab = "Residuals: Horsepower",
  main = "Abs. Residuals vs. Horsepower"
)
abline(h = 0, lty = 3)
plot(
  mpgTrain$weight,
  ab.res.weight,
  xlab = "Weight",
  ylab = "Residuals: Weight",
  main = "Abs. Residuals vs. Weight"
)
abline(h = 0, lty = 3)
plot(
  mpgTrain$acceleration,
  ab.res.acc,
  xlab = "Acceleration",
  ylab = "Residuals: Acceleration",
  main = "Abs. Residuals vs. Acceleration"
)
abline(h = 0, lty = 3)

#Histograms of residuals:
hist(res.disp)
hist(res.hp)
hist(res.weight)
hist(res.acc)

#Evaluate models:
mpgTest <- na.omit(mpgTest)
best.guess <- mean(mpgTrain$mpg)
RMSE.baseline <-
  sqrt(mean((best.guess - mpgTest$mpg) ^ 2)) #root mean squared error
MAE.baseline <-
  mean(abs(best.guess - mpgTest$mpg)) #mean absolute error
Rsquared.baseline <-
  0 #assumes the proposed model does not improve over the
#baseline (mean) model

#Displacement Model:
new <- data.frame(displacement = mpgTest$displacement)
test.pred.disp <- predict(lm.mpg.lgdisp, newdata = new)
mpgTest$pred_disp <- test.pred.disp
RMSE.disp <- sqrt(mean((test.pred.disp - mpgTest$mpg) ^ 2))
MAE.disp <- mean(abs(test.pred.disp - mpgTest$mpg))
Rsquared.disp <- summary(lm.mpg.lgdisp)$r.squared

#Horsepower Model:
new <- data.frame(horsepower = mpgTest$horsepower)
test.pred.hp <- predict(lm.mpg.hp, newdata = new)
mpgTest$pred_hp <- test.pred.hp
RMSE.hp <- sqrt(mean((test.pred.hp - mpgTest$mpg) ^ 2))
MAE.hp <- mean(abs(test.pred.hp - mpgTest$mpg))
Rsquared.hp <- summary(lm.mpg.hp)$r.squared

#Weight Model:
new <- data.frame(weight = mpgTest$weight)
test.pred.wt <- predict(lm.mpg.weight, newdata = new)
mpgTest$pred_wt <- test.pred.wt
RMSE.wt <- sqrt(mean((test.pred.wt - mpgTest$mpg) ^ 2))
MAE.wt <- mean(abs(test.pred.wt - mpgTest$mpg))
Rsquared.wt <- summary(lm.mpg.weight)$r.squared

#Acceleration Model:
new <- data.frame(acceleration = mpgTest$acceleration)
test.pred.acc <- predict(lm.mpg.acc, newdata = new)
mpgTest$pred_acc <- test.pred.acc
RMSE.acc <- sqrt(mean((test.pred.acc - mpgTest$mpg) ^ 2))
MAE.acc <- mean(abs(test.pred.acc - mpgTest$mpg))
Rsquared.acc <- summary(lm.mpg.acc)$r.squared

write.csv(mpgTest, file = "mpgTest")

# Create a data frame with the error metrics for each method
accuracy <- data.frame(
  Model = c(
    "Baseline",
    "Displacement",
    "Horsepower",
    "Weight",
    "Acceleration"
  ),
  RMSE   = c(RMSE.baseline, RMSE.disp, RMSE.hp, RMSE.wt, RMSE.acc),
  MAE    = c(MAE.baseline, MAE.disp, MAE.hp, MAE.wt, RMSE.acc),
  RSQ = c(
    Rsquared.baseline,
    Rsquared.disp,
    Rsquared.hp,
    Rsquared.wt,
    Rsquared.acc
  )
)

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE, 2)
accuracy$MAE <- round(accuracy$MAE, 2)
accuracy$RSQ <- round(accuracy$RSQ, 2)
accuracy <- accuracy[order(accuracy$RMSE), ]
accuracy

write.csv(accuracy, file = "accuracy.csv")
################################################################
#MODEL 2
#THREE VARIABLE REGRESSION MODEL
#EXPLANATORY:MPG
#RESPONSE:Displacement + Weight, Displacement + Horsepower,
#Displacement + Acceleration, Horsepower + Weight,
#Horsepower + Acceleration, Weight + Acceleration
#run simple regressions on combination of 2 variables on 6 cases:
################################################################
par(mar = c(2, 2, 2, 2))
par(mfrow = c(2, 2))
plot(
  mpgTrain$displacement,
  mpgTrain$mpg,
  xlab = "Displacement",
  ylab = "MPG",
  main = "MPG vs. Displacement"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement), col = "red")
plot(
  mpgTrain$horsepower,
  mpgTrain$mpg,
  xlab = "Horsepower",
  ylab = "MPG",
  main = "MPG vs. Horsepower"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower), col = "red")
plot(
  mpgTrain$weight,
  mpgTrain$mpg,
  xlab = "Weight",
  ylab = "MPG",
  main = "MPG vs. Weight"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$weight), col = "red")
plot(
  mpgTrain$acceleration,
  mpgTrain$mpg,
  xlab = "Acceleration",
  ylab = "MPG",
  main = "MPG vs. Acceleration"
)
abline(lm(mpgTrain$mpg ~ mpgTrain$acceleration), col = "red")
#dev.off()

#run simple regressions on combination of 2 variables on 6 cases:
par(mar = c(2, 2, 2, 2))
par(mfrow = c(2, 2))
lm.mpg.disp_hp <-
  lm(mpg ~ displacement + horsepower, data = mpgTrain)
summary(lm.mpg.disp_hp)
plot(lm.mpg.disp_hp)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement + mpgTrain$horsepower),
       col = "red")

lm.mpg.disp_weight <-
  lm(mpg ~ displacement + weight, data = mpgTrain)
summary(lm.mpg.disp_weight)
plot(lm.mpg.disp_weight)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement + mpgTrain$weight),
       col = "red")

lm.mpg.disp_acc <-
  lm(mpg ~ displacement + acceleration, data = mpgTrain)
summary(lm.mpg.disp_acc)
plot(lm.mpg.disp_acc)
abline(lm(mpgTrain$mpg ~ mpgTrain$displacement + mpgTrain$acceleration),
       col = "red")

lm.mpg.hp_weight <- lm(mpg ~ horsepower + weight, data = mpgTrain)
summary(lm.mpg.hp_weight)
plot(lm.mpg.hp_weight)
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower + mpgTrain$weight), col = "red")

lm.mpg.hp_acc <-
  lm(mpg ~ horsepower + acceleration, data = mpgTrain)
summary(lm.mpg.hp_acc)
plot(lm.mpg.hp_acc)
abline(lm(mpgTrain$mpg ~ mpgTrain$horsepower + mpgTrain$acceleration),
       col = "red")

lm.mpg.weight_acc <-
  lm(mpg ~ weight + acceleration, data = mpgTrain)
summary(lm.mpg.weight_acc)
plot(lm.mpg.weight_acc)
abline(lm(mpgTrain$mpg ~ mpgTrain$weight + mpgTrain$acceleration),
       col = "red")


#Residuals vs. Individual Predictors:
res.disp_hp = residuals(lm.mpg.disp_hp)
res.disp_weight = residuals(lm.mpg.disp_weight)
res.disp_acc = residuals(lm.mpg.disp_acc)
res.hp_weight = residuals(lm.mpg.hp_weight)
res.hp_acc = residuals(lm.mpg.hp_acc)
res.weight_acc = residuals(lm.mpg.weight_acc)

#Plotting residuals vs fitted 2-variables combinations predictors:
par(mar = c(3, 3, 3, 3))
par(mfrow = c(2, 2))
plot(
  fitted(lm.mpg.disp_hp),
  res.disp_hp,
  xlab = "Fitted Values (Displacement Horsepower)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_weight),
  res.disp_weight,
  xlab = "Fitted Values (Displacement Acceleration)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_acc),
  res.disp_acc,
  xlab = "Fitted Values (Displacement  Weight)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.hp_weight),
  res.hp_weight,
  xlab = "Fitted Values (Horsepower Weight)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.hp_acc),
  res.hp_acc,
  xlab = "Fitted Values(Horsepower Acceleration)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.weight_acc),
  res.weight_acc,
  xlab = "Fitted Values (Weight Acceleration)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)


#Absolute Values of Residuals vs. 2-variables combination Predictors:
abs.res.disp_hp = abs(residuals(lm.mpg.disp_hp))
abs.res.disp_weight = abs(residuals(lm.mpg.disp_weight))
abs.res.disp_acc = abs(residuals(lm.mpg.disp_acc))
abs.res.hp_weight = abs(residuals(lm.mpg.hp_weight))
abs.res.hp_acc = abs(residuals(lm.mpg.hp_acc))
abs.res.weight_acc = abs(residuals(lm.mpg.weight_acc))


#Plotting Absolute Values of Residuals vs. 2-variables combination Predictors:
par(mar = c(3, 3, 3, 3))
par(mfrow = c(2, 2))
plot(
  fitted(lm.mpg.disp_hp),
  abs.res.disp_hp,
  xlab = "Fitted Values (Displacement Horsepower)",
  ylab = "Absolute Residuals", 
  main = "Absolute Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_weight),
  abs.res.disp_weight,
  xlab = "Fitted Values (Displacement Weight)",
  ylab = "Absolute Residuals", 
  main = "Absolute Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_acc),
  abs.res.disp_acc,
  xlab = "Fitted Values(Displacement Acceleration)",
  ylab = "Absolute Residuals", 
  main = "Absolute Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.hp_weight),
  abs.res.hp_weight,
  xlab = "Fitted Values (HP Weight)",
  ylab = "Absolute Residuals", 
  main = "Absolute Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.hp_acc),
  abs.res.hp_acc,
  xlab = "Fitted Values(Horsepower Acceleration)",
  ylab = "Absolute Residuals", 
  main = "Absolute Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.weight_acc),
  abs.res.weight_acc,
  xlab = "Fitted Values(Weight Acceleration)",
  ylab = "Absolute Residuals", 
  main = "Absolute Residuals vs. Fitted Values")
abline(h = 0, lty = 3)


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
RMSE.baseline <-
  sqrt(mean((best.guess - mpgTest$mpg) ^ 2)) #root mean squared error
MAE.baseline <-
  mean(abs(best.guess - mpgTest$mpg)) #mean absolute error
Rsquared.baseline <-
  0 #assumes the proposed model does not improve over the
#baseline (mean) model

#TWwo-Variables Model-Displacement + Horsepower:
new <-
  data.frame(displacement = mpgTest$displacement,
             horsepower = mpgTest$horsepower)
test.pred.disp_hp <- predict(lm.mpg.disp_hp, newdata = new)
mpgTest$pred_disp_hp <- test.pred.disp_hp
RMSE.disp_hp <- sqrt(mean((test.pred.disp_hp - mpgTest$mpg) ^ 2))
MAE.dis_hp <- mean(abs(test.pred.disp_hp - mpgTest$mpg))
Rsquared.disp_hp <- summary(lm.mpg.disp_hp)$r.squared

#Two-Variables Model-Displacement + weight:
new <-
  data.frame(displacement = mpgTest$displacement,
             weight = mpgTest$weight)
test.pred.disp_weight <- predict(lm.mpg.disp_weight, newdata = new)
mpgTest$pred_disp_weight <- test.pred.disp_weight
RMSE.disp_weight <-
  sqrt(mean((test.pred.disp_weight - mpgTest$mpg) ^ 2))
MAE.dis_weight <- mean(abs(test.pred.disp_weight - mpgTest$mpg))
Rsquared.disp_weight <- summary(lm.mpg.disp_weight)$r.squared

#Two-Variables Model-Displacement + acceleration:
new <-
  data.frame(displacement = mpgTest$displacement,
             acceleration = mpgTest$acceleration)
test.pred.disp_acc <- predict(lm.mpg.disp_acc, newdata = new)
mpgTest$pred_disp_acc <- test.pred.disp_acc
RMSE.disp_acc <- sqrt(mean((test.pred.disp_acc - mpgTest$mpg) ^ 2))
MAE.dis_acct <- mean(abs(test.pred.disp_acc - mpgTest$mpg))
Rsquared.disp_acc <- summary(lm.mpg.disp_acc)$r.squared

#Two-Variables Model-horsepower + weight:
new <-
  data.frame(horsepower = mpgTest$horsepower, weight = mpgTest$weight)
test.pred.hp_weight <- predict(lm.mpg.hp_weight, newdata = new)
mpgTest$pred_hp_weight <- test.pred.hp_weight
RMSE.hp_weight <- sqrt(mean((test.pred.hp_weight - mpgTest$mpg) ^ 2))
MAE.hp_weight <- mean(abs(test.pred.hp_weight - mpgTest$mpg))
Rsquared.hp_weight <- summary(lm.mpg.hp_weight)$r.squared

#Two-Variables Model-horsepower + acceleration:
new <-
  data.frame(horsepower = mpgTest$horsepower,
             acceleration = mpgTest$acceleration)
test.pred.hp_acc <- predict(lm.mpg.hp_acc, newdata = new)
mpgTest$pred_hp_acc <- test.pred.hp_acc
RMSE.hp_acc <- sqrt(mean((test.pred.hp_acc - mpgTest$mpg) ^ 2))
MAE.hp_acc <- mean(abs(test.pred.hp_acc - mpgTest$mpg))
Rsquared.hp_acc <- summary(lm.mpg.hp_acc)$r.squared

#Two-Variables Model-weight + acceleration:
new <-
  data.frame(weight = mpgTest$weight,
             acceleration = mpgTest$acceleration)
test.pred.weight_acc <- predict(lm.mpg.weight_acc, newdata = new)
mpgTest$pred_weight_acc <- test.pred.weight_acc
RMSE.weight_acc <- sqrt(mean((test.pred.weight_acc - mpgTest$mpg) ^ 2))
MAE.weight_acc <- mean(abs(test.pred.weight_acc - mpgTest$mpg))
Rsquared.weight_acc <- summary(lm.mpg.weight_acc)$r.squared


write.csv(mpgTest, file = "mpgTest-2var")

# Create a data frame with the error metrics for each method
accuracy <- data.frame(
  Model = c(
    "Baseline",
    "Displacement",
    "Horsepower",
    "Weight",
    "Acceleration"
  ),
  RMSE   = c(
    RMSE.baseline,
    RMSE.weight_acc,
    RMSE.hp_acc,
    RMSE.hp_weight,
    RMSE.disp_weight,
    RMSE.disp_hp,
    RMSE.disp_weight
  ),
  MAE    = c(
    MAE.baseline,
    MAE.weight_acc,
    MAE.hp_acc,
    MAE.hp_weight,
    RMSE.disp_weight,
    RMSE.weight_acc,
    RMSE.disp_weight
  ),
  RSQ = c(
    Rsquared.baseline,
    Rsquared.weight_acc,
    Rsquared.hp_acc,
    Rsquared.hp_weight,
    Rsquared.disp_weight,
    Rsquared.disp_hp,
    Rsquared.disp_acc
  )
)

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE, 2)
accuracy$MAE <- round(accuracy$MAE, 2)
accuracy$RSQ <- round(accuracy$RSQ, 2)
accuracy <- accuracy[order(accuracy$RMSE), ]
accuracy

write.csv(accuracy, file = "accuracy-2var.csv")

################################################################
#MODEL 3
#EXPLANATORY:MPG
#RESPONSE:Displacement + Horsepower + Weight,
#Displacement + Horsepower + Acceleration,
#Displacement + Weight + Acceleration,
#Horsepower + Weight + Acceleration
#run simple regression on combination of 3 variables on 4 cases
################################################################


lm.mpg.disp_hp_weight <-
  lm(mpg ~ displacement + horsepower + weight, data = mpgTrain)
summary(lm.mpg.disp_hp_weight)
plot(lm.mpg.disp_hp_weight)
par(mfrow = c(2, 2))
termplot(lm.mpg.disp_hp_weight, partial.resid = TRUE)

lm.mpg.disp_hp_acceleration <-
  lm(mpg ~ displacement + horsepower + acceleration, data = mpgTrain)
summary(lm.mpg.disp_hp_acceleration)
plot(lm.mpg.disp_hp_acceleration)
par(mfrow = c(2, 2))
termplot(lm.mpg.disp_hp_acceleration, partial.resid = TRUE)

lm.mpg.disp_weight_acceleration <-
  lm(mpg ~ displacement + weight + acceleration, data = mpgTrain)
summary(lm.mpg.disp_weight_acceleration)
plot(lm.mpg.disp_weight_acceleration)
par(mfrow = c(2, 2))
termplot(lm.mpg.disp_weight_acceleration, partial.resid = TRUE)

lm.mpg.hp_weight_acceleration <-
  lm(mpg ~ horsepower + weight + acceleration, data = mpgTrain)
summary(lm.mpg.hp_weight_acceleration)
plot(lm.mpg.hp_weight_acceleration)
par(mfrow = c(2, 2))
termplot(lm.mpg.hp_weight_acceleration, partial.resid = TRUE)

#Residuals vs. Individual Predictors:
res.disp_hp_weight = residuals(lm.mpg.disp_hp_weight)
res.disp_hp_acceleration = residuals(lm.mpg.disp_hp_acceleration)
res.disp_weight_acceleration = residuals(lm.mpg.disp_weight_acceleration)
res.hp_weight_acceleration = residuals(lm.mpg.hp_weight_acceleration)


#Plotting residuals vs fitted 3-variables combinations predictors:

plot(
  fitted(lm.mpg.disp_hp_weight),
  res.disp_hp_weight,
  xlab = "Displacement HP Weight",
  ylab = "Residuals: Displacement_Horsepower_Weight",
  main = "Residuals vs. Displacement Horsepower Weight"
)
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_hp_acceleration),
  res.disp_hp_acceleration,
  xlab = "Displacement HP Acceleration",
  ylab = "Residuals: Displacement_Horsepower_Acceleration",
  main = "Residuals vs. Displacement HP Acceleration"
)
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_weight_acceleration),
  res.disp_weight_acceleration,
  xlab = "Displacement Weight Acceleration",
  ylab = "Residuals: Displacement_Weight_Acceleration",
  main = "Residuals vs. Displacement Weight Acceleration"
)
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.hp_weight_acceleration),
  res.hp_weight_acceleration,
  xlab = "HP Weight Acceleration",
  ylab = "Residuals: HP_Weight_Acceleration",
  main = "Residuals vs. HP Weight Acceleration"
)
abline(h = 0, lty = 3)

#Absolute Values of Residuals vs. 3-variables combination Predictors:
abs.res.disp_hp_weight = abs(residuals(lm.mpg.disp_hp_weight))
abs.res.disp_hp_acceleration = abs(residuals(lm.mpg.disp_hp_acceleration))
abs.res.disp_weight_acceleration = abs(residuals(lm.mpg.disp_weight_acceleration))
abs.res.hp_weight_acceleration = abs(residuals(lm.mpg.hp_weight_acceleration))

#Plotting Absolute Values of Residuals vs. 3-variables combination Predictors:
plot(
  fitted(lm.mpg.disp_hp_weight),
  abs.res.disp_hp_weight,
  xlab = "Displacement HP Weight",
  ylab = "Residuals: Displacement HP Weight",
  main = "Residuals vs. Displacement HP Weight"
)
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_hp_acceleration),
  abs.res.disp_hp_acceleration,
  xlab = "Displacement HP Acceleration",
  ylab = "Residuals: Displacement HP Acceleration",
  main = "Residuals vs. Displacement HP Acceleration"
)
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.disp_weight_acceleration),
  abs.res.disp_weight_acceleration,
  xlab = "Displacement Weight Acceleration",
  ylab = "Residuals: Displacement Weight Acceleration",
  main = "Residuals vs. Displacement Weight Acceleration"
)
abline(h = 0, lty = 3)

plot(
  fitted(lm.mpg.hp_weight_acceleration),
  abs.res.hp_weight_acceleration,
  xlab = "HP Weight Acceleration",
  ylab = "Residuals: HP Weight Acceleration",
  main = "Residuals vs. HP Weight Acceleration"
)
abline(h = 0, lty = 3)

#Histograms of residuals:
hist(res.disp_hp_weight)
hist(res.disp_hp_acceleration)
hist(res.disp_weight_acceleration)
hist(res.hp_weight_acceleration)
#Evaluate models:
mpgTest <- na.omit(mpgTest)
best.guess <- mean(mpgTrain$mpg)
RMSE.baseline <-
  sqrt(mean((best.guess - mpgTest$mpg) ^ 2)) #root mean squared error
MAE.baseline <-
  mean(abs(best.guess - mpgTest$mpg)) #mean absolute error
Rsquared.baseline <- 0

#Three-Variables Model-Displacement + Horsepower + Weight:
new <-
  data.frame(
    displacement = mpgTest$displacement,
    horsepower = mpgTest$horsepower,
    weight = mpgTest$weight
  )
test.pred.disp_hp_weight <-
  predict(lm.mpg.disp_hp_weight, newdata = new)
mpgTest$pred_disp_hp_weight <- test.pred.disp_hp_weight
RMSE.disp_hp_weight <-
  sqrt(mean((test.pred.disp_hp_weight - mpgTest$mpg) ^ 2))
MAE.dis_hp_weight <-
  mean(abs(test.pred.disp_hp_weight - mpgTest$mpg))
Rsquared.disp_hp_weight <- summary(lm.mpg.disp_hp_weight)$r.squared

#Three-Variables Model-Displacement + Horsepower + Acceleration:
new <-
  data.frame(
    displacement = mpgTest$displacement,
    horsepower = mpgTest$horsepower,
    acceleration = mpgTest$acceleration
  )
test.pred.disp_hp_acceleration <-
  predict(lm.mpg.disp_hp_acceleration, newdata = new)
mpgTest$pred_disp_hp_acceleration <- test.pred.disp_hp_acceleration
RMSE.disp_hp_acceleration <-
  sqrt(mean((
    test.pred.disp_hp_acceleration - mpgTest$mpg
  ) ^ 2))
Rsquared.disp_hp_acceleration <-
  summary(lm.mpg.disp_hp_acceleration)$r.squared


#Three-Variables Model-Displacement + Weight + Acceleration:
new <-
  data.frame(
    displacement = mpgTest$displacement,
    weight = mpgTest$weight,
    acceleration = mpgTest$acceleration
  )
test.pred.disp_weight_acceleration <-
  predict(lm.mpg.disp_weight_acceleration, newdata = new)
mpgTest$pred_disp_weight_acceleration <-
  test.pred.disp_weight_acceleration
RMSE.disp_weight_acceleration <-
  sqrt(mean((
    test.pred.disp_weight_acceleration - mpgTest$mpg
  ) ^ 2))
Rsquared.disp_weight_acceleration <-
  summary(lm.mpg.disp_weight_acceleration)$r.squared

#Three-Variables Model-HP + Weight + Acceleration:
new <-
  data.frame(
    horsepower = mpgTest$horsepower,
    weight = mpgTest$weight,
    acceleration = mpgTest$acceleration
  )
test.pred.hp_weight_acceleration <-
  predict(lm.mpg.hp_weight_acceleration, newdata = new)
mpgTest$pred_hp_weight_acceleration <-
  test.pred.hp_weight_acceleration
RMSE.hp_weight_acceleration <-
  sqrt(mean((
    test.pred.hp_weight_acceleration - mpgTest$mpg
  ) ^ 2))
Rsquared.hp_weight_acceleration <-
  summary(lm.mpg.hp_weight_acceleration)$r.squared


##########################################################################
#MODEL 4
##########################################################################

#run multiple regression using 4 variables: displacement, hp, wt, acceleration

lm.mpg.all <-
  lm(mpg ~ displacement + horsepower + weight + acceleration, data = mpgTrain)
summary(lm.mpg.all)
plot(lm.mpg.all)


#Residual Plots
#

#Residuals:
res.all = residuals(lm.mpg.all)


#Abs. Residuals:
ab.res = abs(residuals(lm.mpg.all))


#Histograms of residuals:
hist(res.all)
hist(ab.res)

#Residuals vs. Fitted Values
fit <- fitted(lm.mpg.all)
plot(fit,
     ab.res,
     xlab = "Fitted Values",
     ylab = "Abs. Residuals",
     main = "Abs. Residuals vs. Fitted Values")
plot(fit,
     res.all,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")


#Evaluate model:
mpgTest <- na.omit(mpgTest)
best.guess <- mean(mpgTrain$mpg)
RMSE.baseline <-
  sqrt(mean((best.guess - mpgTest$mpg) ^ 2)) #root mean squared error
MAE.baseline <-
  mean(abs(best.guess - mpgTest$mpg)) #mean absolute error
Rsquared.baseline <-
  0 #assumes the proposed model does not improve over the
#baseline (mean) model

#Model:
new <-
  data.frame(
    displacement = mpgTest$displacement,
    horsepower = mpgTest$horsepower,
    weight = mpgTest$weight,
    acceleration = mpgTest$acceleration
  )
test.pred <- predict(lm.mpg.all, newdata = new)
mpgTest$pred <- test.pred
RMSE.model <- sqrt(mean((test.pred - mpgTest$mpg) ^ 2))
MAE.model <- mean(abs(test.pred - mpgTest$mpg))
Rsquared.model <- summary(lm.mpg.all)$r.squared

write.csv(mpgTest, file = "mpgTest")

# Create a data frame with the error metrics for each method
accuracy <- data.frame(
  Model = c("Baseline", "Model"),
  RMSE   = c(RMSE.baseline, RMSE.model),
  MAE    = c(MAE.baseline, MAE.model),
  RSQ = c(Rsquared.baseline, Rsquared.model)
)

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE, 2)
accuracy$MAE <- round(accuracy$MAE, 2)
accuracy$RSQ <- round(accuracy$RSQ, 2)
accuracy <- accuracy[order(accuracy$RMSE), ]
accuracy


write.csv(accuracy, file = "accuracy_all.csv")


