setwd("/home/amcdowald/Downloads")
heart.data <- read.csv('Heart.csv')
heartTrain <- na.omit(heart.data[0:250,])
#heartTrain <- head(heart.data, 250)
View(heartTrain)
library(reshape2)
library(ggplot2)
summary(heartTrain)
heartTrain$AHD <- as.numeric(heartTrain$AHD)
heartTrain$ChestPain <- as.numeric(heartTrain$ChestPain)
heartTrain$Thal <- as.numeric(heartTrain$Thal)
#AHD.f<-factor(heartTrain$AHD) 
#Yes=2,No=1
#ChestPain
#x1=typical=4
#x2=asymptomatic=1
#x4=nonanginal=2
#x5=nontypical=3
#Thal
#x1=fixed=1
#x2=normal=2
#x3=reversible=3
#heartTrain$AHD<-sample(0:1, 250, replace=TRUE)
#View(heartTrain)
#heartTrain$ChestPain <- sample(0:3, 250, replace=TRUE)

#Age + Sex + ChestPain + MaxHR + ExAng + Oldpeak + Slope + Ca + Thal

#Age + Sex + Chestpain + MaxHR
glm.AD.Sx.Cn.MR <- glm(AHD ~ Age + Slope + Sex + MaxHR , data = heartTrain)
summary(glm.AD.Sx.Cn.MR)

res.AD.Sx.Cn.MR = residuals(glm.AD.Sx.Cn.MR)

plot(
  fitted(lm.mpg.disp_hp),
  res.AD.Sx.Cn.MR,
  xlab = "Fitted Values (Age + Slope + Sex + MaxHR)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

#Age + Sex + Chestpain + ExAng
#Age + Sex + Chestpain + Oldpeak
#Age + Sex + Chestpain +Slope
#Age + Sex + Chestpain + Ca
#Age + Sex + Chestpain +Thal

#Age + Sex +  

#ExAng + Oldpeak + Slope + Ca
glm.AD.Eg.Ok.Se.Ca <- glm(AHD ~ ExAng + Oldpeak + Slope + Ca , data = heartTrain)

res.AD.Eg.Ok.Se.Ca = residuals(glm.AD.Eg.Ok.Se.Ca)

plot(
  fitted(glm.AD.Eg.Ok.Se.Ca),
  res.AD.Eg.Ok.Se.Ca,
  xlab = "Fitted Values (ExAng + Oldpeak + Slope + Ca)",
  ylab = "Residuals", 
  main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 3)

for (val in x) {
  if(val %% 2 == 0)  count = count+1
}
#ChestPain + MaxHR + ExAng + Oldpeak
four <- c( #4
  "Age+Sex+ChestPain+MaxHR",
  "Age+Sex+ChestPain+ExAng",
  "Age+Sex+ChestPain+Oldpeak",
  "Age+Sex+ChestPain+Slope",
  "Age+Sex+ChestPain+Ca",
  "Age+Sex+ChestPain+Thal",
  "Age+Sex+MaxHR+ExAng",
  "Age+Sex+MaxHR+Oldpeak",
  "Age+Sex+MaxHR+Slope",
  "Age+Sex+MaxHR+Ca",
  "Age+Sex+MaxHR+Thal",
  "Age+Sex+ExAng+Oldpeak",
  "Age+Sex+ExAng+Slope",
  "Age+Sex+ExAng+Ca",
  "Age+Sex+ExAng+Thal",
  "Age+Sex+Oldpeak+Slope",
  "Age+Sex+Oldpeak+Ca",
  "Age+Sex+Oldpeak+Thal",
  "Age+Sex+Slope+Ca",
  "Age+Sex+Slope+Thal",
  "Age+Sex+Ca+Thal",
  "Age+ChestPain+MaxHR+ExAng",
  "Age+ChestPain+MaxHR+Oldpeak",
  "Age+ChestPain+MaxHR+Slope",
  "Age+ChestPain+MaxHR+Ca",
  "Age+ChestPain+MaxHR+Thal",
  "Age+ChestPain+ExAng+Oldpeak",
  "Age+ChestPain+ExAng+Slope",
  "Age+ChestPain+ExAng+Ca",
  "Age+ChestPain+ExAng+Thal",
  "Age+ChestPain+Oldpeak+Slope",
  "Age+ChestPain+Oldpeak+Ca",
  "Age+ChestPain+Oldpeak+Thal",
  "Age+ChestPain+Slope+Ca",
  "Age+ChestPain+Slope+Thal",
  "Age+ChestPain+Ca+Thal",
  "Age+MaxHR+ExAng+Oldpeak",
  "Age+MaxHR+ExAng+Slope",
  "Age+MaxHR+ExAng+Ca",
  "Age+MaxHR+ExAng+Thal",
  "Age+MaxHR+Oldpeak+Slope",
  "Age+MaxHR+Oldpeak+Ca",
  "Age+MaxHR+Oldpeak+Thal",
  "Age+MaxHR+Slope+Ca",
  "Age+MaxHR+Slope+Thal",
  "Age+MaxHR+Ca+Thal",
  "Age+ExAng+Oldpeak+Slope",
  "Age+ExAng+Oldpeak+Ca",
  "Age+ExAng+Oldpeak+Thal",
  "Age+ExAng+Slope+Ca",
  "Age+ExAng+Slope+Thal",
  "Age+ExAng+Ca+Thal",
  "Age+Oldpeak+Slope+Ca",
  "Age+Oldpeak+Slope+Thal",
  "Age+Oldpeak+Ca+Thal",
  "Age+Slope+Ca+Thal",
  "Sex+ChestPain+MaxHR+ExAng",
  "Sex+ChestPain+MaxHR+Oldpeak",
  "Sex+ChestPain+MaxHR+Slope",
  "Sex+ChestPain+MaxHR+Ca",
  "Sex+ChestPain+MaxHR+Thal",
  "Sex+ChestPain+ExAng+Oldpeak",
  "Sex+ChestPain+ExAng+Slope",
  "Sex+ChestPain+ExAng+Ca",
  "Sex+ChestPain+ExAng+Thal",
  "Sex+ChestPain+Oldpeak+Slope",
  "Sex+ChestPain+Oldpeak+Ca",
  "Sex+ChestPain+Oldpeak+Thal",
  "Sex+ChestPain+Slope+Ca",
  "Sex+ChestPain+Slope+Thal",
  "Sex+ChestPain+Ca+Thal",
  "Sex+MaxHR+ExAng+Oldpeak",
  "Sex+MaxHR+ExAng+Slope",
  "Sex+MaxHR+ExAng+Ca",
  "Sex+MaxHR+ExAng+Thal",
  "Sex+MaxHR+Oldpeak+Slope",
  "Sex+MaxHR+Oldpeak+Ca",
  "Sex+MaxHR+Oldpeak+Thal",
  "Sex+MaxHR+Slope+Ca",
  "Sex+MaxHR+Slope+Thal",
  "Sex+MaxHR+Ca+Thal",
  "Sex+ExAng+Oldpeak+Slope",
  "Sex+ExAng+Oldpeak+Ca",
  "Sex+ExAng+Oldpeak+Thal",
  "Sex+ExAng+Slope+Ca",
  "Sex+ExAng+Slope+Thal",
  "Sex+ExAng+Ca+Thal",
  "Sex+Oldpeak+Slope+Ca",
  "Sex+Oldpeak+Slope+Thal",
  "Sex+Oldpeak+Ca+Thal",
  "Sex+Slope+Ca+Thal",
  "ChestPain+MaxHR+ExAng+Oldpeak",
  "ChestPain+MaxHR+ExAng+Slope",
  "ChestPain+MaxHR+ExAng+Ca",
  "ChestPain+MaxHR+ExAng+Thal",
  "ChestPain+MaxHR+Oldpeak+Slope",
  "ChestPain+MaxHR+Oldpeak+Ca",
  "ChestPain+MaxHR+Oldpeak+Thal",
  "ChestPain+MaxHR+Slope+Ca",
  "ChestPain+MaxHR+Slope+Thal",
  "ChestPain+MaxHR+Ca+Thal",
  "ChestPain+ExAng+Oldpeak+Slope",
  "ChestPain+ExAng+Oldpeak+Ca",
  "ChestPain+ExAng+Oldpeak+Thal",
  "ChestPain+ExAng+Slope+Ca",
  "ChestPain+ExAng+Slope+Thal",
  "ChestPain+ExAng+Ca+Thal",
  "ChestPain+Oldpeak+Slope+Ca",
  "ChestPain+Oldpeak+Slope+Thal",
  "ChestPain+Oldpeak+Ca+Thal",
  "ChestPain+Slope+Ca+Thal",
  "MaxHR+ExAng+Oldpeak+Slope",
  "MaxHR+ExAng+Oldpeak+Ca",
  "MaxHR+ExAng+Oldpeak+Thal",
  "MaxHR+ExAng+Slope+Ca",
  "MaxHR+ExAng+Slope+Thal",
  "MaxHR+ExAng+Ca+Thal",
  "MaxHR+Oldpeak+Slope+Ca",
  "MaxHR+Oldpeak+Slope+Thal",
  "MaxHR+Oldpeak+Ca+Thal",
  "MaxHR+Slope+Ca+Thal",
  "ExAng+Oldpeak+Slope+Ca",
  "ExAng+Oldpeak+Slope+Thal",
  "ExAng+Oldpeak+Ca+Thal",
  "ExAng+Slope+Ca+Thal",
  "Oldpeak+Slope+Ca+Thal"
)
pdf(file = "ProjectTwoPlots.pdf", onefile = TRUE)
for (val in four) {
  glm.variable <- glm(paste("AHD ~", val), data = heartTrain)
  summary(glm.variable)
  
  residual.variable = residuals(glm.variable)
  name <- deparse(substitute(val))
  plot(
    #title(paste("Outlier Check:", name), outer = TRUE)
    fitted(glm.variable),
    residual.variable,
    xlab = paste("Fitted Values=",val),
    ylab = "Residuals", 
    main = "Residuals vs. Fitted Values")
  abline(h = 0, lty = 3)
  hist(residual.variable, main= paste("Residuals of",val))
}
dev.off()
if(FALSE){
  #5
  Age+Sex+ChestPain+MaxHR+ExAng
  Age+Sex+ChestPain+MaxHR+Oldpeak
  Age+Sex+ChestPain+MaxHR+Slope
  Age+Sex+ChestPain+MaxHR+Ca
  Age+Sex+ChestPain+MaxHR+Thal
  Age+Sex+ChestPain+ExAng+Oldpeak
  Age+Sex+ChestPain+ExAng+Slope
  Age+Sex+ChestPain+ExAng+Ca
  Age+Sex+ChestPain+ExAng+Thal
  Age+Sex+ChestPain+Oldpeak+Slope
  Age+Sex+ChestPain+Oldpeak+Ca
  Age+Sex+ChestPain+Oldpeak+Thal
  Age+Sex+ChestPain+Slope+Ca
  Age+Sex+ChestPain+Slope+Thal
  Age+Sex+ChestPain+Ca+Thal
  Age+Sex+MaxHR+ExAng+Oldpeak
  Age+Sex+MaxHR+ExAng+Slope
  Age+Sex+MaxHR+ExAng+Ca
  Age+Sex+MaxHR+ExAng+Thal
  Age+Sex+MaxHR+Oldpeak+Slope
  Age+Sex+MaxHR+Oldpeak+Ca
  Age+Sex+MaxHR+Oldpeak+Thal
  Age+Sex+MaxHR+Slope+Ca
  Age+Sex+MaxHR+Slope+Thal
  Age+Sex+MaxHR+Ca+Thal
  Age+Sex+ExAng+Oldpeak+Slope
  Age+Sex+ExAng+Oldpeak+Ca
  Age+Sex+ExAng+Oldpeak+Thal
  Age+Sex+ExAng+Slope+Ca
  Age+Sex+ExAng+Slope+Thal
  Age+Sex+ExAng+Ca+Thal
  Age+Sex+Oldpeak+Slope+Ca
  Age+Sex+Oldpeak+Slope+Thal
  Age+Sex+Oldpeak+Ca+Thal
  Age+Sex+Slope+Ca+Thal
  Age+ChestPain+MaxHR+ExAng+Oldpeak
  Age+ChestPain+MaxHR+ExAng+Slope
  Age+ChestPain+MaxHR+ExAng+Ca
  Age+ChestPain+MaxHR+ExAng+Thal
  Age+ChestPain+MaxHR+Oldpeak+Slope
  Age+ChestPain+MaxHR+Oldpeak+Ca
  Age+ChestPain+MaxHR+Oldpeak+Thal
  Age+ChestPain+MaxHR+Slope+Ca
  Age+ChestPain+MaxHR+Slope+Thal
  Age+ChestPain+MaxHR+Ca+Thal
  Age+ChestPain+ExAng+Oldpeak+Slope
  Age+ChestPain+ExAng+Oldpeak+Ca
  Age+ChestPain+ExAng+Oldpeak+Thal
  Age+ChestPain+ExAng+Slope+Ca
  Age+ChestPain+ExAng+Slope+Thal
  Age+ChestPain+ExAng+Ca+Thal
  Age+ChestPain+Oldpeak+Slope+Ca
  Age+ChestPain+Oldpeak+Slope+Thal
  Age+ChestPain+Oldpeak+Ca+Thal
  Age+ChestPain+Slope+Ca+Thal
  Age+MaxHR+ExAng+Oldpeak+Slope
  Age+MaxHR+ExAng+Oldpeak+Ca
  Age+MaxHR+ExAng+Oldpeak+Thal
  Age+MaxHR+ExAng+Slope+Ca
  Age+MaxHR+ExAng+Slope+Thal
  Age+MaxHR+ExAng+Ca+Thal
  Age+MaxHR+Oldpeak+Slope+Ca
  Age+MaxHR+Oldpeak+Slope+Thal
  Age+MaxHR+Oldpeak+Ca+Thal
  Age+MaxHR+Slope+Ca+Thal
  Age+ExAng+Oldpeak+Slope+Ca
  Age+ExAng+Oldpeak+Slope+Thal
  Age+ExAng+Oldpeak+Ca+Thal
  Age+ExAng+Slope+Ca+Thal
  Age+Oldpeak+Slope+Ca+Thal
  Sex+ChestPain+MaxHR+ExAng+Oldpeak
  Sex+ChestPain+MaxHR+ExAng+Slope
  Sex+ChestPain+MaxHR+ExAng+Ca
  Sex+ChestPain+MaxHR+ExAng+Thal
  Sex+ChestPain+MaxHR+Oldpeak+Slope
  Sex+ChestPain+MaxHR+Oldpeak+Ca
  Sex+ChestPain+MaxHR+Oldpeak+Thal
  Sex+ChestPain+MaxHR+Slope+Ca
  Sex+ChestPain+MaxHR+Slope+Thal
  Sex+ChestPain+MaxHR+Ca+Thal
  Sex+ChestPain+ExAng+Oldpeak+Slope
  Sex+ChestPain+ExAng+Oldpeak+Ca
  Sex+ChestPain+ExAng+Oldpeak+Thal
  Sex+ChestPain+ExAng+Slope+Ca
  Sex+ChestPain+ExAng+Slope+Thal
  Sex+ChestPain+ExAng+Ca+Thal
  Sex+ChestPain+Oldpeak+Slope+Ca
  Sex+ChestPain+Oldpeak+Slope+Thal
  Sex+ChestPain+Oldpeak+Ca+Thal
  Sex+ChestPain+Slope+Ca+Thal
  Sex+MaxHR+ExAng+Oldpeak+Slope
  Sex+MaxHR+ExAng+Oldpeak+Ca
  Sex+MaxHR+ExAng+Oldpeak+Thal
  Sex+MaxHR+ExAng+Slope+Ca
  Sex+MaxHR+ExAng+Slope+Thal
  Sex+MaxHR+ExAng+Ca+Thal
  Sex+MaxHR+Oldpeak+Slope+Ca
  Sex+MaxHR+Oldpeak+Slope+Thal
  Sex+MaxHR+Oldpeak+Ca+Thal
  Sex+MaxHR+Slope+Ca+Thal
  Sex+ExAng+Oldpeak+Slope+Ca
  Sex+ExAng+Oldpeak+Slope+Thal
  Sex+ExAng+Oldpeak+Ca+Thal
  Sex+ExAng+Slope+Ca+Thal
  Sex+Oldpeak+Slope+Ca+Thal
  ChestPain+MaxHR+ExAng+Oldpeak+Slope
  ChestPain+MaxHR+ExAng+Oldpeak+Ca
  ChestPain+MaxHR+ExAng+Oldpeak+Thal
  ChestPain+MaxHR+ExAng+Slope+Ca
  ChestPain+MaxHR+ExAng+Slope+Thal
  ChestPain+MaxHR+ExAng+Ca+Thal
  ChestPain+MaxHR+Oldpeak+Slope+Ca
  ChestPain+MaxHR+Oldpeak+Slope+Thal
  ChestPain+MaxHR+Oldpeak+Ca+Thal
  ChestPain+MaxHR+Slope+Ca+Thal
  ChestPain+ExAng+Oldpeak+Slope+Ca
  ChestPain+ExAng+Oldpeak+Slope+Thal
  ChestPain+ExAng+Oldpeak+Ca+Thal
  ChestPain+ExAng+Slope+Ca+Thal
  ChestPain+Oldpeak+Slope+Ca+Thal
  MaxHR+ExAng+Oldpeak+Slope+Ca
  MaxHR+ExAng+Oldpeak+Slope+Thal
  MaxHR+ExAng+Oldpeak+Ca+Thal
  MaxHR+ExAng+Slope+Ca+Thal
  MaxHR+Oldpeak+Slope+Ca+Thal
  ExAng+Oldpeak+Slope+Ca+Thal
}
if(FALSE){
  #6
  Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak
  Age+Sex+ChestPain+MaxHR+ExAng+Slope
  Age+Sex+ChestPain+MaxHR+ExAng+Ca
  Age+Sex+ChestPain+MaxHR+ExAng+Thal
  Age+Sex+ChestPain+MaxHR+Oldpeak+Slope
  Age+Sex+ChestPain+MaxHR+Oldpeak+Ca
  Age+Sex+ChestPain+MaxHR+Oldpeak+Thal
  Age+Sex+ChestPain+MaxHR+Slope+Ca
  Age+Sex+ChestPain+MaxHR+Slope+Thal
  Age+Sex+ChestPain+MaxHR+Ca+Thal
  Age+Sex+ChestPain+ExAng+Oldpeak+Slope
  Age+Sex+ChestPain+ExAng+Oldpeak+Ca
  Age+Sex+ChestPain+ExAng+Oldpeak+Thal
  Age+Sex+ChestPain+ExAng+Slope+Ca
  Age+Sex+ChestPain+ExAng+Slope+Thal
  Age+Sex+ChestPain+ExAng+Ca+Thal
  Age+Sex+ChestPain+Oldpeak+Slope+Ca
  Age+Sex+ChestPain+Oldpeak+Slope+Thal
  Age+Sex+ChestPain+Oldpeak+Ca+Thal
  Age+Sex+ChestPain+Slope+Ca+Thal
  Age+Sex+MaxHR+ExAng+Oldpeak+Slope
  Age+Sex+MaxHR+ExAng+Oldpeak+Ca
  Age+Sex+MaxHR+ExAng+Oldpeak+Thal
  Age+Sex+MaxHR+ExAng+Slope+Ca
  Age+Sex+MaxHR+ExAng+Slope+Thal
  Age+Sex+MaxHR+ExAng+Ca+Thal
  Age+Sex+MaxHR+Oldpeak+Slope+Ca
  Age+Sex+MaxHR+Oldpeak+Slope+Thal
  Age+Sex+MaxHR+Oldpeak+Ca+Thal
  Age+Sex+MaxHR+Slope+Ca+Thal
  Age+Sex+ExAng+Oldpeak+Slope+Ca
  Age+Sex+ExAng+Oldpeak+Slope+Thal
  Age+Sex+ExAng+Oldpeak+Ca+Thal
  Age+Sex+ExAng+Slope+Ca+Thal
  Age+Sex+Oldpeak+Slope+Ca+Thal
  Age+ChestPain+MaxHR+ExAng+Oldpeak+Slope
  Age+ChestPain+MaxHR+ExAng+Oldpeak+Ca
  Age+ChestPain+MaxHR+ExAng+Oldpeak+Thal
  Age+ChestPain+MaxHR+ExAng+Slope+Ca
  Age+ChestPain+MaxHR+ExAng+Slope+Thal
  Age+ChestPain+MaxHR+ExAng+Ca+Thal
  Age+ChestPain+MaxHR+Oldpeak+Slope+Ca
  Age+ChestPain+MaxHR+Oldpeak+Slope+Thal
  Age+ChestPain+MaxHR+Oldpeak+Ca+Thal
  Age+ChestPain+MaxHR+Slope+Ca+Thal
  Age+ChestPain+ExAng+Oldpeak+Slope+Ca
  Age+ChestPain+ExAng+Oldpeak+Slope+Thal
  Age+ChestPain+ExAng+Oldpeak+Ca+Thal
  Age+ChestPain+ExAng+Slope+Ca+Thal
  Age+ChestPain+Oldpeak+Slope+Ca+Thal
  Age+MaxHR+ExAng+Oldpeak+Slope+Ca
  Age+MaxHR+ExAng+Oldpeak+Slope+Thal
  Age+MaxHR+ExAng+Oldpeak+Ca+Thal
  Age+MaxHR+ExAng+Slope+Ca+Thal
  Age+MaxHR+Oldpeak+Slope+Ca+Thal
  Age+ExAng+Oldpeak+Slope+Ca+Thal
  Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope
  Sex+ChestPain+MaxHR+ExAng+Oldpeak+Ca
  Sex+ChestPain+MaxHR+ExAng+Oldpeak+Thal
  Sex+ChestPain+MaxHR+ExAng+Slope+Ca
  Sex+ChestPain+MaxHR+ExAng+Slope+Thal
  Sex+ChestPain+MaxHR+ExAng+Ca+Thal
  Sex+ChestPain+MaxHR+Oldpeak+Slope+Ca
  Sex+ChestPain+MaxHR+Oldpeak+Slope+Thal
  Sex+ChestPain+MaxHR+Oldpeak+Ca+Thal
  Sex+ChestPain+MaxHR+Slope+Ca+Thal
  Sex+ChestPain+ExAng+Oldpeak+Slope+Ca
  Sex+ChestPain+ExAng+Oldpeak+Slope+Thal
  Sex+ChestPain+ExAng+Oldpeak+Ca+Thal
  Sex+ChestPain+ExAng+Slope+Ca+Thal
  Sex+ChestPain+Oldpeak+Slope+Ca+Thal
  Sex+MaxHR+ExAng+Oldpeak+Slope+Ca
  Sex+MaxHR+ExAng+Oldpeak+Slope+Thal
  Sex+MaxHR+ExAng+Oldpeak+Ca+Thal
  Sex+MaxHR+ExAng+Slope+Ca+Thal
  Sex+MaxHR+Oldpeak+Slope+Ca+Thal
  Sex+ExAng+Oldpeak+Slope+Ca+Thal
  ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca
  ChestPain+MaxHR+ExAng+Oldpeak+Slope+Thal
  ChestPain+MaxHR+ExAng+Oldpeak+Ca+Thal
  ChestPain+MaxHR+ExAng+Slope+Ca+Thal
  ChestPain+MaxHR+Oldpeak+Slope+Ca+Thal
  ChestPain+ExAng+Oldpeak+Slope+Ca+Thal
  MaxHR+ExAng+Oldpeak+Slope+Ca+Thal
}

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

#########################################################################

glm(heartTrain$AHD ~ heartTrain$), col = "red")

plot
termplot(lm.AHD.Age, partial.resid = TRUE)



























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


lm.mpg.hp_weight_acceleration <-
  lm(mpg ~ horsepower + weight + acceleration, data = mpgTrain)
summary(lm.mpg.hp_weight_acceleration)
plot(lm.mpg.hp_weight_acceleration)
par(mfrow = c(2, 2))
termplot(lm.mpg.hp_weight_acceleration, partial.resid = TRUE)