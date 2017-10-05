setwd("/home/amcdowald/Downloads")
heart.data <- read.csv('Heart.csv')
heartTrain <- na.omit(heart.data[0:250, ])
#heartTrain <- head(heart.data, 250)
#View(heartTrain)
library(reshape2)
library(ggplot2)
summary(heartTrain)
recode <-c("Yes"=2,"No"= 1)
heartTrain$AHD <-ifelse (heartTrain$AHD == "No",0, ifelse(heartTrain$AHD =="Yes", 1, NA))
heartTrain$ChestPain  <-ifelse (heartTrain$ChestPain == "typical",0, ifelse(heartTrain$ChestPain =="asymptomatic", 1, ifelse(heartTrain$ChestPain =="nonanginal", 2, ifelse(heartTrain$ChestPain =="nontypical", 3, NA))))
heartTrain$Thal <-ifelse (heartTrain$Thal == "fixed",0, ifelse(heartTrain$Thal =="normal", 1, ifelse(heartTrain$Thal =="reversible", 2, NA)))

#pdf(file = "ProjectTwoPlots.pdf", onefile = TRUE)
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


#================================================================================== NOTES
# We need to remove the weakly correlated
# Categorical variables we dont remove outliers Sex ChestPain Thal ExAng Slope Ca
# Removed 0s in Oldpeak to make data normal
#


#Age + Sex + ChestPain + MaxHR + ExAng + Oldpeak + Slope + Ca + Thal

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
  #response <-
    #readline(prompt = "Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if (TRUE) {
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
#Age + Sex + ChestPain + MaxHR + ExAng + Oldpeak + Slope + Ca + Thal
Age = heartTrain$Age
Sex = heartTrain$Sex
ChestPain = heartTrain$ChestPain
MaxHR = heartTrain$MaxHR
ExAng = heartTrain$ExAng
Oldpeak = heartTrain$Oldpeak
Slope = heartTrain$Slope
Ca = heartTrain$Ca
Thal = heartTrain$Thal
outlierCheck(heartTrain, Age)
outlierCheck(heartTrain, MaxHR)
outlierCheck(heartTrain, Oldpeak)
heartTrain <- na.omit(heartTrain)
#================================================================================
one <- c("Age",
         "Sex",
         "ChestPain",
         "MaxHR",
         "ExAng",
         "Oldpeak",
         "Slope",
         "Ca",
         "Thal")

two <- c(
  "Age + Sex",
  "Age + ChestPain",
  "Age + MaxHR",
  "Age + ExAng",
  "Age + Oldpeak",
  "Age + Slope",
  "Age + Ca",
  "Age + Thal",
  "Sex + ChestPain",
  "Sex + MaxHR",
  "Sex + ExAng",
  "Sex + Oldpeak",
  "Sex + Slope",
  "Sex + Ca",
  "Sex + Thal",
  "ChestPain + MaxHR",
  "ChestPain + ExAng",
  "ChestPain + Oldpeak",
  "ChestPain + Slope",
  "ChestPain + Ca",
  "ChestPain + Thal",
  "MaxHR + ExAng",
  "MaxHR + Oldpeak",
  "MaxHR + Slope",
  "MaxHR + Ca",
  "MaxHR + Thal",
  "ExAng + Oldpeak",
  "ExAng + Slope",
  "ExAng + Ca",
  "ExAng + Thal",
  "Oldpeak + Slope",
  "Oldpeak + Ca",
  "Oldpeak + Thal",
  "Slope + Ca",
  "Slope + Thal",
  "Ca + Thal"
)

three <- c(
  "Age + Sex + ChestPain",
  "Age + Sex + MaxHR",
  "Age + Sex + ExAng",
  "Age + Sex + Oldpeak",
  "Age + Sex + Slope",
  "Age + Sex + Ca",
  "Age + Sex + Thal",
  "Age + ChestPain + MaxHR",
  "Age + ChestPain + ExAng",
  "Age + ChestPain + Oldpeak",
  "Age + ChestPain + Slope",
  "Age + ChestPain + Ca",
  "Age + ChestPain + Thal",
  "Age + MaxHR + ExAng",
  "Age + MaxHR + Oldpeak",
  "Age + MaxHR + Slope",
  "Age + MaxHR + Ca",
  "Age + MaxHR + Thal",
  "Age + ExAng + Oldpeak",
  "Age + ExAng + Slope",
  "Age + ExAng + Ca",
  "Age + ExAng + Thal",
  "Age + Oldpeak + Slope",
  "Age + Oldpeak + Ca",
  "Age + Oldpeak + Thal",
  "Age + Slope + Ca",
  "Age + Slope + Thal",
  "Age + Ca + Thal",
  "Sex + ChestPain + MaxHR",
  "Sex + ChestPain + ExAng",
  "Sex + ChestPain + Oldpeak",
  "Sex + ChestPain + Slope",
  "Sex + ChestPain + Ca",
  "Sex + ChestPain + Thal",
  "Sex + MaxHR + ExAng",
  "Sex + MaxHR + Oldpeak",
  "Sex + MaxHR + Slope",
  "Sex + MaxHR + Ca",
  "Sex + MaxHR + Thal",
  "Sex + ExAng + Oldpeak",
  "Sex + ExAng + Slope",
  "Sex + ExAng + Ca",
  "Sex + ExAng + Thal",
  "Sex + Oldpeak + Slope",
  "Sex + Oldpeak + Ca",
  "Sex + Oldpeak + Thal",
  "Sex + Slope + Ca",
  "Sex + Slope + Thal",
  "Sex + Ca + Thal",
  "ChestPain + MaxHR + ExAng",
  "ChestPain + MaxHR + Oldpeak",
  "ChestPain + MaxHR + Slope",
  "ChestPain + MaxHR + Ca",
  "ChestPain + MaxHR + Thal",
  "ChestPain + ExAng + Oldpeak",
  "ChestPain + ExAng + Slope",
  "ChestPain + ExAng + Ca",
  "ChestPain + ExAng + Thal",
  "ChestPain + Oldpeak + Slope",
  "ChestPain + Oldpeak + Ca",
  "ChestPain + Oldpeak + Thal",
  "ChestPain + Slope + Ca",
  "ChestPain + Slope + Thal",
  "ChestPain + Ca + Thal",
  "MaxHR + ExAng + Oldpeak",
  "MaxHR + ExAng + Slope",
  "MaxHR + ExAng + Ca",
  "MaxHR + ExAng + Thal",
  "MaxHR + Oldpeak + Slope",
  "MaxHR + Oldpeak + Ca",
  "MaxHR + Oldpeak + Thal",
  "MaxHR + Slope + Ca",
  "MaxHR + Slope + Thal",
  "MaxHR + Ca + Thal",
  "ExAng + Oldpeak + Slope",
  "ExAng + Oldpeak + Ca",
  "ExAng + Oldpeak + Thal",
  "ExAng + Slope + Ca",
  "ExAng + Slope + Thal",
  "ExAng + Ca + Thal",
  "Oldpeak + Slope + Ca",
  "Oldpeak + Slope + Thal",
  "Oldpeak + Ca + Thal",
  "Slope + Ca + Thal"
)
#ChestPain + MaxHR + ExAng + Oldpeak
four <- c(
  #4
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
five <- c(
  "Age+Sex+ChestPain+MaxHR+ExAng",
  "Age+Sex+ChestPain+MaxHR+Oldpeak",
  "Age+Sex+ChestPain+MaxHR+Slope",
  "Age+Sex+ChestPain+MaxHR+Ca",
  "Age+Sex+ChestPain+MaxHR+Thal",
  "Age+Sex+ChestPain+ExAng+Oldpeak",
  "Age+Sex+ChestPain+ExAng+Slope",
  "Age+Sex+ChestPain+ExAng+Ca",
  "Age+Sex+ChestPain+ExAng+Thal",
  "Age+Sex+ChestPain+Oldpeak+Slope",
  "Age+Sex+ChestPain+Oldpeak+Ca",
  "Age+Sex+ChestPain+Oldpeak+Thal",
  "Age+Sex+ChestPain+Slope+Ca",
  "Age+Sex+ChestPain+Slope+Thal",
  "Age+Sex+ChestPain+Ca+Thal",
  "Age+Sex+MaxHR+ExAng+Oldpeak",
  "Age+Sex+MaxHR+ExAng+Slope",
  "Age+Sex+MaxHR+ExAng+Ca",
  "Age+Sex+MaxHR+ExAng+Thal",
  "Age+Sex+MaxHR+Oldpeak+Slope",
  "Age+Sex+MaxHR+Oldpeak+Ca",
  "Age+Sex+MaxHR+Oldpeak+Thal",
  "Age+Sex+MaxHR+Slope+Ca",
  "Age+Sex+MaxHR+Slope+Thal",
  "Age+Sex+MaxHR+Ca+Thal",
  "Age+Sex+ExAng+Oldpeak+Slope",
  "Age+Sex+ExAng+Oldpeak+Ca",
  "Age+Sex+ExAng+Oldpeak+Thal",
  "Age+Sex+ExAng+Slope+Ca",
  "Age+Sex+ExAng+Slope+Thal",
  "Age+Sex+ExAng+Ca+Thal",
  "Age+Sex+Oldpeak+Slope+Ca",
  "Age+Sex+Oldpeak+Slope+Thal",
  "Age+Sex+Oldpeak+Ca+Thal",
  "Age+Sex+Slope+Ca+Thal",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak",
  "Age+ChestPain+MaxHR+ExAng+Slope",
  "Age+ChestPain+MaxHR+ExAng+Ca",
  "Age+ChestPain+MaxHR+ExAng+Thal",
  "Age+ChestPain+MaxHR+Oldpeak+Slope",
  "Age+ChestPain+MaxHR+Oldpeak+Ca",
  "Age+ChestPain+MaxHR+Oldpeak+Thal",
  "Age+ChestPain+MaxHR+Slope+Ca",
  "Age+ChestPain+MaxHR+Slope+Thal",
  "Age+ChestPain+MaxHR+Ca+Thal",
  "Age+ChestPain+ExAng+Oldpeak+Slope",
  "Age+ChestPain+ExAng+Oldpeak+Ca",
  "Age+ChestPain+ExAng+Oldpeak+Thal",
  "Age+ChestPain+ExAng+Slope+Ca",
  "Age+ChestPain+ExAng+Slope+Thal",
  "Age+ChestPain+ExAng+Ca+Thal",
  "Age+ChestPain+Oldpeak+Slope+Ca",
  "Age+ChestPain+Oldpeak+Slope+Thal",
  "Age+ChestPain+Oldpeak+Ca+Thal",
  "Age+ChestPain+Slope+Ca+Thal",
  "Age+MaxHR+ExAng+Oldpeak+Slope",
  "Age+MaxHR+ExAng+Oldpeak+Ca",
  "Age+MaxHR+ExAng+Oldpeak+Thal",
  "Age+MaxHR+ExAng+Slope+Ca",
  "Age+MaxHR+ExAng+Slope+Thal",
  "Age+MaxHR+ExAng+Ca+Thal",
  "Age+MaxHR+Oldpeak+Slope+Ca",
  "Age+MaxHR+Oldpeak+Slope+Thal",
  "Age+MaxHR+Oldpeak+Ca+Thal",
  "Age+MaxHR+Slope+Ca+Thal",
  "Age+ExAng+Oldpeak+Slope+Ca",
  "Age+ExAng+Oldpeak+Slope+Thal",
  "Age+ExAng+Oldpeak+Ca+Thal",
  "Age+ExAng+Slope+Ca+Thal",
  "Age+Oldpeak+Slope+Ca+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak",
  "Sex+ChestPain+MaxHR+ExAng+Slope",
  "Sex+ChestPain+MaxHR+ExAng+Ca",
  "Sex+ChestPain+MaxHR+ExAng+Thal",
  "Sex+ChestPain+MaxHR+Oldpeak+Slope",
  "Sex+ChestPain+MaxHR+Oldpeak+Ca",
  "Sex+ChestPain+MaxHR+Oldpeak+Thal",
  "Sex+ChestPain+MaxHR+Slope+Ca",
  "Sex+ChestPain+MaxHR+Slope+Thal",
  "Sex+ChestPain+MaxHR+Ca+Thal",
  "Sex+ChestPain+ExAng+Oldpeak+Slope",
  "Sex+ChestPain+ExAng+Oldpeak+Ca",
  "Sex+ChestPain+ExAng+Oldpeak+Thal",
  "Sex+ChestPain+ExAng+Slope+Ca",
  "Sex+ChestPain+ExAng+Slope+Thal",
  "Sex+ChestPain+ExAng+Ca+Thal",
  "Sex+ChestPain+Oldpeak+Slope+Ca",
  "Sex+ChestPain+Oldpeak+Slope+Thal",
  "Sex+ChestPain+Oldpeak+Ca+Thal",
  "Sex+ChestPain+Slope+Ca+Thal",
  "Sex+MaxHR+ExAng+Oldpeak+Slope",
  "Sex+MaxHR+ExAng+Oldpeak+Ca",
  "Sex+MaxHR+ExAng+Oldpeak+Thal",
  "Sex+MaxHR+ExAng+Slope+Ca",
  "Sex+MaxHR+ExAng+Slope+Thal",
  "Sex+MaxHR+ExAng+Ca+Thal",
  "Sex+MaxHR+Oldpeak+Slope+Ca",
  "Sex+MaxHR+Oldpeak+Slope+Thal",
  "Sex+MaxHR+Oldpeak+Ca+Thal",
  "Sex+MaxHR+Slope+Ca+Thal",
  "Sex+ExAng+Oldpeak+Slope+Ca",
  "Sex+ExAng+Oldpeak+Slope+Thal",
  "Sex+ExAng+Oldpeak+Ca+Thal",
  "Sex+ExAng+Slope+Ca+Thal",
  "Sex+Oldpeak+Slope+Ca+Thal",
  "ChestPain+MaxHR+ExAng+Oldpeak+Slope",
  "ChestPain+MaxHR+ExAng+Oldpeak+Ca",
  "ChestPain+MaxHR+ExAng+Oldpeak+Thal",
  "ChestPain+MaxHR+ExAng+Slope+Ca",
  "ChestPain+MaxHR+ExAng+Slope+Thal",
  "ChestPain+MaxHR+ExAng+Ca+Thal",
  "ChestPain+MaxHR+Oldpeak+Slope+Ca",
  "ChestPain+MaxHR+Oldpeak+Slope+Thal",
  "ChestPain+MaxHR+Oldpeak+Ca+Thal",
  "ChestPain+MaxHR+Slope+Ca+Thal",
  "ChestPain+ExAng+Oldpeak+Slope+Ca",
  "ChestPain+ExAng+Oldpeak+Slope+Thal",
  "ChestPain+ExAng+Oldpeak+Ca+Thal",
  "ChestPain+ExAng+Slope+Ca+Thal",
  "ChestPain+Oldpeak+Slope+Ca+Thal",
  "MaxHR+ExAng+Oldpeak+Slope+Ca",
  "MaxHR+ExAng+Oldpeak+Slope+Thal",
  "MaxHR+ExAng+Oldpeak+Ca+Thal",
  "MaxHR+ExAng+Slope+Ca+Thal",
  "MaxHR+Oldpeak+Slope+Ca+Thal",
  "ExAng+Oldpeak+Slope+Ca+Thal"
)

six <- c(
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak",
  "Age+Sex+ChestPain+MaxHR+ExAng+Slope",
  "Age+Sex+ChestPain+MaxHR+ExAng+Ca",
  "Age+Sex+ChestPain+MaxHR+ExAng+Thal",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Slope",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Ca",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Thal",
  "Age+Sex+ChestPain+MaxHR+Slope+Ca",
  "Age+Sex+ChestPain+MaxHR+Slope+Thal",
  "Age+Sex+ChestPain+MaxHR+Ca+Thal",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Slope",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Ca",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Thal",
  "Age+Sex+ChestPain+ExAng+Slope+Ca",
  "Age+Sex+ChestPain+ExAng+Slope+Thal",
  "Age+Sex+ChestPain+ExAng+Ca+Thal",
  "Age+Sex+ChestPain+Oldpeak+Slope+Ca",
  "Age+Sex+ChestPain+Oldpeak+Slope+Thal",
  "Age+Sex+ChestPain+Oldpeak+Ca+Thal",
  "Age+Sex+ChestPain+Slope+Ca+Thal",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Slope",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Ca",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Thal",
  "Age+Sex+MaxHR+ExAng+Slope+Ca",
  "Age+Sex+MaxHR+ExAng+Slope+Thal",
  "Age+Sex+MaxHR+ExAng+Ca+Thal",
  "Age+Sex+MaxHR+Oldpeak+Slope+Ca",
  "Age+Sex+MaxHR+Oldpeak+Slope+Thal",
  "Age+Sex+MaxHR+Oldpeak+Ca+Thal",
  "Age+Sex+MaxHR+Slope+Ca+Thal",
  "Age+Sex+ExAng+Oldpeak+Slope+Ca",
  "Age+Sex+ExAng+Oldpeak+Slope+Thal",
  "Age+Sex+ExAng+Oldpeak+Ca+Thal",
  "Age+Sex+ExAng+Slope+Ca+Thal",
  "Age+Sex+Oldpeak+Slope+Ca+Thal",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Slope",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Ca",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Thal",
  "Age+ChestPain+MaxHR+ExAng+Slope+Ca",
  "Age+ChestPain+MaxHR+ExAng+Slope+Thal",
  "Age+ChestPain+MaxHR+ExAng+Ca+Thal",
  "Age+ChestPain+MaxHR+Oldpeak+Slope+Ca",
  "Age+ChestPain+MaxHR+Oldpeak+Slope+Thal",
  "Age+ChestPain+MaxHR+Oldpeak+Ca+Thal",
  "Age+ChestPain+MaxHR+Slope+Ca+Thal",
  "Age+ChestPain+ExAng+Oldpeak+Slope+Ca",
  "Age+ChestPain+ExAng+Oldpeak+Slope+Thal",
  "Age+ChestPain+ExAng+Oldpeak+Ca+Thal",
  "Age+ChestPain+ExAng+Slope+Ca+Thal",
  "Age+ChestPain+Oldpeak+Slope+Ca+Thal",
  "Age+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "Age+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "Age+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "Age+MaxHR+ExAng+Slope+Ca+Thal",
  "Age+MaxHR+Oldpeak+Slope+Ca+Thal",
  "Age+ExAng+Oldpeak+Slope+Ca+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Ca",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Slope+Ca",
  "Sex+ChestPain+MaxHR+ExAng+Slope+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Ca+Thal",
  "Sex+ChestPain+MaxHR+Oldpeak+Slope+Ca",
  "Sex+ChestPain+MaxHR+Oldpeak+Slope+Thal",
  "Sex+ChestPain+MaxHR+Oldpeak+Ca+Thal",
  "Sex+ChestPain+MaxHR+Slope+Ca+Thal",
  "Sex+ChestPain+ExAng+Oldpeak+Slope+Ca",
  "Sex+ChestPain+ExAng+Oldpeak+Slope+Thal",
  "Sex+ChestPain+ExAng+Oldpeak+Ca+Thal",
  "Sex+ChestPain+ExAng+Slope+Ca+Thal",
  "Sex+ChestPain+Oldpeak+Slope+Ca+Thal",
  "Sex+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "Sex+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "Sex+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "Sex+MaxHR+ExAng+Slope+Ca+Thal",
  "Sex+MaxHR+Oldpeak+Slope+Ca+Thal",
  "Sex+ExAng+Oldpeak+Slope+Ca+Thal",
  "ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "ChestPain+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "ChestPain+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "ChestPain+MaxHR+ExAng+Slope+Ca+Thal",
  "ChestPain+MaxHR+Oldpeak+Slope+Ca+Thal",
  "ChestPain+ExAng+Oldpeak+Slope+Ca+Thal",
  "MaxHR+ExAng+Oldpeak+Slope+Ca+Thal"
  
)

seven <- c(
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope",
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Ca",
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Thal",
  "Age+Sex+ChestPain+MaxHR+ExAng+Slope+Ca",
  "Age+Sex+ChestPain+MaxHR+ExAng+Slope+Thal",
  "Age+Sex+ChestPain+MaxHR+ExAng+Ca+Thal",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Slope+Ca",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Slope+Thal",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Ca+Thal",
  "Age+Sex+ChestPain+MaxHR+Slope+Ca+Thal",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Slope+Ca",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Slope+Thal",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Ca+Thal",
  "Age+Sex+ChestPain+ExAng+Slope+Ca+Thal",
  "Age+Sex+ChestPain+Oldpeak+Slope+Ca+Thal",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "Age+Sex+MaxHR+ExAng+Slope+Ca+Thal",
  "Age+Sex+MaxHR+Oldpeak+Slope+Ca+Thal",
  "Age+Sex+ExAng+Oldpeak+Slope+Ca+Thal",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "Age+ChestPain+MaxHR+ExAng+Slope+Ca+Thal",
  "Age+ChestPain+MaxHR+Oldpeak+Slope+Ca+Thal",
  "Age+ChestPain+ExAng+Oldpeak+Slope+Ca+Thal",
  "Age+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Slope+Ca+Thal",
  "Sex+ChestPain+MaxHR+Oldpeak+Slope+Ca+Thal",
  "Sex+ChestPain+ExAng+Oldpeak+Slope+Ca+Thal",
  "Sex+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal",
  "ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal"
)

eight <- c(
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca",
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Thal",
  "Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Ca+Thal",
  "Age+Sex+ChestPain+MaxHR+ExAng+Slope+Ca+Thal",
  "Age+Sex+ChestPain+MaxHR+Oldpeak+Slope+Ca+Thal",
  "Age+Sex+ChestPain+ExAng+Oldpeak+Slope+Ca+Thal",
  "Age+Sex+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal",
  "Age+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal",
  "Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal"
)

nine <- c("Age + Sex + ChestPain + MaxHR + ExAng + Oldpeak + Slope + Ca + Thal")

plotFunc <- function(variable) {
  for (val in variable) {
    glm.variable <- glm(paste("AHD ~", val), data = heartTrain)
    x= summary(glm.variable)$aic
    x2 <- anova(glm.variable)$'Pr(>F)'[1]
    print(paste("AIC",x,"Pval",x2,val))
    
    # 
    # residual.variable = residuals(glm.variable)
    # name <- deparse(substitute(val))
    # plot(
    #   #title(paste("Outlier Check:", name), outer = TRUE)
    #   fitted(glm.variable),
    #   residual.variable,
    #   xlab = paste("Fitted Values=", val),
    #   ylab = "Residuals",
    #   main = "Residuals vs. Fitted Values"
    # )
    # abline(h = 0, lty = 3)
    # hist(residual.variable,col="lightgreen", freq=FALSE, xlab = paste("Residuals of", val),main = paste("Residuals of", val))
    # curve(dnorm(x, mean=mean(residual.variable), sd=sd(residual.variable)), add=TRUE, col="darkblue", lwd=2)
    # new.var <- heartTrain[[paste(val)]]
    # hist(new.var,prob=T,breaks=9, xlab=paste("This",val), main=paste("Histogram of", val))
    # lines(density(heartTrain[[paste(val)]]), col="red")
    # qqnorm(heartTrain[[paste(val)]], main=paste("Normal Q-Q Plot of", val))
    # qqline(heartTrain[[paste(val)]])
  }
  
}
heartTrain <- subset(heartTrain,Oldpeak > 0)
#^^INITIALIZED CODE========================================================================================
pdf(file = "ProjectTwoPlots.pdf", onefile = TRUE)
l <- c("Age", "Oldpeak", "MaxHR")
variable_list <- c(one , two, three, four, five, six, seven, eight, nine)
for(num in variable_list){
  plotFunc(num)
  
}#

glm.variable <- glm(heartTrain$AHD ~  heartTrain$Sex + heartTrain$ChestPain + heartTrain$MaxHR + heartTrain$ExAng + heartTrain$Oldpeak + heartTrain$Slope + heartTrain$Ca + heartTrain$Thal, data = heartTrain)
step <- step(glm.variable)
step$anova
plotFunc(one)
glm.variable <- glm(heartTrain$AHD ~heartTrain$Sex+heartTrain$ChestPain+heartTrain$MaxHR+heartTrain$ExAng+heartTrain$Oldpeak+heartTrain$Slope+heartTrain$Ca+heartTrain$Thal, data = heartTrain)
summary(glm.variable)
  glm.variable <- glm(heartTrain$AHD ~heartTrain$Age)
summary(glm.variable)

hist(heartTrain$Age,prob=T,breaks=9, xlab=paste("This"), main=paste("Histogram of"))
logit2 <- glm(formula = AHD ~ MaxHR + Oldpeak + Sex + ChestPain + ExAng + Slope + Ca + Thal, data=heart250, family="gaussian"(link="identity"))
summary(logit2)
anova(object=logit2, test="Chisq")
BIC(logit2) #Higher numbers of Residual deviance indicates bad fit.  Deviance is a measure of goodness of fit of a model.
pR2(logit2) # higher values indicating better model fit
exp(confint(logit2))
nagelkerke(logit2)
res2 <- residuals(logit2)
hist(res2,prob=T,breaks=9, xlab="Residuals", main="Histogram of the residuals" )
lines(density(res2),col="red")
qqnorm(res2)
qqline(res2)
pred<-predict(logit3)
pred
table(pred,heart250$AHD)
min(pred)
heartTrain$AHD<-heartTrain$AHD==""

