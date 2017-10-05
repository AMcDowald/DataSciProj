library(reshape2)
library(ggplot2)
library("e1071")

setwd("/home/amcdowald/Downloads")
heart.data <- read.csv('Heart.csv')

#Creating Test data and Training data
heartTrain <- na.omit(heart.data[0:250, ])
heartTest <- na.omit(heart.data[251:nrow(heart.data), ])


# Changing categorical values to numbers
heartTrain$AHD <-ifelse (heartTrain$AHD == "No",0, ifelse(heartTrain$AHD =="Yes", 1, NA))
heartTrain$ChestPain  <-ifelse (heartTrain$ChestPain == "typical",0, ifelse(heartTrain$ChestPain =="asymptomatic", 1, ifelse(heartTrain$ChestPain =="nonanginal", 2, ifelse(heartTrain$ChestPain =="nontypical", 3, NA))))
heartTrain$Thal <-ifelse (heartTrain$Thal == "fixed",0, ifelse(heartTrain$Thal =="normal", 1, ifelse(heartTrain$Thal =="reversable", 2, NA)))

heartTest$AHD <-ifelse (heartTest$AHD == "No",0, ifelse(heartTest$AHD =="Yes", 1, NA))
heartTest$ChestPain  <-ifelse (heartTest$ChestPain == "typical",0, ifelse(heartTest$ChestPain =="asymptomatic", 1, ifelse(heartTest$ChestPain =="nonanginal", 2, ifelse(heartTest$ChestPain =="nontypical", 3, NA))))
heartTest$Thal <-ifelse (heartTest$Thal == "fixed",0, ifelse(heartTest$Thal =="normal", 1, ifelse(heartTest$Thal =="reversable", 2, NA)))

heart2$AHD <-ifelse (heart2$AHD == "No",0, ifelse(heart2$AHD =="Yes", 1, NA))
heart2$ChestPain  <-ifelse (heart2$ChestPain == "typical",0, ifelse(heart2$ChestPain =="asymptomatic", 1, ifelse(heart2$ChestPain =="nonanginal", 2, ifelse(heart2$ChestPain =="nontypical", 3, NA))))
heart2$Thal <-ifelse (heart2$Thal == "fixed",0, ifelse(heart2$Thal =="normal", 1, ifelse(heart2$Thal =="reversable", 2, NA)))


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
# Removing outliers from heartTrain data set
# Categorical variables we dont remove outliers (Sex ChestPain Thal ExAng Slope Ca)
Age = heartTrain$Age
Sex = heartTrain$Sex
ChestPain = heartTrain$ChestPain
MaxHR = heartTrain$MaxHR
ExAng = heartTrain$ExAng
Oldpeak = heartTrain$Oldpeak
Slope = heartTrain$Slope
Ca = heartTrain$Ca
Chol = heartTrain$Chol
Thal = heartTrain$Thal

outlierCheck(heartTrain, MaxHR)
outlierCheck(heartTrain, Oldpeak)
outlierCheck(heartTrain, RestBP)
outlierCheck(heartTrain, Chol)
heartTrain <- subset(heartTrain,Oldpeak > 0)
heartTrain <- na.omit(heartTrain)
#================================================================================
# These are the different combinations of the nine variables accepted variables
# We removed RestBP Chol Fbs RestECG because they were weakly coorealated to AHD
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
  "Age+Sex",
  "Age+ChestPain",
  "Age+MaxHR",
  "Age+ExAng",
  "Age+Oldpeak",
  "Age+Slope",
  "Age+Ca",
  "Age+Thal",
  "Sex+ChestPain",
  "Sex+MaxHR",
  "Sex+ExAng",
  "Sex+Oldpeak",
  "Sex+Slope",
  "Sex+Ca",
  "Sex+Thal",
  "ChestPain+MaxHR",
  "ChestPain+ExAng",
  "ChestPain+Oldpeak",
  "ChestPain+Slope",
  "ChestPain+Ca",
  "ChestPain+Thal",
  "MaxHR+ExAng",
  "MaxHR+Oldpeak",
  "MaxHR+Slope",
  "MaxHR+Ca",
  "MaxHR+Thal",
  "ExAng+Oldpeak",
  "ExAng+Slope",
  "ExAng+Ca",
  "ExAng+Thal",
  "Oldpeak+Slope",
  "Oldpeak+Ca",
  "Oldpeak+Thal",
  "Slope+Ca",
  "Slope+Thal",
  "Ca+Thal"
)

three <- c(
  "Age+Sex+ChestPain",
  "Age+Sex+MaxHR",
  "Age+Sex+ExAng",
  "Age+Sex+Oldpeak",
  "Age+Sex+Slope",
  "Age+Sex+Ca",
  "Age+Sex+Thal",
  "Age+ChestPain+MaxHR",
  "Age+ChestPain+ExAng",
  "Age+ChestPain+Oldpeak",
  "Age+ChestPain+Slope",
  "Age+ChestPain+Ca",
  "Age+ChestPain+Thal",
  "Age+MaxHR+ExAng",
  "Age+MaxHR+Oldpeak",
  "Age+MaxHR+Slope",
  "Age+MaxHR+Ca",
  "Age+MaxHR+Thal",
  "Age+ExAng+Oldpeak",
  "Age+ExAng+Slope",
  "Age+ExAng+Ca",
  "Age+ExAng+Thal",
  "Age+Oldpeak+Slope",
  "Age+Oldpeak+Ca",
  "Age+Oldpeak+Thal",
  "Age+Slope+Ca",
  "Age+Slope+Thal",
  "Age+Ca+Thal",
  "Sex+ChestPain+MaxHR",
  "Sex+ChestPain+ExAng",
  "Sex+ChestPain+Oldpeak",
  "Sex+ChestPain+Slope",
  "Sex+ChestPain+Ca",
  "Sex+ChestPain+Thal",
  "Sex+MaxHR+ExAng",
  "Sex+MaxHR+Oldpeak",
  "Sex+MaxHR+Slope",
  "Sex+MaxHR+Ca",
  "Sex+MaxHR+Thal",
  "Sex+ExAng+Oldpeak",
  "Sex+ExAng+Slope",
  "Sex+ExAng+Ca",
  "Sex+ExAng+Thal",
  "Sex+Oldpeak+Slope",
  "Sex+Oldpeak+Ca",
  "Sex+Oldpeak+Thal",
  "Sex+Slope+Ca",
  "Sex+Slope+Thal",
  "Sex+Ca+Thal",
  "ChestPain+MaxHR+ExAng",
  "ChestPain+MaxHR+Oldpeak",
  "ChestPain+MaxHR+Slope",
  "ChestPain+MaxHR+Ca",
  "ChestPain+MaxHR+Thal",
  "ChestPain+ExAng+Oldpeak",
  "ChestPain+ExAng+Slope",
  "ChestPain+ExAng+Ca",
  "ChestPain+ExAng+Thal",
  "ChestPain+Oldpeak+Slope",
  "ChestPain+Oldpeak+Ca",
  "ChestPain+Oldpeak+Thal",
  "ChestPain+Slope+Ca",
  "ChestPain+Slope+Thal",
  "ChestPain+Ca+Thal",
  "MaxHR+ExAng+Oldpeak",
  "MaxHR+ExAng+Slope",
  "MaxHR+ExAng+Ca",
  "MaxHR+ExAng+Thal",
  "MaxHR+Oldpeak+Slope",
  "MaxHR+Oldpeak+Ca",
  "MaxHR+Oldpeak+Thal",
  "MaxHR+Slope+Ca",
  "MaxHR+Slope+Thal",
  "MaxHR+Ca+Thal",
  "ExAng+Oldpeak+Slope",
  "ExAng+Oldpeak+Ca",
  "ExAng+Oldpeak+Thal",
  "ExAng+Slope+Ca",
  "ExAng+Slope+Thal",
  "ExAng+Ca+Thal",
  "Oldpeak+Slope+Ca",
  "Oldpeak+Slope+Thal",
  "Oldpeak+Ca+Thal",
  "Slope+Ca+Thal"
)
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

nine <- c("Age+Sex+ChestPain+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal")
# =================================================================================
# This function takes one of the variable combinations above and creates residual histograms and qqnorm tables. This function specifically shows
# the distribution of the observed values of our heartTest data minus the predicted values of our logistic model

plotPredictedHistogram <- function(variable) {
  for (val in variable) {
    string <- as.vector(unlist(strsplit(val,"[+]")),mode="list")
    actual_data <- as.data.frame(matrix( nrow = 50 ))
    for (word in string){
      actual_data[paste(word)] <- NULL
      
      actual_data[paste(word)] <-heartTest[[paste(word)]]
      
    }
    actual_data$V1 <-NULL
    #Creating log regression model, determining predictions then finding residuals by minusing observed values of heartTest data from the predicted
    #values of the log regression model
    glm.variable <- glm(paste("AHD ~", val), data = heartTrain, family="binomial"(link="logit"))
    pred<-predict(glm.variable, actual_data)
    res.train <- (heartTest$AHD - pred)
    hist(res.train, prob=T, breaks=9,main = paste("Using",val))
    lines(density(res.train), col="red")
    qqnorm(res.train, main = paste("Using models",val))
    qqline(res.train)
    
    anova(object=glm.variable, test="Chisq")
    BIC(glm.variable) #Higher numbers of Residual deviance indicates bad fit.  Deviance is a measure of goodness of fit of a model.
    exp(confint(glm.variable))
    res2 <- residuals(glm.variable)
    
  }
  
}
#NITIALIZED CODE COMPLETE========================================================================================

#Looping through all the model combinations to observe how well each model does predicting heartTest$AHD
pdf(file = "ProjectTwoPlots3.pdf", onefile = TRUE)
variable_list <- c(one , two, three, four, five, six, seven, eight, nine)
for(num in variable_list){
  plotPredictedHistogram(num)
}
dev.off()


# ====================================================================
# SVM MODEL
# ====================================================================
detach(df)
h2<-heartTrain
h2 <- na.omit(h2)
df<-h2

attach(df)

x <- subset(df, select =-AHD) 
y <- as.factor(AHD)

model <- svm(AHD ~.,type = "C-classification",data=df) #train an one-classification model 
print(model)
summary(model)
plot(formula=model,x,data=df)
svm_model1 <- svm(x,y)
summary(svm_model1)
pred <- predict(svm_model1,x)
table(pred,y)

svm_tune <- tune(svm, train.x=x, train.y=y, type = "C-classification",kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
plot(cmdscale(dist(df[,-14])),
     col = as.integer(df[,14]),
     pch = c("o","+")[1:150 %in% model$index + 1])
print(svm_tune)
model_after_tune <- svm(AHD ~ ., data=df, type = "C-classification",kernel="radial", cost=10, gamma=0.5)
summary(model_after_tune)
plot(formula=model_after_tune, x,data=df)

pred <- predict(model_after_tune,x)
table(pred,y)

plot(model, col = 1:1000 %in% svm_model1$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(model_after_tune,pch = "+", col = 2, cex = 5)
x <- prcomp(df[,1:14])$x[,1:2]
y <- df[,5]
df <- data.frame(cbind(x[],y[]))
machine <- svm(y ~ PC1 + PC2,type = "C-classification",kernel="radial", cost=10, gamma=0.5, data=df)
plot(machine, data=df)

library(e1071)

x <- prcomp(iris[,1:4])$x[,1:2]
y <- iris[,5]

df <- data.frame(cbind(x[],y[]))

machine <- svm(y ~ PC1 + PC2, data=df)
plot(machine, data=df)