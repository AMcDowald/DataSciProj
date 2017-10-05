###WORKING
csv_file = '/home/amcdowald/Downloads/Data_file.csv'
factor_data <-
  read.csv(
    csv_file,
    sep = ",",
    quote = "\"",
    header = TRUE
  )

View(factor_data)

ln_returns <- vector()
for(i in 1:length(factor_data$assets)){
  if (identical(factor_data$ticker[i+1],factor_data$ticker[i])){
    ln_returns[i] = log(factor_data$price[i+1]/factor_data$price[i])
  }
  else{
    ln_returns[i] = -9999999
  }
  
}
factor_data <- cbind(ln_returns, factor_data)
factor_data_reduced <- subset(factor_data, ln_returns > -100)
factor_data$calendardate <- as.Date(factor_data$calendardate,format= c("%Y-%m-%d"))
factor_data_reduced <- subset(factor_data, factor_data$calendardate == as.Date("2011-12-31"))
View(factor_data_reduced)
df1 = factor_data_reduced


#END

#TEST
df1[complete.cases(df1),]


df1[is.na(df1)] <- NA
df2 <- na.omit(df1)
View(df1)
model1 <- lm(formula = ln_returns ~ revenueusd, data=df2)

print(length(df1$revenueusd))
