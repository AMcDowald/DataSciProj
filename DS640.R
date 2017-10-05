###WORKING HW 3
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
    ln_returns[i] = 9999999
  }
  
}

#Creating A dataframe using 3 indicators and Log_Returns
indicator_df <- data.frame(Assets=factor_data$assets, Revenue_USD=factor_data$revenueusd,Net_Margin=factor_data$netmargin,Log_Returns=ln_returns)
#Removing -9999999 from ln_returns
factor_data_reduced <- subset(indicator_df, ln_returns != 9999999)
#Formating calenderdate to date format.
indicator_df$Date <- as.Date(factor_data$calendardate,format= c("%Y-%m-%d"))
#Taking all rows that have 2011-12-31 date
factor_data_reduced <- subset(indicator_df, indicator_df$Date == as.Date("2011-12-31"))
#Omitting NAs and using rows [4491:4699,]
df <- factor_data_reduced[!is.infinite(rowSums(factor_data_reduced[1:4])),]
indicator_df = na.omit(df)
#Create list
model_vec <- list()
#Get LM for each column in indicator_df that is not Date or Log_Returns, plot and summarize data.
for(df_name in names(indicator_df)[1:(length(names(indicator_df))-2)]){
  print(df_name)
  model_vec[[df_name]]<- lm(formula = indicator_df$Log_Returns ~ indicator_df[[df_name]], data=indicator_df)
  plot(indicator_df$Log_Returns, indicator_df[[df_name]], main=paste("Scatterplot for lm(log_returns~", df_name,")"),
       xlab="Log_Returns ", ylab=paste(df_name), pch=19)
  abline(model_vec[[df_name]], col="red" )
  print(summary(model_vec[[df_name]]))
}




