###WORKING HW 3
csv_file = '/home/amcdowald/Downloads/Data_file.csv'
factor_data <-
  read.csv(
    csv_file,
    sep = ",",
    quote = "\"",
    header = TRUE
  )

print(names(factor_data))
test_df <- factor_data
short_df <- indicator_df
ln_returns <- vector()
for(i in 1:length(factor_data$assets)){
  if (identical(factor_data$ticker[i+1],factor_data$ticker[i])){
    ln_returns[i] = log(factor_data$price[i+1]/factor_data$price[i])
  }
  else{
    ln_returns[i] = 9999999
  }
  
}

factor_data <- cbind(factor_data[1:5], ln_returns,factor_data[6:length(names(factor_data))])

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
  print(df_name)test_df
  model_vec[[df_name]]<- lm(formula = indicator_df$Log_Returns ~ indicator_df[[df_name]], data=indicator_df)
  plot(indicator_df$Log_Returns, indicator_df[[df_name]], main=paste("Scatterplot for lm(log_returns~", df_name,")"),
       xlab="Log_Returns ", ylab=paste(df_name), pch=19)
  abline(model_vec[[df_name]], col="red" )
  print(summary(model_vec[[df_name]]))
}
####FUNCTIONS#############################################
#' keep rows that have a certain number (range) of NAs anywhere/somewhere and delete others
#' @param df a data frame
#' @param col restrict to the columns where you would like to search for NA; eg, 3, c(3), 2:5, "place", c("place","age")
#' \cr default is NULL, search for all columns
#' @param n integer or vector, 0, c(3,5), number/range of NAs allowed.
#' \cr If a number, the exact number of NAs kept
#' \cr Range includes both ends 3<=n<=5
#' \cr Range could be -Inf, Inf
#' @return returns a new df with rows that have NA(s) removed
#' @export
ez.na.keep = function(df, col=NULL, n=0){
  if (!is.null(col)) {
    # R converts a single row/col to a vector if the parameter col has only one col
    # see https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/#comments
    df.temp = df[,col,drop=FALSE]
  } else {
    df.temp = df
  }
  
  if (length(n)==1){
    if (n==0) {
      # simply call complete.cases which might be faster
      result = df[complete.cases(df.temp),]
    } else {
      # credit: http://stackoverflow.com/a/30461945/2292993
      log <- apply(df.temp, 2, is.na)
      logindex <- apply(log, 1, function(x) sum(x) == n)
      result = df[logindex, ]
    }
  }
  
  if (length(n)==2){
    min = n[1]; max = n[2]
    log <- apply(df.temp, 2, is.na)
    logindex <- apply(log, 1, function(x) {sum(x) >= min && sum(x) <= max})
    result = df[logindex, ]
  }
  
  return(result)
}
##################################################TEST

#Create list
test_df <- factor_data
test_df$Date <- as.Date(factor_data$calendardate,format= c("%Y-%m-%d"))
data_file_names <- names(test_df)
data_file_dates <- unique(test_df$calendardate)

  
data_by_uniq_date <- vector()
data_by_uniq_date_cases <- vector()
not_data_by_uniq_date_cases <- vector()

for(date in seq(1:length(data_file_dates))){
    dtfi_name <- paste("dataByDate_",gsub('-','',data_file_dates[date]),sep='')
    assign(dtfi_name, subset(test_df, test_df$Date == as.Date(data_file_dates[date])))
    data_by_uniq_date <- c(data_by_uniq_date,dtfi_name)
}
for(date in data_by_uniq_date){
  df = get(date)
  df.temp = df
  dtfi_name <- sprintf("comp_case_for_%s",date )
  log <- apply(df.temp, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) == 1)
  result = df[logindex, ]
  assign(dtfi_name,result)
  data_by_uniq_date_cases <- c(data_by_uniq_date_cases,dtfi_name)
  #dtfi_name <- sprintf("comp_case_for_%s",date )
  #dtfi_name2 <- sprintf("not_comp_case_for_%s",date )
  #assign(dtfi_name, df[complete.cases(df.temp),])
  #assign(dtfi_name2, df[!complete.cases(df.temp),])
  #data_by_uniq_date_cases <- c(data_by_uniq_date_cases,dtfi_name)
  #not_data_by_uniq_date_cases <- c(not_data_by_uniq_date_cases,dtfi_name2)
}


print(data_by_uniq_date_cases)

print(apply(df.temp, 2, is.na))



for(date in data_by_uniq_date){
  print(length(data_by_uniq_date))
  
}
##################################################

#Taking all rows that have 2011-12-31 date
factor_data_reduced <- subset(indicator_df, indicator_df$Date == as.Date("2011-12-31"))
df <- test_df[rowSums(is.na(test_df) == 0)]


View(data_file_dates)

no_na_test_df<-test_df[rowSums(is.na(test_df))]
test_df_without_na<-test_df[~is.na(test_df)]
no_na_test_df<-factor_data[!complete.cases(factor_data),]
no_na_test_df = test_df[!complete.cases(test_df),]
colSums(is.na(factor_data))
no_na_test_df<-colnames(test_df)[colSums(is.na(test_df)) > 0]
View(no_na_test_df)





View(short_df)

for (i in seq_along(mylist)){
  +   print(paste(i,names(mylist)[i],mylist[[i]]))

}


print(fit)
df.tmp <- get(oneNaPerRow_cases[1])
df.tmp <- df.tmp.start[complete.cases(df.tmp.start),]
lm_df <- df.tmp[5:ncol(df.tmp)]
formula<- vector()
dfnam <- names(lm_df)
df <- dfnam[2:length(dfnam)]
b <- paste("lm_df$",df, sep="",collapse ="+")
form2 <- paste("lm_df$ln_returns ~ ",b,sep = "")
fitO <- lm(as.formula(form2), na.action=na.omit)
fit<- c(fit,fitO)


for(name in names(lm_df)){
  if(identical(name,'ln_returns')) {
    print(name)
    formula <- r(formula,sprintf("lm_df$%s~",name) )
    print(formula)
  }
  else {
    formula <- c(formula,sprintf("lm_df$%s",name) )
  }
  
}
fitO <- lm(as.formula(formula), na.action=na.omit)
print(formula)




df3 <- get(oneNaPerRow_cases[1])
scale.df3 <- 
  print(scale.df3)

log <- apply(df3, 1, is.na)

df3[ , apply(df3, 2, function(x) !any(is.na(x)))]
View(df3)
subset(df3, !is.na())
print(df3[!log,])
#logindex <- apply(log, 1, function(x) sum(x) == no_of_nas_in_each_row)



View()
print(oneNaPerRow_cases)

print(apply(df.temp, 2, is.na))