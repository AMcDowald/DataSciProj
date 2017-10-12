install.packages('dplyr') # Only have to run once.
library(dplyr)
library(reshape2)

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

test_df <- factor_data
test_df$Date <- as.Date(factor_data$calendardate,format= c("%Y-%m-%d"))
data_file_names <- names(test_df)
data_file_dates <- unique(test_df$calendardate)

data_by_uniq_date <- vector()
oneNaPerRow_cases <- vector()
morethatonenaPerRow_cases <- vector()
fit<-vector()

for(date in seq(1:length(data_file_dates))){
  dtfi_name <- paste("dataByDate_",gsub('-','',data_file_dates[date]),sep='')
  assign(dtfi_name, subset(test_df, test_df$Date == as.Date(data_file_dates[date])))
  data_by_uniq_date <- c(data_by_uniq_date,dtfi_name)
}
#Get Coorelation list
correlation_vec <- vector()
for(dataframe in oneNaPerRow_cases ){
  print(dataframe)
  dtfi_name <- sprintf("Corr_%s",dataframe )
  df.tmp <- get(dataframe)
  lm_df <- df.tmp[5:ncol(df.tmp)]
  d_cor <- as.matrix(cor(lm_df ))
  d_cor_melt <- melt(d_cor)
  melt_sub<- subset(d_cor_melt, value > .5 & value != 1)
  assign(dtfi_name, melt_sub)
  correlation_vec <- c(correlation_vec,dtfi_name)
}


#Keeping rows with no NA in the row.
for(date in data_by_uniq_date){
  df = get(date)
  df.temp = df
  no_of_nas_in_each_row=0
  dtfi_name <- sprintf("Only_One_NA_PerRow_%s",date )
  dtfi_name2 <- sprintf("not_Only_One_NA_PerRow_%s",date )
  log <- apply(df.temp, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) == no_of_nas_in_each_row)
  oneNa = df[logindex, ]
  log <- apply(df.temp, 2, is.nan)
  logindex <- apply(log, 1, function(x) sum(x) == no_of_nas_in_each_row)
  oneNaPerRow = oneNa[logindex, ]
  morethatonenaPerRow=df[!logindex, ]
  oneNaPerRow.scale <- scale(df3[6:94])
  oneNaPerRow <- cbind(df3[1:4],oneNaPerRow.scale)
  assign(dtfi_name,oneNaPerRow)
  assign(dtfi_name2,morethatonenaPerRow)
  oneNaPerRow_cases <- c(oneNaPerRow_cases,dtfi_name)
  morethatonenaPerRow_cases <- c(morethatonenaPerRow_cases,dtfi_name2)
}


View(get(correlation_vec[1]))



for(dataframe in oneNaPerRow_cases ){
  print(sprintf("DATAFRAME: %s", dataframe))
  df.tmp <- get(dataframe)
  lm_df <- df.tmp[5:ncol(df.tmp)]
  lm_df$invcap<-NULL
  
  d_cor <- as.matrix(cor(lm_df ))
  d_cor_melt <- arrange(melt(d_cor), -abs(value))
  
  formula<- vector()
  #create column names dataframe and create an array without ln_returns
  dfnam <- names(lm_df)
  df <- dfnam[2:length(dfnam)]
  #Create formula
  b <- paste("lm_df$",df, sep="",collapse ="+")
  form2 <- paste("lm_df$ln_returns ~ ",b,sep = "")
  #Linear model
  fitO <- lm(as.formula(form2), na.action=na.exclude)
  #Create residuals and histogram
  res <- residuals(fitO)
  hist(res,prob=T,breaks=9, xlab="Residuals", main=sprintf("Histogram of the residuals\n %s",dataframe) )
  lines(density(res),col="red")
  print(summary(fitO))
  fit<- c(fit,summary(fitO))
}



d_cor_melt <- arrange(melt(d_cor), -abs(value))

css <- as.matrix(lm_df )
rcorr(ccs, type="pearson")

cor.test(training_set$Theft,training_set$Subway, method="pearson")
plot(training_set$Theft,training_set$Subway)
abline(lm(training_set$Theft~training_set$Subway), col="red")

print(sprintf("DATAFRAME: %s", dataframe))

ctable = table.Correlation(lm_df[1],lm_df[,2:ncol(lm_df),drop=FALSE], conf.level=.99)
lm_df$invcap<-NULL

which(d_cor > 0.15 & lower.tri(d_cor), arr.ind = T, useNames = F)


subset(d_cor, value > .5)
l <- apply(X=lm_df, MARGIN = 2, FUN = function(x) cor.test(x[2], x[6]))
cor <-apply(lm_df, MARGIN = 1, FUN = function(x) return(cor.test(x[1:44], x[45:88])$estimate))


lm_df$ln_returns

l <- apply(d_cor,2, function(x) x >.5)
df2 <- d_cor[l]
sd <-subset(d_cor, value > .5)
d_cor_melt <- arrange(melt(d_cor), -abs(value))
lm_df[,2]
df1 =lm_df[,2] #lm_df[,2:ncol(lm_df),drop=FALSE]
df1 = as.numeric(df1[,1])
cor.test(lm_df[,2],lm_df[,3])

data(managers)
table.Correlation(managers[,1:6],managers[,7:8])
ctable = table.Correlation(managers[,1:6],managers[,8,drop=FALSE], conf.level=.99)