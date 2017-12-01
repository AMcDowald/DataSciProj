###WORKING Project
csv_file = '/home/amcdowald/Downloads/Data_file.csv'
factor_data <-
  read.csv(
    csv_file,
    sep = ",",
    quote = "\"",
    header = FALSE
  )
colnames(factor_data) <- c("ticker","dimension","calendardate","datekey","reportperiod","accoci","assets","assetsavg","assetsc","assetsnc","assetturnover","bvps","capex","cashneq","cashnequsd","cor","currentratio","de","debt","debtusd","depamor","divyield","dps","ebit","ebitda","ebitdamargin","ebitdausd","ebitusd","ebt","eps","epsdil","epsusd","equity","equityavg","equityusd","ev","evebit","evebitda","fcf","fcfps","fxusd","gp","grossmargin","intangibles","intexp","invcap","invcapavg","inventory","liabilities","liabilitiesc","liabilitiesnc","marketcap","ncf","ncfcommon","ncfdebt","ncfdiv","ncff","ncfi","ncfo","ncfx","netinc","netinccmn","netinccmnusd","netincdis","netmargin","payables","payoutratio","pb","pe","pe1","prefdivis","price","ps","ps1","receivables","retearn","revenue","revenueusd","rnd","roa","roe","roic","ros","sgna","sharefactor","sharesbas","shareswa","shareswadil","sps","tangibles","taxexp","tbvps","workingcapital")
x <- c("ticker","dimension","calendardate","datekey","reportperiod","accoci","assets","assetsavg","assetsc","assetsnc","assetturnover","bvps","capex","cashneq","cashnequsd","cor","currentratio","de","debt","debtusd","depamor","divyield","dps","ebit","ebitda","ebitdamargin","ebitdausd","ebitusd","ebt","eps","epsdil","epsusd","equity","equityavg","equityusd","ev","evebit","evebitda","fcf","fcfps","fxusd","gp","grossmargin","intangibles","intexp","invcap","invcapavg","inventory","liabilities","liabilitiesc","liabilitiesnc","marketcap","ncf","ncfcommon","ncfdebt","ncfdiv","ncff","ncfi","ncfo","ncfx","netinc","netinccmn","netinccmnusd","netincdis","netmargin","payables","payoutratio","pb","pe","pe1","prefdivis","price","ps","ps1","receivables","retearn","revenue","revenueusd","rnd","roa","roe","roic","ros","sgna","sharefactor","sharesbas","shareswa","shareswadil","sps","tangibles","taxexp","tbvps","workingcapital")
length(x)
View(factor_data)
#extracting ARQ
arq_data<- subset(factor_data, factor_data$dimension == 'ARQ')

ln_returns <- vector()
for(i in 1:length(arq_data$ticker)){
  if (identical(arq_data$ticker[i+1],arq_data$ticker[i])){
    ln_returns[i] = log(arq_data$price[i+1]/arq_data$price[i])
  }
  else{
    ln_returns[i] = 9999999
  }
  
}
!is.nan(ln_returns)
#Creating A dataframe using 3 indicators and Log_Returns
indicator_df <- data.frame(ticker=arq_data$ticker, calendardate=arq_data$calendardate,Log_Return=ln_returns, Assets=arq_data$assets, Revenue_USD=arq_data$revenueusd,Net_Margin=arq_data$netmargin,
                           ebtusd=arq_data$ebitusd, netinc=arq_data$netinc, assetturnover=arq_data$assetturnover, currentratio=arq_data$currentratio, pe1=arq_data$pe1, roic=arq_data$roic, roa=arq_data$roa, de= arq_data$de,
                           ncfo=arq_data$ncfo, workingcapital=arq_data$workingcapital, sharesbas=arq_data$sharesbas, fcf=arq_data$fcf, grossmargin=arq_data$grossmargin,
                           ev=arq_data$ev, ebitusd=arq_data$ebitusd, liabilities=arq_data$liabilities, equityavg=arq_data$equityavg)
indicator_df<-cbind(Log_Return=ln_returns,arq_data)
#Removing -9999999 from ln_returns
factor_data_reduced <- subset(indicator_df, is.finite(ln_returns) & ln_returns != 9999999)

df.temp<-factor_data_reduced 
#Removing nas by 5% per column
df_remove_nas <- df.temp[ lapply(df.temp, function(x) sum(is.na(x)) / length(x)) <0.05]
#Omit, Scale and combine

df_remove_nas<-na.omit(df_remove_nas)
result_scaled<-scale(df_remove_nas[-c(1,2,3)])
result_scaled<- cbind(df_remove_nas[c(1,2,3)],result_scaled)
#Format data set
result_scaled$calendardate <- as.Date(result_scaled$calendardate,format= c("%Y-%m-%d"))
data_file_dates <- unique(result_scaled$calendardate)
data_file_dates
test_df<-result_scaled

#Creating Datasets separated by date
data_by_uniq_date <- vector()
for(date in seq(1:length(data_file_dates))){
  dtfi_name <- paste("data_By_Date_",gsub('-','',data_file_dates[date]),sep='')
  assign(dtfi_name, subset(test_df, test_df$calendardate == as.Date(data_file_dates[date])))
  data_by_uniq_date <- c(data_by_uniq_date,dtfi_name)
}

fiveteen_datasets_by_date<-c('data_By_Date_20120630', 'data_By_Date_20150930','data_By_Date_20131231','data_By_Date_20110930',
             'data_By_Date_20110331','data_By_Date_20110630','data_By_Date_20110930','data_By_Date_20111231',
             'data_By_Date_20120331','data_By_Date_20120930','data_By_Date_20121231','data_By_Date_20130331',
             'data_By_Date_20130630','data_By_Date_20130930','data_By_Date_20140331','data_By_Date_20140630',
             'data_By_Date_20130630','data_By_Date_20110930')
#Linear model for All data sets
vector_of_coef<- vector()
t=1
for(data in sort(data_by_uniq_date[0:15])){
  print(data)
  df.tmp <- get(data)
  formula<- vector()
  dfnam <- names(df.tmp[-c(1,2,3)])
  df <- dfnam[2:length(dfnam)]
  b <- paste("df.tmp$",df, sep="",collapse ="+")
  form2 <- as.formula(paste("df.tmp$Log_Return ~ ",b,sep = ""))
  fitO <- lm(form2, na.action=na.omit)
  print(summary(fitO))
  print(fitO$coefficients)
  dtfi_name <- data#paste("t",t,sep='')
  t=t+1
  assign(dtfi_name, fitO$coefficients)
  vector_of_coef <- c(vector_of_coef,dtfi_name)
}

x=stack(data.frame(cbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)))

#y=as.data.frame(t(data.frame(as.list(sort(data_by_uniq_date[0:15])))))
new=x[-c(12)]
new=na.omit(new)
arima(new[,1], order = c(13,0,2))
plot(lm(new))

model <- auto.arima(new[,1])


plot(new[,1])
View(new)
x<-lm(new)
summary(x)
plot(x)




###















##Linear model for the 15 datasets
vector_of_coef<- vector()
t=1
for(data in sort(fiveteen_datasets_by_date)){
  print(data)
  df.tmp <- get(data)
  formula<- vector()
  dfnam <- names(df.tmp[-c(1,2,3)])
  df <- dfnam[2:length(dfnam)]
  b <- paste("df.tmp$",df, sep="",collapse ="+")
  form2 <- as.formula(paste("df.tmp$Log_Return ~ ",b,sep = ""))
  fitO <- lm(form2, na.action=na.omit)
  
  print(fitO$coefficients)
  dtfi_name <- paste("t",t,sep='')
  t=t+1
  assign(dtfi_name, fitO$coefficients)
  vector_of_coef <- c(vector_of_coef,dtfi_name)
}

x=data.frame(cbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15))
x=t(x)
y=x[c(15)]
