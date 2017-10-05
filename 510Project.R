setwd("/home/amcdowald/Downloads")
mpg.data <- read.table('auto-mpg.data')


par(mar=c(2,2,2,2))
par(mfrow=c(2,2))
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
outlier_values <- boxplot.stats(mpgTest$mpg)$out
boxplot(mpgTrain$mpg, main="Pressure Height", boxwex=0.1, horizontal=TRUE)
text(x=fivenum(mpgTrain$mpg), labels =fivenum(mpgTrain$mpg), y=1.25)

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Examine shape of variable distributions:
library(reshape2)
library(ggplot2)
d <- melt(mpgTrain[,-c(2,8,9)])
d.2 <- melt(mpgTrain[,c(2,8)])
#Wieght, Cylinders,displacement




##################################3 
boxdata <- with(mpgTrain$mpg,boxplot(mpgTrain$mpg, range=4))

#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i], which(data$dependent==boxdata$out[i]),pos=4)
}

###################################
plot(mpgTrain$cylinders + mpgTrain$displacement + mpgTrain$weight , mpgTrain$mpg, xlab='Cylinders+Displacement+Weight' ,ylab='MPG', main="MPG vs Cylinders")
abline(lm(mpgTrain$mpg ~ (mpgTrain$cylinders + mpgTrain$displacement + mpgTrain$weight) ), col="red")

md = lm(formula = mpg ~ weight + cylinders + acceleration, data = mpgTest )
lines(df$Income, coef(md)[1] + coef(md)[2]*df$Income, col="red")
summary(md)
coef(md)
plot(md)
hist(md)
