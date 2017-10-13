library(reshape)
library(MASS)
library(class)
csv_file = '/home/amcdowald/wrksite/school/DS630/breastCancerData.csv'
breast_cancer_data <-
  read.csv(
    csv_file,
    sep = ",",
    quote = "\"",
    header = TRUE
  )
#Creating Random Sample
sample_number <- sample(100:400, 1, replace=FALSE)
index <- sample(1:nrow(breast_cancer_data), sample_number)
training_data_set <- breast_cancer_data[index,]

#Separating into mean, standard and worse
mean_matrix <- breast_cancer_data[3:12]
stder_matrix <- breast_cancer_data[13:22]
worst_matrix <- breast_cancer_data[23:32]
data_vector_1 <- mget( ls( pattern = "^.*_matrix$" ) )

#CORRELATION:
for(data_f in names(data_vector_1)){
  print(sprintf("Corelation for '%s\'",data_f))
  data_f <- get(data_f)
  melt = melt(cor(data_f))
  melt_sub<- subset(melt, value > .9 & value != 1)
  print(melt_sub)
}
diag_as_numeric<-as.numeric(as.factor(training_data_set$Diagnosis))
print(sprintf("Levels are %s=1 and %s=2",levels(training_data_set$Diagnosis)[1],levels(training_data_set$Diagnosis)[2]))
melt = melt(cor(diag_as_numeric,training_data_set[3:32]))
melt_sub<- subset(melt, value > .7 & value != 1)
print(melt_sub)

#MATRIX WITH STRONG CORRELATION
corr_list <- as.vector(melt_sub[2]$X2)
data_with_corr <-sapply(corr_list, function(name) {data_with_corr <- training_data_set[[name]]})
data_with_corr <- cbind(training_data_set[1:2],data_with_corr)

#CREATE FORMULAS
formula_1 <- as.formula(paste("data_with_corr$Diagnosis ~ ",paste("data_with_corr$",names(data_with_corr)[-c(1,2)], sep="",collapse ="+"),sep = ""))

#RUN GLM
glm_data <- glm(formula_1, data = data_with_corr,family=binomial())
summary(glm_data)

#RUN LDA
train <- sample(1:nrow(data_with_corr), 96)
table(data_with_corr$Diagnosis[train])
predict_lda <- data_with_corr
data <- lda(formula_1, data = predict_lda , subset = train)
predict_lda$predict<-predict(data, predict_lda)$class
abline(plot(predict_lda$predict,predict_lda$Diagnosis,
            xlab="predicted",ylab="actual"),a=0,b=1)

#knn inc
#https://rstudio-pubs-static.s3.amazonaws.com/123438_3b9052ed40ec4cd2854b72d1aa154df9.html
train <- 1:100
train_data <- data_with_corr[train,][3:8]
test_data <- data_with_corr[-train,][3:8]
train.def <- factor(data_with_corr$Diagnosis[train])
test.def <- factor(data_with_corr$Diagnosis[-train])

knn.1 <-  knn(train_data, test_data, train.def, k=1)
knn.5 <-  knn(train_data, test_data, train.def, k=5)
knn.20 <- knn(train_data, test_data, train.def, k=20)

100 * sum(test.def == knn.1)/100 
100 * sum(test.def == knn.5)/100
100 * sum(test.def == knn.20)/100

table(knn.1 ,test.def)
table(knn.5 ,test.def)
table(knn.20 ,test.def)

gc.bkup <- data_with_corr
plot(train_data[,names(train_data)],
     col=gc.bkup[-train,],
     pch=as.numeric(train.def),
     main="Predicted Default, by 1 Nearest Neighbors",cex.main=.95)

points(test_data[,names(train_data)],
       bg=gc.bkup[-train,],
       pch=c(21,24)[as.numeric(knn.1)],cex=1.2,col=grey(.7))

legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),
       legend=c("data 0","pred 0","data 1","pred 1"),
       title="default",bty="n",cex=.8)

legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),
       title="installment %", horiz=TRUE,bty="n",col=grey(.7),cex=.8)
#----------------
plot(train_data[,names(train_data)[1:2]],
     col=c(4,3,6,2)[gc.bkup[-train, "worst_radius"]],
     pch=c(1,2)[as.numeric(train.def)],
     main="Predicted Default, by 1 Nearest Neighbors",cex.main=.95)

points(test_data[,names(train_data)[1:2]],
       bg=c(4,3,6,2)[gc.bkup[-train,"worst_radius"]],
       pch=c(21,24)[as.numeric(knn.1)],cex=1.2,col=grey(.7))

legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),
       legend=c("data 0","pred 0","data 1","pred 1"),
       title="default",bty="n",cex=.8)

legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),
       title="installment %", horiz=TRUE,bty="n",col=grey(.7),cex=.8)
#svm
