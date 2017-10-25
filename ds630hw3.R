library(reshape)
library(MASS)
library(class)
library(e1071)
csv_file = '/home/amcdowald/wrksite/school/DS630/breastCancerData.csv'
breast_cancer_data <-
  read.csv(
    csv_file,
    sep = ",",
    quote = "\"",
    header = TRUE
  )
View(breast_cancer_data)
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
diag_as_numeric<-factor(breast_cancer_data$Diagnosis, levels=c("B","M"), labels=c("Benign","Malignant"))
print(sprintf("Levels are %s=1 and %s=2",levels(breast_cancer_data$Diagnosis)[1],levels(breast_cancer_data$Diagnosis)[2]))
scale_data <- scale(breast_cancer_data[3:32])
melt <- melt(cor(scale_data))
melt
melt_sub <- subset(melt, value < -.9 | value > .9 & value != 1)
melt_sub
print(unique(melt_sub$X2))

#MATRIX WITH STRONG CORRELATION
corr_list <- as.vector(unique(melt_sub$X2))
data_with_corr <-sapply(corr_list, function(name) {data_with_corr <- breast_cancer_data[[name]]})
data_with_corr <- cbind(breast_cancer_data[1:2],data_with_corr)

#CREATE FORMULAS
formula_1 <- as.formula(paste("data_with_corr$Diagnosis ~ ",paste("data_with_corr$",names(data_with_corr)[-c(1,2)], sep="",collapse ="+"),sep = ""))

#RUN GLM
predict_glm <- data_with_corr
glm_data <- glm(formula_1, data = predict_glm,family="binomial"(link="logit"))
glm_data$predict<-predict(glm_data,training_data_set)$class
summary(glm_data)
predict_table<-table(predicted=glm_data$predict,Actual=glm_data$Diagnosis)
Accuracy<-sum(diag(predict_table))/sum(predict_table)
Accuracy

#RUN LDA
train <- sample(1:nrow(data_with_corr), 96)
predict_lda <- data_with_corr
lda_data <- lda(formula_1, data = predict_lda , subset = train)
predict_lda$predict<-predict(lda_data,training_data_set)$class
abline(plot(predict_lda$predict,predict_lda$Diagnosis,
            xlab="predicted",ylab="actual"),a=0,b=1)
linearfit<-lda(diag_re~.,prc_train)
predict_table<-table(predicted=predict_lda$predict,Actual=predict_lda$Diagnosis)
Accuracy<-sum(diag(predict_table))/sum(predict_table)
Accuracy

#RUN KNN
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
#RUN SVM
old_Accuracy<-0
for (kernal_type in c("polynomial","radial","linear","sigmoid")){
  print(sprintf('%s and old accuracy=%s', kernal_type,old_Accuracy))
  svm_data <- data_with_corr[-c(1)]
  svm_model<- svm(svm_data$Diagnosis~.,kernel=sprintf('%s', kernal_type), data = svm_data)
  print(svm_model)
  svm_pred <- predict(svm_model,svm_data[-c(1)])
  table(svm_pred,svm_data$Diagnosis)
  svm_tune <- tune(svm, train.x=,svm_data[-c(1)], train.y=svm_data$Diagnosis, kernel=sprintf('%s', kernal_type),
                   ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  svm_model_tuned<- svm(svm_data$Diagnosis~.,data = svm_data, cost=svm_tune$best.parameters[1], gamma=svm_tune$best.parameters[2])
  svm_pred_tuned <- predict(svm_model_tuned,svm_data[-c(1)])
  predict_table<-table(svm_pred_tuned,svm_data$Diagnosis)
  new_Accuracy<-sum(diag(predict_table))/sum(predict_table)
  if(new_Accuracy > old_Accuracy){
    sprintf("Better accuracy than %s, using cost=%s, gamma=%s and kernal=%s",old_Accuracy,svm_tune$best.parameters[1],svm_tune$best.parameters[2],kernal_type)
    print(new_Accuracy)
    old_Accuracy<-new_Accuracy
  }
}

