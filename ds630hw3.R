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
train_data <- data_with_corr[train,]
test_data <- data_with_corr[-train,]
cl <- factor(train_data$Diagnosis)
x<-knn(train_data, test_data, cl, k = 10, prob=TRUE)
summary(x)
data_with_corr
#svm