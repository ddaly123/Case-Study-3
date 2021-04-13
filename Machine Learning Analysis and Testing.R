cancer <- read.csv("C:\\Users\\ddaly\\Documents\\Daly_CS3_DS501\\Data\\breast-cancer.data", header = FALSE)
colnames(cancer) <- c("Class", "Age", "Menopause", "Tumor_Size", "Inv_Nodes", "Node_Caps", "Deg-Malig", "Breast", "Breast_Quad", "Irradiat")
head(cancer)
dim(cancer)
sapply(cancer, class)
levels(cancer$class)

percentage <- prop.table(table(cancer$Class)) * 100
cbind(freq=table(cancer$Class), percentage=percentage)

summary(cancer)

x <- cancer[,2:10]
y <- cancer[,1]


par(mfrow=c(2,4))
for(i in 2:9) {
  counts <- table(cancer[,i])
  print(counts)
  name <- names(cancer[i])
  barplot(counts, main=name)
}
library(caret)
colnames(cancer) <- make.names(colnames(cancer))
control <- trainControl(method='cv', number = 10)
metric = "Accuracy"
valid_index <- createDataPartition(cancer$Class, p= 0.8, list=FALSE)
valid <- cancer[-valid_index,]
cancer <- cancer[valid_index,]
# a) linear algorithms
set.seed(7)
fit.lda <- train(Class~., data=cancer, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Class~., data=cancer, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Class~., data=cancer, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Class~., data=cancer, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Class~., data=cancer, method="rf", metric=metric, trControl=control)

#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
print(fit.knn)
#knn proves to be the most accurate model to display the data

predictions <- predict(fit.knn, valid)
confusionMatrix(predictions, as.factor(valid$Class))

