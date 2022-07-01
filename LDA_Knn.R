library("readxl")
library("matlib")

bean_data <- read_excel(file.choose())

#Linear Discriminant Analysis
n_selectedfeatures <- 10
feature_const <- names(bean_data)
feature_const <- head(feature_const,-1)
n_features <- length(names(bean_data)) - 1
class_labels <- unique(bean_data$Class)
mean_overall <- colMeans(bean_data[,1:16])
S_w <- matrix(0,n_features,n_features)
S_b <- matrix(0,n_features,n_features)

for (c in class_labels){
  X_c <- subset(bean_data, Class == c, select = feature_const)
  mean_c <- colMeans(X_c)
  mat_trans <- data.matrix(t(X_c - mean_c))
  mat <- data.matrix(X_c - mean_c)
  S_w <- mat_trans%*%mat
  mean_diff <- data.matrix(mean_c - mean_overall)
  n_c <- nrow(X_c)
  S_b <- S_b + drop(n_c)*(mean_diff)%*%t(mean_diff) 
}

A <- inv(S_w)%*%S_b
e <- eigen(A)
eigenvalues <- e$values
eigenvectors <- e$vectors
indices <- order(eigenvalues, decreasing = T)
eigenvalues <- eigenvalues[indices]
eigenvectors <- eigenvectors[,indices]
linear_discriminants <- eigenvectors[,1:n_selectedfeatures]
final_features <- data.matrix(bean_data[,1:16])%*%linear_discriminants

beans <- matrix(, nrow = nrow(final_features), ncol = ncol(final_features))

#to extract only the real entries of the matrix
for (i in 1: nrow(beans)){
  for (j in 1: ncol(beans)){
    beans[i,j] <- Re(final_features[i,j])
  }
}
print(beans)


##Classification of Data
library(caret)
library(class)


#function to normalize the sample data
data_norm <- function(x) {
  ((x - min(x)) / (max(x) - min(x)))
}

## Training and Testing data split
smp_size <- floor(0.70 * nrow(beans))
set.seed(123)
beans <- cbind(beans, bean_data[, 17])
#seq_len(nrow(beans))
train_index <- sample(seq_len(nrow(beans)), size = smp_size)
train_data <- beans[train_index, ]
test_data <- beans[-train_index, ]

#normalized train and test data
beans_training <- as.data.frame(lapply(train_data[, -11], data_norm))
beans_testing <- as.data.frame(lapply(test_data[, -11], data_norm))

#KNN classifier
beans_pred <- knn(beans_training, beans_testing, train_data$Class , k = 3)

#Generating Confusion Matrix
confusion_matrix <- table(test_data$Class, beans_pred)
print(confusion_matrix)
plot(beans_pred)

#Predicting class for Test data
predicted_beans_test <- beans_testing
predicted_beans_test$Class <- beans_pred
View(predicted_beans_test)

#Accuracy of the Classifier
knn_accuracy <- sum(diag(confusion_matrix))*100 / nrow(beans_testing)

#Macro_Precision
tot <- 0
cols <- colSums(confusion_matrix)
for (i in 1:ncol(confusion_matrix)){
  
  value <- (confusion_matrix[i,i] / cols[i])*100
  tot <- tot + value
  
}
Macro_Pre <- tot / ncol(confusion_matrix)

#Macro_Recall
tot1 <- 0
rows <- rowSums(confusion_matrix)
for (j in 1:nrow(confusion_matrix)){
  
  value1 <- (confusion_matrix[j,j] / rows[j])*100
  tot1 <- tot1 + value1
  
}
Macro_Re <- tot1 / nrow(confusion_matrix)

#Macro_F1
F_score <- (2*Macro_Pre*Macro_Re) / (Macro_Pre + Macro_Re)

