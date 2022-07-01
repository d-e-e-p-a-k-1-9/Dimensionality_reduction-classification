library("readxl")
library("psych")
library(caret)
library(class)

Dry_Bean_Dataset <- read_excel(file.choose())
pairs.panels(Dry_Bean_Dataset[, -17],
             gap = 0,
             pch = 21)

#principal Component Analysis for Training data
pc <- princomp(Dry_Bean_Dataset[,-17],
             scores = TRUE,
             cor = TRUE)


#to find features to be selected
eig <- (pc$sdev)^2
#print(eig)

#finding the necessary scores
scores_pc <- data.frame(pc$scores[, 1:3])
#View(scores_pc)

#Classification of Data

#function to normalize the sample data
data_norm <- function(x) {
  ((x - min(x)) / (max(x) - min(x)))
}

set.seed(111)
scores_pc['Class'] <- Dry_Bean_Dataset[, 17]

#dividing training and testing data
ind <- sample(2, nrow(scores_pc), replace = TRUE, prob = c(0.7,0.3))
training <- scores_pc[ind == 1, ]
testing <- scores_pc[ind == 2,]

#normalized train and test data
beans_training <- as.data.frame(lapply(training[,-4], data_norm))
beans_testing <- as.data.frame(lapply(testing[, -4], data_norm))

#KNN classifier
beans_pred <- knn(beans_training, beans_testing, training$Class , k = 3)

#Generating Confusion Matrix
confusion_matrix <- table(testing$Class, beans_pred)
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


