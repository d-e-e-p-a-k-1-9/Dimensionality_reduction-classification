library("readxl")
library("infotheo")
library("praznik")

bean_data <- read_excel(file.choose())

## Forward Feature Selection using CCMI method
features <- colnames(bean_data)
typeof(features)
feature_list <- names(bean_data)
features <- head(features,-1)
selected_features <- c()
relevance_list <- c()
dat <- discretize(bean_data)
for (x in 1:16){
  mutinfo <-  mutinformation(unlist(dat[,x]),dat$Class, method="emp")
  print("Mutual Informations")
  print(mutinfo)
  relevance_list[x] <- mutinfo
}
feature_index <- c()
feature1 <- which(relevance_list == max(relevance_list))
selected_features[1] = features[feature1]
feature_index[1] <- feature1  
features <- features[-feature1]

count = 1

while (count<10) {
  r <- 16 - count
  J_list <- c()
  for (m in 1:r){
    condinfo_list <- c()
    mutinfo_list <- c()
    corrcoeff_list <- c()
    for (i in 1:count){
      condinfo <- condinformation(dat[,features[m]],dat[,tail(features, n=1)],dat[,selected_features[i]])
      corrcoeff <- cov(bean_data[,features[m]],bean_data[,selected_features[i]])/(sqrt(var(bean_data[,features[m]]))*sqrt(var(bean_data[,selected_features[i]])))
      mutinfo_pairs <- mutinformation(dat[,features[m]],dat[,selected_features[i]])
      condinfo_list[i] <- condinfo
      mutinfo_list[i] <- mutinfo_pairs
      corrcoeff_list[i] <- corrcoeff
    }
    
    condinfo_min <- min(condinfo_list)
    mutinfo_min <- min(mutinfo_list)
    corrcoeff_min <- min(corrcoeff_list)
    J_CCMI <- condinfo_min - abs(corrcoeff_min)*(mutinfo_min)
    J_list[m] <- J_CCMI
  }
  feature_select <- which(J_list == max(J_list))
  selected_features[count+1] <- features[feature_select]
  feature_index[count+1] <- which(feature_list == features[feature_select])
  features <- features[-feature_select]
  count <- count + 1
}

print("Selected sorted Features")
print(selected_features)

print(" Remaining Features")
print(features)
beans <- bean_data[,feature_index]
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
beans['Class'] <- bean_data[, 17]
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



