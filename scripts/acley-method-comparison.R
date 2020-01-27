# My first try of training classic random forest
library(randomForest)
library(rpart)


# Load data
avilatr = read.csv("data/avila/avila-tr.txt", header = T, sep = ",")
avilats = read.csv("data/avila/avila-ts.txt", header = T, sep = ",")


dataset <- rbind(avilatr,avilats)
dataset$Class <- as.factor(dataset$Class)


dataset <- dataset[sample(nrow(dataset)),]
train_indices <- 1:round(0.7 * nrow(dataset))
trainset <- dataset[train_indices,]
test_indices <- round(0.7 * nrow(dataset)):nrow(dataset)
testset <- dataset[test_indices,]

# Grow a forest
forest <- randomForest(
  Class ~ .,
	data = trainset
)

pred_forest <- predict(forest,newdata = testset)
confusionMatrix(pred_forest,testset$Class)

# Get some feedback

(confusion_matrix <- table(pred_forest,testset$Class))
(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
(precision <- mean(diag(confusion_matrix) / rowSums(confusion_matrix)))
(recall <- mean((diag(confusion_matrix) / colSums(confusion_matrix))))
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
precision <-  diag(confusion_matrix) / rowSums(confusion_matrix)
(f_score <- mean(ifelse(is.nan(2*precision*recall/(precision+recall)),0,2*precision*recall/(precision+recall))))



library(devtools)
install('.')
library(mowRandomForest)
library(rpart)
library(parallel)

#grow a custom forest
mowForest <- mowRandomForest(
  df = trainset,
  formula = Class ~.,
  ntree = 50,
  complexity = -1,
  subsetRatio = 1,
  zratio = 0.3
)

mow_forest_preeds <- predict(mowForest,newData = testset)


mow_forest_preeds <- (factor(mow_forest_preeds))
# Get some feedback
levels(mow_forest_preeds) <- c("A","W","X","Y","B","C","D","E","F","G","H","I")
mow_forest_preeds <- factor(mow_forest_preeds, levels(mow_forest_preeds)[c(1,5:12,2:4)])

confusionMatrix(mow_forest_preeds,testset$Class)


(confusion_matrix <- table(mow_forest_preeds,testset$Class))
(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
(precision <- mean(diag(confusion_matrix) / rowSums(confusion_matrix)))
(recall <- mean((diag(confusion_matrix) / colSums(confusion_matrix))))
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
precision <-  diag(confusion_matrix) / rowSums(confusion_matrix)
(f_score <- mean(ifelse(is.nan(2*precision*recall/(precision+recall)),0,2*precision*recall/(precision+recall))))



library(rattle)
library(rpart.plot)
library(RColorBrewer)#grow tree with rpart
tree <- rpart(
  Class ~ .,
  method = "class",
  data = trainset
  #cp = 0.005
)

fancyRpartPlot(tree)

tree2_preds <- predict(tree, testset, type="class")

confusionMatrix(tree2_preds,testset$Class)

tree2_preds <- factor(tree2_preds)
levels(tree2_preds) <- c("A","D","E","F","G","H","I","X","Y","B","C","W")
tree2_preds <- factor(tree2_preds, levels(tree2_preds)[c(1,10,11,2:7,12,8,9)])

(confusion_matrix <- table(tree2_preds,testset$Class))
(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
(precision <- mean(ifelse(is.nan(diag(confusion_matrix) / rowSums(confusion_matrix)), 0, diag(confusion_matrix) / rowSums(confusion_matrix))))
(recall <- mean(diag(confusion_matrix) / colSums(confusion_matrix)))
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
precision <-  diag(confusion_matrix) / rowSums(confusion_matrix)
(f_score <- mean(ifelse(is.nan(2*precision*recall/(precision+recall)),0,2*precision*recall/(precision+recall))))

