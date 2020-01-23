# My first try of training classic random forest
library(randomForest)

# Load data
wineWhite = read.csv("data/WINE/winequality-white.csv", header = TRUE, sep = ";")
wineRed = read.csv("data/WINE/winequality-red.csv", header = TRUE, sep = ";")


dataset <- rbind(wineWhite,wineRed)
dataset$quality <- as.factor(dataset$quality)


dataset <- dataset[sample(nrow(dataset)),]
train_indices <- 1:round(0.7 * nrow(dataset))
trainset <- dataset[train_indices,]
test_indices <- round(0.7 * nrow(dataset)):nrow(dataset)
testset <- dataset[test_indices,]

# Grow a forest
forest <- randomForest(
  quality ~ .,
	data = trainset
)

pred_forest <- predict(forest,newdata = testset)

# Get some feedback
levels(pred_forest) <- levels(factor(testset$quality))

(confusion_matrix <- table(pred_forest,testset$quality))
(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
(precision <- diag(confusion_matrix) / rowSums(confusion_matrix))
(recall <- (diag(confusion_matrix) / colSums(confusion_matrix)))


library(devtools)
install('.')
library(mowRandomForest)
library(rpart)

#grow a custom forest
mowForest <- mowRandomForest(
  df = trainset,
  formula = quality ~.,
  ntree = 100,
  complexity = -1,
  subsetRatio = 0.6,
  zratio = 0.3
)

mow_forest_preeds <- predict(mowForest,newData = testset)

mow_forest_preeds <- (factor(mow_forest_preeds))
# Get some feedback
levels(mow_forest_preeds) <- levels(factor(testset$quality))

(confusion_matrix <- table(mow_forest_preeds,testset$quality))
(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
(precision <- diag(confusion_matrix) / rowSums(confusion_matrix))
(recall <- (diag(confusion_matrix) / colSums(confusion_matrix)))


library(rattle)
library(rpart.plot)
library(RColorBrewer)#grow tree with rpart
tree <- rpart(
  quality ~ .,
  method = "class",
  data = trainset
)
print(tree)
summary(tree)
fancyRpartPlot(tree)

tree2_preds <- predict(tree, testset)
tree2_preds <- round(tree2_preds)

confusionMatrix(data = factor(round(tree2_preds)), reference = factor(testset$quality))
