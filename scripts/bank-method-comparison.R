# My first try of training classic random forest
library(randomForest)

# Load data
bank = read.csv("data/bank/bank-full.csv", header = TRUE, sep = ";")


dataset <- bank
dataset$y <- as.factor(dataset$y)


dataset <- dataset[sample(nrow(dataset)),]
train_indices <- 1:round(0.7 * nrow(dataset))
trainset <- dataset[train_indices,]
test_indices <- round(0.7 * nrow(dataset)):nrow(dataset)
testset <- dataset[test_indices,]

# Grow a forest
forest <- randomForest(
  y ~ .,
	data = trainset
)

pred_forest <- predict(forest,newdata = testset)

# Get some feedback
levels(pred_forest) <- levels(factor(testset$y))

confusionMatrix(pred_forest,testset$y)

library(devtools)
install('.')
library(mowRandomForest)
library(rpart)

#grow a custom forest
mowForest <- mowRandomForest(
  df = trainset,
  formula = y ~.,
  ntree = 10,
  complexity = 0.01,
  subsetRatio = 0.6,
  zratio = 0.3
)

mow_forest_preeds <- predict(mowForest,newData = testset)

mow_forest_preeds <- (factor(mow_forest_preeds))
# Get some feedback
levels(mow_forest_preeds) <- levels(factor(testset$y))

confusionMatrix(mow_forest_preeds,testset$y)



library(rattle)
library(rpart.plot)
library(RColorBrewer)#grow tree with rpart
tree <- rpart(
  y ~ .,
  method = "class",
  data = trainset
)
print(tree)
summary(tree)
fancyRpartPlot(tree)

tree2_preds <- predict(tree, testset)
tree2_preds <- round(tree2_preds)

confusionMatrix(data = factor(round(tree2_preds)), reference = factor(testset$y))
