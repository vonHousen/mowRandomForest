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
system.time(
forest <- randomForest(
  y ~ .,
	data = trainset,
))

pred_forest <- predict(forest,newdata = testset)

# Get some feedback
levels(pred_forest) <- levels(factor(testset$y))

confusionMatrix(testset$y,pred_forest)

library(devtools)
install('.')
library(mowRandomForest)
library(rpart)

#grow a custom forest
mowForest <- mowRandomForest(
  df = trainset,
  formula = y ~.,
  ntree = 50,
  complexity = -1,
  subsetRatio = 1,
  zratio = 0.6
)

mow_forest_preeds <- predict(mowForest,newData = testset)

mow_forest_preeds <- (factor(mow_forest_preeds))
# Get some feedback
levels(mow_forest_preeds) <- levels(factor(testset$y))

confusionMatrix(testset$y,mow_forest_preeds)



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

confusionMatrix(factor(testset$y),factor(ifelse(tree2_preds[,1]==1,"no","yes")))
