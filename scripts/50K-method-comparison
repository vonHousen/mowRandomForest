# My first try of training classic random forest
library(randomForest)

# Load data
over50K = read.csv(
  file = 'data/50K-y/adult.data', 
  header = TRUE, 
  sep = ','
)

testOver50k = read.csv(
  file = 'data/50K-y/adult.test', 
  header = TRUE, 
  sep = ','
)

dataset <- rbind(testOver50k,over50K)
dataset$salary <- as.factor(dataset$salary)


dataset <- dataset[sample(nrow(dataset)),]
train_indices <- 1:round(0.7 * nrow(dataset))
trainset <- dataset[train_indices,]
test_indices <- round(0.7 * nrow(dataset)):nrow(dataset)
testset <- dataset[test_indices,]

# Grow a forest
forest <- randomForest(
	salary ~ .,
	data = trainset
)

pred_forest <- predict(forest,newdata = testset)

library(caret)

# Get some feedback
confusionMatrix(factor(testset$salary),factor(pred_forest))



library(devtools)
install('.')
library(mowRandomForest)
library(rpart)

#grow a custom forest
mowForest <- mowRandomForest(
  df = trainset,
  formula = salary ~.,
  ntree = 40,
  complexity = 0.0001,
  subsetRatio = 0.6,
  zratio = 0.3
)

mow_forest_preeds <- predict(mowForest,testset)
confusionMatrix(factor(testset$salary),factor(mow_forest_preeds))


#grow tree with rpart
tree <- rpart(
  salary ~ .,
  method = "class",
  data = over50K
)
print(tree)
summary(tree)
fancyRpartPlot(tree)

tree2_preds <- predict(tree, testOver50k)
confusionMatrix(data = factor(ifelse(tree2_preds[,1]>= 0.5,1,2)), reference = factor(testOver50k$salary))
