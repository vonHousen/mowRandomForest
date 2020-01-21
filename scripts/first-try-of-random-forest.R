# My first try of training classic random forest
library(randomForest)

# Load data
wineWhite = read.csv("data/WINE/winequality-white.csv", header = TRUE, sep = ";")

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


# wineRed = read.csv("../data/WINE/winequality-red.csv", header = TRUE, sep = ";")

# Show it
head(wineWhite)

# Grow a forest
forest <- randomForest(
	salary ~ .,
	data = trainset
)

library(caret)

# Get some feedback
pred_forest <- predict(forest,newdata = testset)
confusionMatrix(factor(testset$salary),factor(pred_forest))
