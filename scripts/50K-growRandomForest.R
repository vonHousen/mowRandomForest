library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(devtools)
install('.')
library(mowRandomForest)
library(rpart)
library(caret)
library(randomForest)



#load dataset
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

#show it
head(over50K)

#grow a custom forest
forest <- mowRandomForest(
  df = over50K,
  formula = salary ~.,
  ntree = 20,
  complexity = 0.01,
  subsetRatio = 0.3,
  zratio = 0.3
)
print(forest)

forest_preeds <- predict(forest,testOver50k)
confusionMatrix(data = factor(forest_preeds), reference = factor(testOver50k$salary))

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
                
#grow tree with forest
tree <- randomForest(
  formula = salary ~.,
data = over50K,
importance = TRUE
)
print(tree)
summary(tree)
fancyRpartPlot(tree)

tree2_preds <- predict(tree, testOver50k)
confusionMatrix(data = factor(ifelse(tree2_preds[,1]>= 0.5,1,2)), reference = factor(testOver50k$salary))
