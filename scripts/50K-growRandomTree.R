library(rattle)
library(rpart.plot)
library(RColorBrewer)
install('.')
library(mowRandomForest)
library(rpart)
library(caret)


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

#Grow a tree
tree1 <- growRandomTree(
  df = over50K,
  formula = salary ~ .,
  subsetRatio = 0.1,
  maxDepth = 2
)
print(tree1)
summary(tree1)
fancyRpartPlot(tree1)

tree1_preds <- predict(tree1, testOver50k)
confusionMatrix(data = factor(ifelse(tree1_preds >= 1.5, 2, 1)), reference = factor(testOver50k$salary))


#grow tree with anova
tree2 <- rpart(
  salary ~ .,
  method = "anova",
  data = randomSubset,
  control = rpart.control(
    maxdepth = 2
  )
)
print(tree2)
summary(tree2)
fancyRpartPlot(tree2)

tree2_preds <- predict(tree2, testOver50k)
confusionMatrix(data = factor(ifelse(tree2_preds >= 1.5, 2, 1)), reference = factor(testOver50k$salary))

                