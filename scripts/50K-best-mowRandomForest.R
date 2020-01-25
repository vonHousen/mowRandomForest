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

library(caret)
library(devtools)
install('.')
library(mowRandomForest)
library(rpart)
library(parallel)


#complexity test
print(" ======== COMPLEXITY TEST ========")
print("ldrzew subset z")
print("20 0.6 0.3")
print("complexity 0.1 0.05 0.01 0.005 0.001 0.0005")
complexity <- c(0.05, 0.01, 0.005, 0.001, 0.0005)
for (comp_i in complexity) {
  print(system.time(
    {
      print("complexity")
      print(comp_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = 20,
        complexity = 0.01,
        subsetRatio = 0.6,
        zratio = 0.3
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}

#subset test
print(" ======== SUBSET RATIO TEST ========")
print("ldrzew z comlexity")
print("20 0.3 0.01")
print("subset 0.1 0.2 0.4 0.6 0.8")
subset <- c(0.1, 0.2, 0.4, 0.6, 0.8)
for (sub_i in subset) {
  print(system.time(
    {
      print("subset")
      print(sub_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = 20,
        complexity = 0.01,
        subsetRatio = sub_i,
        zratio = 0.3
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}

#l drzew test
print(" ======== l DRZEW TEST ========")
print("z subset comlexity")
print("0.3 0.6 0.01")
print("l drzew 10 20 40 50 100 200")
ntree <- c(10, 20, 40, 50, 100, 200)
for (n_i in ntree) {
  print(system.time(
    {
      print("tree numbers")
      print(n_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = n_i,
        complexity = 0.01,
        subsetRatio = 0.6,
        zratio = 0.3
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}



#z test
print(" ======== Z RATIO TEST ========")
print("ldrzew subset comlexity")
print("20 0.6 0.01")
print("z ratio 0.6, 0.3, 0.1")
z <- c(0.6, 0.3, 0.1)
for (z_i in z) {
  print(system.time(
    {
      print("z ratio")
      print(z_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = 20,
        complexity = 0.01,
        subsetRatio = 0.6,
        zratio = z_i
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}