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

reference_parameters <- list(
  n_tree = 50,
  complexity = -1,
  zratio = 0.6,
  subset = 1
)

parameters <- list(
  complexity = c(0.05, 0.005, 0.0005, -1),
  subset = c(0.1, 0.2, 0.4, 0.6, 0.8, 1.0),
  n_tree = c(10, 20, 40, 50, 100, 200),
  z =  c(1, 0.8, 0.6, 0.4, 0.3)
)

#complexity test
print(" ======== COMPLEXITY TEST ========")
print("complexity")
print(parameters$complexity)
for (comp_i in parameters$complexity) {
  print(system.time(
    {
      print("complexity")
      print(comp_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = reference_parameters$n_tree,
        complexity = comp_i,
        subsetRatio = reference_parameters$subset,
        zratio = reference_parameters$zratio
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}

#subset test
print(" ======== SUBSET RATIO TEST ========")
print("subset")
print(parameters$subset)
for (sub_i in parameters$subset) {
  print(system.time(
    {
      print("subset")
      print(sub_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = reference_parameters$n_tree,
        complexity = reference_parameters$complexity,
        subsetRatio = sub_i,
        zratio = reference_parameters$zratio
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}

#l drzew test
print(" ======== l DRZEW TEST ========")
print("l drzew")
print(parameters$n_tree)
for (n_i in parameters$n_tree) {
  print(system.time(
    {
      print("tree numbers")
      print(n_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = n_i,
        complexity = reference_parameters$complexity,
        subsetRatio = reference_parameters$subset,
        zratio = reference_parameters$zratio
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}



#z test
print(" ======== Z RATIO TEST ========")
print("z ratio")
print(parameters$z)
for (z_i in parameters$z) {
  print(system.time(
    {
      print("z ratio")
      print(z_i)
      mowForest <- mowRandomForest(
        df = trainset,
        formula = salary ~.,
        ntree = reference_parameters$n_tree,
        complexity = reference_parameters$complexity,
        subsetRatio = reference_parameters$subset,
        zratio = z_i
      )
      mow_forest_preeds <- predict(mowForest,testset)
      print(confusionMatrix(factor(testset$salary),factor(mow_forest_preeds)))
    }
  ))
}