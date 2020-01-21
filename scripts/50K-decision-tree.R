library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#load dataset
over50K = read.csv(
  file = 'data/50K-y/adult.data', 
  header = TRUE, 
  sep = ','
)

#show it
head(over50K)

#Grow a tree
tree <- rpart(
  salary ~ .,
  method = "class",
  data = over50K,
  control = rpart.control(
    minsplit = 2,
    cp = 0.001
  )
)

#showtime
plot(tree)
show(tree)
summary(tree)
printcp(tree)
plotcp(tree)
