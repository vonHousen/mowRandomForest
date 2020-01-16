# My first try of training classic decision tree
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load data
wineWhite = read.csv("data/WINE/winequality-white.csv", header = TRUE, sep = ";")
# wineRed = read.csv("../data/WINE/winequality-red.csv", header = TRUE, sep = ";")

# Show it
head(wineWhite)

# Grow a tree
tree <- rpart(
	wineWhite$quality
		~ wineWhite$fixed.acidity
		+ wineWhite$volatile.acidity
		+ wineWhite$citric.acid
		+ wineWhite$residual.sugar
		+ wineWhite$chlorides
		+ wineWhite$free.sulfur.dioxide
		+ wineWhite$total.sulfur.dioxide
		+ wineWhite$density
		+ wineWhite$pH
		+ wineWhite$sulphates
		+ wineWhite$alcohol,
	method = "class",
	data = wineWhite)

# Show it
fancyRpartPlot(tree, caption = NULL)

# Predict something
prediction <- predict(tree, newdata = wineWhite[1:11], type = "class")

