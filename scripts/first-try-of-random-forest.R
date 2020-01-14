# My first try of training classic random forest
library(randomForest)

# Load data
wineWhite = read.csv("data/WINE/winequality-white.csv", header = TRUE, sep = ";")
# wineRed = read.csv("../data/WINE/winequality-red.csv", header = TRUE, sep = ";")

# Show it
head(wineWhite)

# Grow a forest
forest <- randomForest(
	quality ~ .,
	data = wineWhite
)

# Get some feedback
forest
