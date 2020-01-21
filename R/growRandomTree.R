#'
#' @title Grows a Random Tree.
#'
#' @name growRandomTree
#'
#' @description Adapter for rpart function from rpart library.
#' Function grows a single tree from rpart out of randomised subset of training data.
#' The tree is ready to become a part of Random Forest.
#'
#' @param df              data frame storing data to train a tree
#' @param formula         formula pointing what data should be used as attributes and classes
#' @param complexity      (= -1) max. depth of the trees used in ensemble (-1 = unlimited growing)
#' @param subsetRatio     (= 1) ratio of count of random subset of training data to count of df
#' @param zratio          (= 0) frequency of calling function decreasing importance
#'                        of random attributes
#'
#' @return single Random Tree (rpart class).
#' @export

library(rpart)

growRandomTree <- function (
	df,
	formula,
	complexity = -1,
	maxDepth = 5,
	subsetRatio = 1,
	zratio = 0
)
{
	# Get count of examples in a subset of data
	count <- as.integer(nrow(df) * subsetRatio)

	# Get random subset of given data (sample with replacement)
	randomSubset <- df[sample(nrow(df), count, replace = T), ]
	return(
		rpart(
			formula = formula,
			#method = "class",     # TODO 
			method = getFunctionsToInject(),
			data = randomSubset,
			control = rpart.control(
			  cp = complexity,
			  maxdepth = maxDepth
			)
		)
	)
}
