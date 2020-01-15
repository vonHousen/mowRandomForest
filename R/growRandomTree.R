#'
#' @title Grows a Random Tree.
#'
#' @name growRandomTree
#'
#' @description Adapter for rpart function from rpart library.
#' Function grows a single tree from rpart out of randomised subset of training data.
#' The tree is ready to become a part of Random Forest.
#'
#' @param dataFrame       data frame storing data to train a tree
#' @param formula         formula pointing what data should be used as attributes and classes
#' @param complexity      (= -1) max. depth of the trees used in ensemble (-1 = unlimited growing)
#' @param nsubset         (= 100) count of elements in subsets used for growing each tree
#' @param zratio          (= 0) frequency of calling function decreasing importance
#'                        of random attributes
#'
#' @return single Random Tree (rpart class).
#' @export

library(rpart)

growRandomTree <- function (
	dataFrame,
	formula,
	complexity = -1,
	nsubset = 100,
	zratio = 0
)
{
	return(
		rpart(
			formula = formula,
			method = "class",
			data = dataFrame,
			cp = complexity
			# TODO should other parameters be changed here?
		)
	)
}
