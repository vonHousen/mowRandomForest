#'
#' @title Custom Random Forest.
#'
#' @name mowRandomForest
#'
#' @description Function - adapter, used as a Proof of Concept.
#' It plays the role of adapter between user and rpart function.
#'
#' @param df              dataframe consising of attributes & classes used for training
#' @param classesName     name of the column of classes (values)
#' @param ntree           (= 1) count of trees in ensemble
#' @param maxdepth        (= -1) max. depth of the trees used in ensemble (-1 = unlimited growing)
#' @param nsubset         (= 100) count of elements in subsets used for growing each tree
#' @param zratio          (= 0) frequency of calling function decreasing importance
#'                        of random attributes
#'
#' @export

library(rpart)

mowRandomForest <- function (
	df,
	classesName,
	ntree = 1,
	maxdepth = -1,
	nsubset = 100,
	zratio = 0
)
{
	# prepare rpart parameters
	userParamFormula <- getFormula(df, deparse(substitute(df)), classesName)
	userParamComplexity <- maxdepth

	if(ntree < 1)
		return(NA)

	trees <- list(length = ntree)
	for(i in seq(1, ntree))
	{
		# grow a tree
		singleTree <- rpart(
			formula = userParamFormula,
			method = "class",
			data = df,
			cp = userParamComplexity
			# TODO should other parameters be changed here?
		)
		trees[[i]] <- singleTree
	}

	return(trees)
}
