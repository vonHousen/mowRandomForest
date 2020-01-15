#'
#' @title Custom Random Forest.
#'
#' @name mowRandomForest
#'
#' @description Function constructing custom Random Forest.
#' It grows mutliple Decision Trees storing them in a single object of 'Forest' class.
#'
#' @param df              dataframe consising of attributes & classes used for training
#' @param classesName     name of the column of classes (values)
#' @param ntree           (= 1) count of trees in ensemble
#' @param maxdepth        (= -1) max. depth of the trees used in ensemble (-1 = unlimited growing)
#' @param nsubset         (= 100) count of elements in subsets used for growing each tree
#' @param zratio          (= 0) frequency of calling function decreasing importance
#'                        of random attributes
#'
#' @return multiple trees representing Random Forest encapsulated inside 'Forest' class.
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
	if(is.na(userParamFormula))
		return(NA)

	forest <- list()
	forest <- lapply(    # applies function below `ntree` times, storing it's results in a list
		seq(1,ntree),

		# grow a tree
		function(i)
		rpart(
			formula = userParamFormula,
			method = "class",
			data = df,
			cp = userParamComplexity
			# TODO should other parameters be changed here?
		)
	)
	class(forest) <- "Forest"    # list becomes now an object - Forest


	return(forest)
}
