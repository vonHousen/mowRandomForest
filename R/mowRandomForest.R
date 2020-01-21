#'
#' @name mowRandomForest
#'
#' @title Custom Random Forest.
#' @description Function constructing custom Random Forest.
#' It grows mutliple Decision Trees storing them in a single object of 'Forest' class.
#'
#' @param df              dataframe consising of attributes & classes used for training
#' @param formula         name of the column of classes (values)
#' @param ntree           (= 1) count of trees in ensemble
#' @param complexity      (= -1) complexity of the trees used in ensemble (-1 = unlimited growing)
#' @param subsetRatio     (= 1) ratio of count of random subset of training data to count of df
#' @param zratio          (= 0) frequency of calling function decreasing importance
#'                        of random attributes
#'
#' @return multiple trees representing Random Forest encapsulated inside 'Forest' class.
#' @export

mowRandomForest <- function (
	df,
	formula,
	ntree = 1,
	complexity = -1,
	subsetRatio = 1,
	zratio = 0
)
{
	# check input parameters
	if(ntree < 1)
		stop("Cannot create a forest without trees.")
	if(subsetRatio > 1 || subsetRatio < 0)
		stop("Subset ratio must be in range [0, 1]")

	# create forest out of random trees
	forest <- list()
	forest <- lapply(    # applies function below `ntree` times, storing it's results in a list
		seq(1,ntree),
		function(i) growRandomTree(
			df = df,
			formula = formula,
			complexity = complexity,
			subsetRatio = subsetRatio,
			maxDepth = 5,
			zratio = zratio
			)
	)
	class(forest) <- "Forest"    # list becomes an object - Forest

	return(forest)
}
