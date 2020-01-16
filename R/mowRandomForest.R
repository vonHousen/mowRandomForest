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
#' @param nsubset         (= 100) count of elements in subsets used for growing each tree
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
	nsubset = 100,
	zratio = 0
)
{
	# check input parameters
	if(ntree < 1)
		stop("Cannot create a forest without trees.")

	# create forest out of random trees
	forest <- list()
	forest <- lapply(    # applies function below `ntree` times, storing it's results in a list
		seq(1,ntree),
		function(i) growRandomTree(
			data = df,
			formula = formula,
			complexity = complexity,
			nsubset = nsubset,
			zratio = zratio
			)
	)
	class(forest) <- "Forest"    # list becomes an object - Forest

	return(forest)
}
