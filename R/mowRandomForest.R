#'
#' @title Custom Random Forest.
#'
#' @description Function - adapter, used as a Proof of Concept.
#' It plays the role of adapter between user and rpart function.
#'
#' @param df              - dataframe consising of attributes & classes used for training
#' @param classesName     - name of the column of classes (values)
#' @param ntree = 1       - count of trees in ensemble
#' @param maxdepth = 0    - max. depth of the trees used in ensemble (0 = unlimited growing)
#' @param nsubset = 100   - count of elements in subsets used for growing each tree
#' @param zratio = 0      - frequency of calling function decreasing importance of random attributes
#'
#' @export

library(rpart)

mowRandomForest <- function (
	df,
	classesName,
	ntree = 1,
	maxdepth = 0,
	nsubset = 100,
	zratio = 0
)
{
	userFormula <- getFormula(df, deparse(substitute(df)), classesName)

	return(rpart(formula = userFormula, method = "class", data = df))
}
