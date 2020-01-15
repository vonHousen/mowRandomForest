#' Helpful function in mowRandomForest.R
#'
#' Gets formula required by rpart function, based on dataFrame & classes' column's name.
#'
#' @param dataFrame          data frame to be used in function mowRandomForest
#' @param dataFrameName      name of the data frame (variable name)
#' @param classesColumnName  name of the column (attribute) in data frame treated as resulting class
#'
#' @export

getFormula <- function (
	dataFrame,
	dataFrameName,
	classesColumnName
)
{
	attributesColumnName <- vector()
	classesName <- NA

	# iterate through all column names in dataFrame and put it into a vector
	# apart from classesColumnName
	for (column in colnames(dataFrame))
	{
		if (column == classesColumnName)
		{
			classesName <- classesColumnName
			next
		}
		attributesColumnName <- c(attributesColumnName, column)
	}
	# return if given classesColumnName does not match dataFrame
	if(is.na(classesName))
		return(NA)
	if (classesName != classesColumnName)
		return(NA)

	dfName <- dataFrameName

	# generate formula
	userFormula  <- paste(dfName, "$", classesName, " ~ ", sep = "")
	for(col in attributesColumnName)
	{
		userFormula <- paste(userFormula, dfName, "$", col, " + ", sep = "")
	}
	userFormula <- substr(userFormula, 1, nchar(userFormula) - 3)

	return(userFormula)
}

