#'
#' @title predicts classes by the Random Forest.
#'
#' @name predict.Forest
#'
#' @description Method of Forest class. It predicts classes by the Random Forest (Forest class).
#' In fact, it calls rpart.predict() methods on every tree that aforementioned Forest consists of.
#'
#' @param forest          object of Forest class representing Random Forest
#' @param newData         data frame of data on which prediction is to be made
#' TODO any other arguments here?
#'
#' @return provided data frame with one more column - predicted class.
#' @export

predict.Forest <- function (
	forest,
	newData
)
{
	if (!inherits(forest, "Forest")) stop("Not a legitimate \"Forest\" object")

	cat("Given forest consists of...")
	cat(length(forest))
	cat(" trees. Now I will be ")
	cat("working on the data...") # TODO replace with real code
}
