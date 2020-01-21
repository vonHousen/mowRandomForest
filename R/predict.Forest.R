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
#' @return vector with predicted classes.
#' @export

predict.Forest <- function (
	forest,
	newData
)
{
	# some initial check
	if (!inherits(forest, "Forest")) stop("Not a legitimate \"Forest\" object")
	if (is.null(newData)) stop("Missing argument")
  
  # utility for generating classes from regression tree prediction
  classify <- function(model, newdata){
    round(predict(model,newdata))
  }
  
	# call predict method on every tree in the forest & store their predictions in a list
  predictions <- lapply(forest, classify, newdata = testOver50k)
  
	# make it a data frame
	predictions <- data.frame(predictions)

	# compare all predictions and take the most frequent prediction for each case (voting)
	afterVoting <- apply(predictions, 1, function(x) tail(names(sort(table(x))), 1))

	return(afterVoting)
}


