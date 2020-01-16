#'
#' @title Wraps 3 functions necessary for rpart customisation.
#'
#' @name getFunctionsToInject
#'
#' @description Adapter for a list of 3 functions to be injected into rpart on constructing a tree.
#' Aforementioned functions are required by rpart in order to change it's way of creating splits.
#'
#' @return list of functions: eval, spli & init.
#' @export

getFunctionsToInject <- function ()
{
	return(
		list(
			eval = function(x) x,    # TODO replace with name of real eval function just like below:
			# eval = realFunction1,
			split = function(y) y,   # TODO
			init = function(z) z     # TODO
		)
	)
}

#' realFunction1 <- function()
#' {
#'     ...
#' }
