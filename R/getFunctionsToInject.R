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
			eval = customEval,    
			split = customSplit,  
			init = customInit
		)
	)
}

#custom eval function
customEval <- function(y, wt, parms)
{
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label= wmean, deviance=rss)
}

#custom split function
customSplit <- function(y, wt, x, parms, continuous)
{
  y <- y - sum(y*wt)/sum(wt)
  
  if (continuous) {
    # continuous x variable
    n <- length(y)
  
    left.wt  <- cumsum(wt)[-n]
    temp <- cumsum(y*wt)[-n]
    lmean <- temp/left.wt
    direction=sign(lmean)
    
    #goodness <- runif(n-1, min = 0, max = 0.5)
    goodness <- c(rep(runif(1, min = 0, max = 1), n-1))
    
    list(goodness = goodness, direction=direction)
  }
  else {
    ux <- unique(x)
    n <- length(ux)
    
    goodness <- c(rep(runif(1, min = 0, max = 1), n-1))
    direction = sample(ux)
    
    list(goodness= goodness,
         direction =direction)
  }
  
  #goodness <- runif(n-1, min = 0, max = 1)
  #direction <- c(rep(1,n-1))
  #list(goodness = goodness, direction=direction)
}

#custom init function
customInit <- function(y, offset, parms, wt)
{
  if (!is.null(offset)) y <- y-offset
  list(y=y, parms=0, numresp=1, numy=1,
       summary= function(yval, dev, wt, ylevel, digits ) {
         paste("  mean=", format(signif(yval, digits)),
               ", MSE=" , format(signif(dev/wt, digits)),
               sep='')
       })
}