#'
#' @title Wraps 3 functions necessary for rpart customisation.
#'
#' @name getFunctionsToInject
#'
#' @description Adapter for a list of 3 functions to be injected into rpart on constructing a tree.
#' Aforementioned functions are required by rpart in order to change it's way of creating splits.
#' Function set of "Anova" method has been implemented here.
#' Split function has been modified to discount random argument in dataset.
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
{  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  
  # Will it count while splitting? 
  # random binary parameter
  isCounted <- rbinom(1,1,zratio)
  
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    
    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    
    goodness <- isCounted*(left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    
    list(goodness= goodness, direction=sign(lmean))
  }
  else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    # For anova splits, we can order the categories by their means
    #  then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness=isCounted*(left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord])
  }
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