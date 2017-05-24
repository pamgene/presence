#' @import plyr pgMulticore
runPresence = function(df, multicore = TRUE){
  if (!multicore){
    aResult = ddply(df, ~rowSeq + colSeq, .fun = presence)
  }else {
    aResult = doMultiCore(~rowSeq + colSeq, data = df, operatorFunction = presence, .export = "presence")
  }
  return(aResult)
}

presence <- function(aFrame)
{
    aLm <- try(lm(value~x,data = aFrame),  silent = TRUE)
    if(!inherits(aLm, 'try-error')){
      slope = aLm$coefficients[[2]]
      intercept = aLm$coefficients[[1]]
	  r2 = summary(aLm)$r.squared
	  p = anova(aLm)[['Pr(>F)']][1]
	  presence = -log10(p) * sign(slope)
    } else {
      slope = NaN
      intercept = NaN
      r2 = NaN
	  p = NaN
	  present = NaN
    }
	return(c(rowSeq = aFrame$rowSeq[1], colSeq = aFrame$colSeq[1], slope=slope, intercept = intercept, r2 = r2, p= p, presence = presence  ))
}

countPresence <- function(aFrame, cutOff){
	if (any(is.na(aFrame$presence))){
		fractionPresent = NaN
	} else {
		fractionPresent = sum(aFrame$presence > cutOff)/dim(aFrame)[1]
	}
	aFrame = data.frame(aFrame, fractionPresent = fractionPresent)
	return(aFrame)
}





