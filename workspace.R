library(bnutil)
library(plyr)
library(pgMulticore)
library(presence)

getdata = function() {
  return(presence::kineticdata)
}

setResult = function(annotatedResult){
  print(annotatedResult)
  result = annotatedResult$data
}

getProperties = function(){
  props = list(Interactive = "No")
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult
bnMessageHandler$getPropertiesAsMapHandler = getProperties


bnshiny::startBNTestShiny('presence', sessionType='run', bnMessageHandler=bnMessageHandler)
