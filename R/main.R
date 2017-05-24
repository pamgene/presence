#' @import bnutil
#'
#' @export
operatorProperties = function() {
  return (list(
    list('Interactive', list('No', 'Yes')))
  )
}

#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({
    mainPanel(
      numericInput("cutoff", "Presence Cut-off",2, min = 0, max = 5),
      checkboxInput("multi", "Multi core processing", TRUE),
      actionButton("start", "Press to start ...")
    )
  })

  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getDataReactive = context$getData()
  go = reactiveVal(0)

  observe({
    getData=getDataReactive$value
    if (is.null(getData)) return()

    getPropertiesAsMap=getPropertiesAsMapReactive$value
    if (is.null(getPropertiesAsMap)) return()
    propertiesAsMap = getPropertiesAsMap()

    bndata = getData()
    df = bndata$data
    df$x = df[[bndata$xAxisColumnName]]

    observeEvent(go(), {
        showNotification(ui = "Running presence analysis ...", type = "message", closeButton = FALSE, duration = NULL)
        result = runPresence(df, input$multi)
        result = ddply(result, ~rowSeq, .fun = countPresence, cutOff = input$cutoff )
        aMetaData <- data.frame(labelDescription=colnames(result),
                                groupingType=c("rowSeq",
                                               "colSeq"
                                               ,"QuantitationType"
                                               ,"QuantitationType"
                                               ,"QuantitationType"
                                               ,"QuantitationType"
                                               ,"QuantitationType"
                                               ,"Spot") )
        aResult = AnnotatedData$new(data = result, metadata = aMetaData)
        showNotification(ui = "DONE!", type = "message", closeButton = FALSE, duration = NULL)
        context$setResult(aResult)
    }, ignoreInit = TRUE, once = TRUE)

    observeEvent(input$start, {
        if(is.null(input$start)) return()
        if ( (propertiesAsMap$Interactive == "No") | (input$start > 0) ){
          val = go()
          go(val + 1)
        }
    }, ignoreNULL = FALSE)
  })
}

