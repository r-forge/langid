setGeneric("makeUTF", function(object, ...) standardGeneric("makeUTF"))
setMethod("makeUTF",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Encoding(Content(object)) <- "UTF-8"
              object
          })

modify_icorpus <- function(icorpus)
    tmMap(icorpus, FUN=tmReduce,
          tmFuns = list(makeUTF, tmTolower, removePunctuation,
                        removeNumbers, stripWhitespace))
