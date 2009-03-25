setGeneric("makeUTF", function(object, ...) standardGeneric("makeUTF"))
setMethod("makeUTF",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Encoding(Content(object)) <- "UTF-8"
              return(object)
          })

tmReduce <- function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

modify_icorpus <- function(icorpus)
    tmMap(icorpus, FUN=tmReduce,
          tmFuns = list(makeUTF, tmTolower, removePunctuation,
                        removeNumbers, stripWhitespace))