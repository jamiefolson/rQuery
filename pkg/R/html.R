
.html = function(elem,value) {
    if (missing(value)){
        saveXML(elem)
    } else {
        if (is.character(value)) {
            value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
        }
        addChildren(elem,kids=value,append=FALSE)
    }
}

#' Get the HTML contents of the first element in the set of matched elements or set the HTML contents of every matched element.
#'
#' @param rquery an rQueryResult object
#' @param value optional \code{XML} object or html character vector.
#' 
#' @return rQueryResult containing the modified \code{rquery} element or a
#' character vector containing the html contents of the first element.
#' 
#' 
#' @exportMethod rQuery.html
setGeneric("rQuery.html",function(rquery,value){standardGeneric("rQuery.html")})
setMethod("rQuery.html","rQueryResult",function(rquery,value){
          if (missing(value)){
              .html(rquery[[1]])
          }else {
              lapply(rquery,.html,value=value)
              rquery
          }
})

