
#' rQuery S4 class for containing selections is a simple wrapper around list
#' 
#' This class exists primarily to facilitate the chaining syntax of \code{jQuery}.
#' 
#' @author jfolson
#' @exportClass rQueryResult
setClass("rQueryResult",contains="list")
rQueryResult<- function(nodeset){
    nodeset = unlist(nodeset)
    if (is.null(nodeset)){
        new("rQueryResult",list())
    }else {
        new("rQueryResult",nodeset)
    }
}

setMethod("show","rQueryResult",function(object){
          show(object@node)
})

setMethod("show","rQueryResult",function(object){
          show(object@.Data)
})

#' Syntax for allowing \code{jQuery}'s chaining syntax.
#' 
#' Any S4 methods for \code{rQueryResult} are available throught the \code{`$`} 
#' operator.
#' 
#' @param x 
#' @param name 
#' @returnType function
#' @return function to call the specified S4 method
#' 
#' @author jfolson
#' @exportMethod $
setMethod("$","rQueryResult",function(x,name){
    if (hasMethod(paste0("rQuery.",name),"rQueryResult")){
        function(...){
            do.call(paste0("rQuery.",name),list(x,...))
        }
    } else {
        stop("No method: `"+name+"` available for rQueryResult")
      }
})

.rquery_add = function(rquery,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    rQueryResult(append(rquery,value))
}

#' Add one or more element to an rQueryResult container.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element or rQueryResult to add to \code{rquery}
#' @returnType 
#' @return a new \code{rQueryResult} containing the combined elements.
#' 
#' @author jfolson
#' @exportMethod rQuery.add
setGeneric("rQuery.add",function(rquery,value){standardGeneric("rQuery.add")})
setMethod("rQuery.add","rQueryResult",.rquery_add)

#' jQuery for R
#' 
#' @param elem html element to query
#' @param query CSS selector query
#' @return XMLAbstractNode wrapped in a rQueryResult
#' @returnType rQueryResult
#' 
#' @author jfolson
#' @export
rQuery <- function(elem,query) {
    node = elem
    if (inherits(elem,"rQueryResult")){
        rQueryResult(lapply(elem,rQuery,query=query))
    }else {
        if (missing(query)){
            rQueryResult(elem)
        }else {
            res = querySelectorAll(node,query)
            rQueryResult(res)
        }
    }
}

#' 
#' @rdname rQuery
#' @author jfolson
#' @export
`%$%` <- function(elem,query){rQuery(elem,query)}

#' Get the descendants of each element in the current set of matched elements, 
#' filtered by a selector, jQuery object, or element.
#' 
#' @param rquery rQueryResult container to search
#' @param query CSS selector query
#' @return XMLAbstractNode wrapped in a rQueryResult
#' @returnType rQueryResult
#' 
#' @author jfolson
#' @exportMethod rQuery.find
setGeneric("rQuery.find",function(rquery,query){standardGeneric("rQuery.find")})
setMethod("rQuery.find",signature(rquery="rQueryResult"),function(rquery,query){rQueryResult(lapply(rquery,rQuery,query))})
