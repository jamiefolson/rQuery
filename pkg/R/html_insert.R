
.html_append = function(elem,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    addChildren(elem,value)
}

#' Insert content, specified by the parameter, to the end of each element in the 
#' set of matched elements.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element, \code{rQueryResult} container, or character vector of html  
#' @returnType rQueryResult 
#' @return the modified \code{rquery} object
#' 
#' @author jfolson
#' @exportMethod rQuery.append
setGeneric("rQuery.append",function(rquery,value){standardGeneric("rQuery.append")})
setMethod("rQuery.append","rQueryResult",function(rquery,value){
          lapply(rquery,.html_append,value=value)
})

.html_insert = function(elem,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    addChildren(elem,value,at=0)
}

#' Insert content, specified by the parameter, after each element in the set of 
#' matched elements.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element, \code{rQueryResult} container, or character vector of html  
#' @returnType rQueryResult 
#' @return the modified \code{rquery} object
#' 
#' @author jfolson
#' @exportMethod rQuery.after
setGeneric("rQuery.after",function(rquery,value){standardGeneric("rQuery.after")})
setMethod("rQuery.after","rQueryResult",function(rquery,value){
          lapply(rquery,.html_append,value=value)
})

#' Insert content, specified by the parameter, before each element in the set of 
#' matched elements.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element, \code{rQueryResult} container, or character vector of html  
#' @returnType rQueryResult 
#' @return the modified \code{rquery} object
#' 
#' @author jfolson
#' @exportMethod rQuery.before
setGeneric("rQuery.before",function(rquery,value){standardGeneric("rQuery.before")})
setMethod("rQuery.before","rQueryResult",function(rquery,value){
          lapply(rquery,.html_insert,value=value)
})

#' Insert every element in the set of matched elements before the target.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element, \code{rQueryResult} container, or character vector of html  
#' @returnType rQueryResult 
#' @return the modified \code{rquery} object
#' 
#' @author jfolson
#' @exportMethod rQuery.insertBefore
setGeneric("rQuery.insertBefore",function(rquery,value){standardGeneric("rQuery.insertBefore")})
setMethod("rQuery.insertBefore","rQueryResult",function(rquery,value){
    rQueryResult(lapply(rQuery(xmlRoot(rquery[[1]]),value),.html_insert,rquery))
})

#' Insert every element in the set of matched elements after the target.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element, \code{rQueryResult} container, or character vector of html  
#' @returnType rQueryResult 
#' @return the modified \code{rquery} object
#' 
#' @author jfolson
#' @exportMethod rQuery.insertAfter
setGeneric("rQuery.insertAfter",function(rquery,value){standardGeneric("rQuery.insertAfter")})
setMethod("rQuery.insertAfter","rQueryResult",function(rquery,value){
    rQueryResult(lapply(rQuery(xmlRoot(rquery[[1]]),value),.html_append,rquery))
})


