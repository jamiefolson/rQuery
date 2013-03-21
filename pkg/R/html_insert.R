.html_insert = function(elem,value,...) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    addChildren(elem,value,...)
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
          rQueryResult(lapply(rquery,.html_insert,value=value))
})

#' Insert content, specified by the parameter, to the beginning of each element in the 
#' set of matched elements.
#' 
#' @param rquery rQueryResult container 
#' @param value \code{XML} element, \code{rQueryResult} container, or character vector of html  
#' @returnType rQueryResult 
#' @return the modified \code{rquery} object
#' 
#' @author jfolson
#' @exportMethod rQuery.prepend
setGeneric("rQuery.prepend",function(rquery,value){standardGeneric("rQuery.prepend")})
setMethod("rQuery.prepend","rQueryResult",function(rquery,value){
          rQueryResult(lapply(rquery,.html_insert,value=value,at=0))
})

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
        xml_parents = lapply(rquery,xmlParent)
        xml_position = mapply(function(parent,child){
                which(xmlChildren(parent)==child)-1
            },xml_parents,rquery)
        rQueryResult(mapply(.html_insert,xml_parents,value=value,at=xml_position))
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
        xml_parents = lapply(rquery,xmlParent)
        xml_position = mapply(function(parent,child){
                which(xmlChildren(parent)==child)
            },xml_parents,rquery)
        rQueryResult(mapply(.html_insert,xml_parents,value=value,at=xml_position))
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
    rQuery.before(rQuery(xmlRoot(rquery[[1]]),value),rquery)
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
    rQuery.after(rQuery(xmlRoot(rquery[[1]]),value),rquery)
})


