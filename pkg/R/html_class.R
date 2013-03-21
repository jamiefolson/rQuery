
.html_class = function(elem,value){
    if (missing(value)){
        attrs = XML:::xmlAttrs(elem)
        class_idx = match("class",names(attrs))
        if (is.na(class_idx)){
            NULL
        } else {
            attrs[[class_idx]]
        }
    } else {
            attrs = XML:::xmlAttrs(elem)
            if ("class" %in% names(attrs)) {
                othervalue = attrs[["class"]]
                if (!value %in% strsplit(othervalue,",")[[1]]) {
                    value = paste(othervalue,value,sep=",")
                }
            }
            #  This returns the new node for non-internal `XML` nodes
            XML:::addAttributes(elem,class=value)
    }
}
#' Get the class value(s) for the first element in the set of matched 
#' elements or add a class for every matched element.
#' 
#' @param rquery rQueryResult container 
#' @param class a new class value
#' @returnType list or rQueryResult
#' @return list of class values or a rQueryResult with the modified elements
#' 
#' @author jfolson
#' @exportMethod rQuery.class
setGeneric("rQuery.class",function(rquery,value,...){standardGeneric("rQuery.class")})
setMethod("rQuery.class","rQueryResult",
          function(rquery,value){
              if (missing(value)){
                  .html_class(rquery[[1]])
              }else {
                  lapply(rquery,.html_class,value=value)
              }
          })

#' Adds the specified class(es) to each of the set of matched elements.
#' @param rquery rQueryResult container 
#' @param class a new class value
#' @returnType rQueryResult 
#' @return rQueryResult with the modified elements
#' 
#' @author jfolson
#' @exportMethod rQuery.addClass
setGeneric("rQuery.addClass",function(rquery,value){standardGeneric("rQuery.addClass")})
setMethod("rQuery.addClass","rQueryResult",function(rquery,value){
    if (missing(value)){
        stop("No new class provided")
    } else {
        lapply(rquery,.html_class,value=value)
        rquery
    }
})

#' Determine whether any of the matched elements are assigned the given class.
#' 
#' @param rquery rQueryResult container 
#' @param class a new class value
#' @returnType boolean
#' @return vector of booleans
#' 
#' @author jfolson
#' @exportMethod rQuery.hasClass
setGeneric("rQuery.hasClass",function(rquery,value){standardGeneric("rQuery.hasClass")})
setMethod("rQuery.hasClass","rQueryResult",function(rquery,value){
    if (missing(value)){
        stop("No class provided")
    } else {
        hasClass <- function(elem){
            othervalue = .html_class(elem)
            value %in% strsplit(othervalue,",")
        }
        lapply(rquery,hasClass)
    }
})
