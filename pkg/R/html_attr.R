.html_attr = function(elem,value){
    if (!missing(value)){
        elem = XML:::addAttributes(elem,.attrs=value)
        elem
    }else {
        attrs = XML:::xmlAttrs(elem)
        attrs
    }
}

#' Get the value of an attribute for the first element in the set of matched 
#' elements or set one or more attributes for every matched element.
#' 
#' @param rquery rQueryResult container 
#' @param ... a namded list of attribute values
#' @returnType list or rQueryResult
#' @return list of attribute values or a rQueryResult with the modified elements
#' 
#' @author jfolson
#' @exportMethod rQuery.attr
setGeneric("rQuery.attr",function(rquery,...){standardGeneric("rQuery.attr")})
setMethod("rQuery.attr","rQueryResult",function(rquery,...){
    newattrs = list(...)
    if (length(newattrs)>0){
        # Return the "new" elements, which for internal nodes are the same as the original
        lapply(rquery,.html_attr,value=newattrs)
    }else {
        .html_attr(rquery[[1]])
    }
})
