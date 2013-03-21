.html_attr = function(elem,value){
    if (!missing(value)){
        elem = XML:::addAttributes(elem,.attrs=value)
        elem
    }else {
        attrs = XML:::xmlAttrs(elem)
        attrs
    }
}
setGeneric("rQuery.attr",function(rquery,value,...){standardGeneric("rQuery.attr")})
setMethod("rQuery.attr","rQueryResult",function(rquery,...){
    newattrs = list(...)
    if (length(newattrs)>0){
        # Return the "new" elements, which for internal nodes are the same as the original
        lapply(rquery,.html_attr,value=newattrs)
    }else {
        .html_attr(rquery[[1]])
    }
})


