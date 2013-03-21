
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
setGeneric("rQuery.class",function(rquery,value,...){standardGeneric("rQuery.class")})
setMethod("rQuery.class","rQueryResult",
          function(rquery,value){
              if (missing(value)){
                  .html_class(rquery[[1]])
              }else {
                  lapply(rquery,.html_class,value=value)
              }
          })

setGeneric("rQuery.addClass",function(rquery,value){standardGeneric("rQuery.addClass")})
setMethod("rQuery.addClass","rQueryResult",function(rquery,value){
    if (missing(value)){
        stop("No new class provided")
    } else {
        lapply(rquery,.html_class,value=value)
        rquery
    }
})

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


