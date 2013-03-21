
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
setGeneric("rQuery.html",function(rquery,value){standardGeneric("rQuery.html")})
setMethod("rQuery.html","rQueryResult",function(rquery,value){
          if (missing(value)){
              .html(rquery[[1]])
          }else {
              lapply(rquery,.html,value=value)
              rquery
          }
})

