
.html_append = function(elem,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    addChildren(elem,value)
}
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

setGeneric("rQuery.after",function(rquery,value){standardGeneric("rQuery.after")})
setMethod("rQuery.after","rQueryResult",function(rquery,value){
          lapply(rquery,.html_append,value=value)
})

setGeneric("rQuery.before",function(rquery,value){standardGeneric("rQuery.before")})
setMethod("rQuery.before","rQueryResult",function(rquery,value){
          lapply(rquery,.html_insert,value=value)
})

setGeneric("rQuery.insertBefore",function(rquery,value){standardGeneric("rQuery.insertBefore")})
setMethod("rQuery.insertBefore","rQueryResult",function(rquery,value){
    rQueryResult(lapply(rQuery(xmlRoot(elem),value),.html_insert,rquery))
})

setGeneric("rQuery.insertAfter",function(rquery,value){standardGeneric("rQuery.insertAfter")})
setMethod("rQuery.insertAfter","rQueryResult",function(rquery,value){
    rQueryResult(lapply(rQuery(xmlRoot(elem),value),.html_append,rquery))
})


