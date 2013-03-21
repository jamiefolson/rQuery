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

setMethod("$","rQueryResult",function(x,name){
              function(...){
                  do.call(paste0("rQuery.",name),list(x,...))
              }
})

setMethod("$","rQueryResult",function(x,name){
    if (hasMethod(paste0("rQuery.",name),"rQueryResult")){
        function(...){
            do.call(paste0("rQuery.",name),list(x,...))
        }
    } else {
        function(...){
            lapply(x@.Data,function(elem){do.call(paste0("rQuery.",name),list(elem,...))})
          }
      }
})

.rquery_add = function(rquery,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    rQueryResult(append(rquery,value))
}
setGeneric("rQuery.add",function(rquery,value){standardGeneric("rQuery.add")})
setMethod("rQuery.add","rQueryResult",.rquery_add)

#' jQuery for R
#' 
#' @param elem html element to query
#' @param query CSS selector query
#' @return XMLAbstractNode wrapped in a rQueryResult
#' @export
#' 
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
`%$%` <- rQuery
setGeneric("rQuery.find",function(rquery,query){standardGeneric("rQuery.find")})
setMethod("rQuery.find",signature(rquery="rQueryResult"),function(rquery,query){rQueryResult(lapply(rquery,rQuery,query))})


