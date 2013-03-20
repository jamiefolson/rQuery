

.node.fn = new.env()

.nodeset.fn = new.env()

setClass("rQueryNode", representation(node = "XMLAbstractNode"))

rQueryNode <- function(elem){
    if (inherits(elem,"XMLAbstractNode")){
        new("rQueryNode",node=elem)
    }else {
        elem
    }
}

setClass("rQueryNodeSet",contains="list")

setGeneric("rQuery.select",function(elem,query){standardGeneric("rQuery.select")})
setGeneric("rQuery.html",function(elem,value){standardGeneric("rQuery.html")})
setGeneric("rQuery.attr",function(elem,value,...){standardGeneric("rQuery.attr")})
setGeneric("rQuery.class",function(elem,value,...){standardGeneric("rQuery.class")})
setGeneric("rQuery.hasClass",function(elem,value){standardGeneric("rQuery.hasClass")})
setGeneric("rQuery.addClass",function(elem,value){standardGeneric("rQuery.addClass")})
setGeneric("rQuery.add",function(elem,value){standardGeneric("rQuery.add")})
setGeneric("rQuery.append",function(elem,value){standardGeneric("rQuery.append")})
setGeneric("rQuery.insertBefore",function(elem,value){standardGeneric("rQuery.insertBefore")})
setGeneric("rQuery.insertAfter",function(elem,value){standardGeneric("rQuery.insertAfter")})
setGeneric("rQuery.before",function(elem,value){standardGeneric("rQuery.before")})
setGeneric("rQuery.after",function(elem,value){standardGeneric("rQuery.after")})

rQueryNodeSet <- function(nodeset){
    nodeset = unlist(nodeset)
    if (length(nodeset)>0){
        if (inherits(nodeset[[1]],"XMLAbstractNode") || inherits(nodeset,"XMLNodeSet")){
            new("rQueryNodeSet",lapply(nodeset,rQueryNode))
        }else if (inherits(nodeset[[1]],"rQueryNode")) {
            new("rQueryNodeSet",nodeset)
        }else {
            nodeset
        }
    }else {
        nodeset
    }
}

setMethod("show","rQueryNode",function(object){
          show(object@node)
})

setMethod("show","rQueryNodeSet",function(object){
          show(object@.Data)
})

setMethod("$","rQueryNode",function(x,name){
              function(...){
                  do.call(paste0("rQuery.",name),list(x,...))
              }
})

setMethod("$","rQueryNodeSet",function(x,name){
    if (hasMethod(paste0("rQuery.",name),"rQueryNodeSet")){
        function(...){
            do.call(paste0("rQuery.",name),list(x,...))
        }
    } else {
        function(...){
            lapply(x@.Data,function(elem){do.call(paste0("rQuery.",name),list(elem,...))})
          }
      }
})


.html_attr = function(elem,...){
    newattrs = list(...)
    attrs = XML:::xmlAttrs(elem@node)
    if (length(newattrs)>0){
        attrs = newattrs
#        if (is.null(attrs)) {
#            attrs = newattrs
#        }else {
#            for (attr_idx in seq_along(newattrs)){
#                attr_name = names(newattrs)[[attr_idx]]
#                attr_value = newattrs[[attr_idx]]
#                if (attr_name %in% names(attrs)){
#                    attr_value = paste(attrs[[attr_name]],attr_value,sep=",")
#                }
#                attrs[[attr_name]] = attr_value
#            }
#        }
        elem@node = XML:::addAttributes(elem@node,.attrs=attrs)
        elem
    }else {
        attrs
    }
}
setMethod("rQuery.attr","rQueryNode",.html_attr)


.html_class = function(elem,value){
    attrs = XML:::xmlAttrs(elem@node)
    if (missing(value)){
        class_idx = match("class",names(attrs))
        if (is.na(class_idx)){
            NULL
        } else {
            attrs[[class_idx]]
        }
    } else {
        if ("class" %in% names(attrs)) {
            othervalue = attrs[["class"]]
            if (!value %in% strsplit(othervalue,",")[[1]]) {
                value = paste(othervalue,value,sep=",")
            }
        }
        elem@node = XML:::addAttributes(elem@node,class=value)
        elem
    }
}
setMethod("rQuery.class","rQueryNode",.html_class)

setMethod("rQuery.addClass","rQueryNode",function(elem,value){
    attrs = XML:::xmlAttrs(elem@node)
    if (missing(value)){
        stop("No new class provided")
    } else {
        .html_class(elem,value)
        elem
    }
})

setMethod("rQuery.hasClass","rQueryNode",function(elem,value){
    attrs = XML:::xmlAttrs(elem@node)
    if (missing(value)){
        stop("No class provided")
    } else {
        othervalue = .html_class(elem)
        value %in% strsplit(othervalue,",")
    }
})

.html = function(elem,value) {
    if (missing(value)){
        saveXML(elem@node)
    } else {
        if (is.character(value)) {
            value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
        }
        addChildren(elem@node,kids=value,append=FALSE)
    }
}
setMethod("rQuery.html","rQueryNode",.html)

.html_append = function(elem,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    addChildren(elem@node,value)
}
setMethod("rQuery.append","rQueryNode",.html_append)

.html_insert = function(elem,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    addChildren(elem@node,value,at=0)
}
setMethod("rQuery.before","rQueryNode",.html_insert)

setMethod("rQuery.insertBefore","rQueryNode",function(elem,value){
    rQueryNodeSet(lapply(rQuery(xmlRoot(elem),value),.htmlinsert,elem))
})

setMethod("rQuery.insertAfter","rQueryNode",function(elem,value){
    rQueryNodeSet(lapply(rQuery(xmlRoot(elem),value),.htmlappend,elem))
})

.rquery_add = function(elem,value) {
    if (is.character(value)) {
        value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
    }
    rQueryNodeSet(append(elem@nodeset,value))
}
setMethod("rQuery.add","rQueryNode",.rquery_add)
setMethod("rQuery.add","rQueryNodeSet",.rquery_add)

#' jQuery for R
#' 
#' @param elem html element to query
#' @param query CSS selector query
#' @return XMLAbstractNode wrapped in a rQueryNodeSet
#' @export
#' 
rQuery <- function(elem,query) {
    node = elem
    if (inherits(elem,"rQueryNode")){
        node = elem@node
    }
    if (missing(query)){
        rQueryNode(elem)
    }else {
        res = querySelectorAll(node,query)
        if (length(res) == 1) {
            rQueryNode(res[[1]])
        }else {
            rQueryNodeSet(res)
        }
    }
}
`%$%` <- rQuery
setMethod("rQuery.select","rQueryNode",rQuery)
setMethod("rQuery.select","rQueryNodeSet",function(elem,query){rQueryNodeSet(lapply(elem,rQuery,query))})


