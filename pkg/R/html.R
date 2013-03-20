

.env = new.env()

setClass("rQueryNode", representation(node = "XMLAbstractNode"))

rQueryNode <- function(elem){
    if (inherits(elem,"XMLAbstractNode")){
        new("rQueryNode",node=elem)
    }else {
        elem
    }
}

setClass("rQueryNodeSet",contains="list")

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
          f = get(name,envir=.env)
          elem = x
          function(...){
              f(elem,...)
          }
})

setMethod("$","rQueryNodeSet",function(x,name){
          f = get(name,envir=.env)
          obj = x
          function(...){
              rQueryNodeSet(lapply(obj@.Data,f,...))
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
assign("attr",.html_attr,envir=.env)


.class = function(elem,klass){
    attrs = XML:::xmlAttrs(elem@node)
    if (missing(klass)){
        class_idx = match("class",attrs)
        if (is.na(class_idx)){
            NULL
        } else {
            attrs[[class_idx]]
        }
    } else {
        if ("class" %in% attrs) {
            otherklass = attrs$class
            if (!klass %in% strsplit(otherklass,",")) {
                klass = paste(otherklass,klass,sep=",")
            }
        }
        elem@node = XML:::addAttributes(elem@node,class=klass)
        elem
    }
}
assign("class",.class,envir=.env)

.html = function(elem,value) {
    if (missing(value)){
        saveXML(elem@node)
    } else {
        if (is.character(value)) {
            value <- querySelectorAll(XML:::htmlParse(value,asText=T),"body >*")
        }
        removeChildren(elem@node,kids=xmlChildren(elem@node))
        xmlChildren(elem@node) <- value
    }
}
assign("html",.html,envir=.env)

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
assign("select",rQuery,envir=.env)


