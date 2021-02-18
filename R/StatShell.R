

setMethod("PnodeMargin",c("PnetShell","PnodeShell"),
    stop("Method PnodeMargin not implemented for PnetShell, PnodeShell.")
          function(net,node) NodeBeliefs(node))
setMethod("PnodeMargin",c("PnetShell","character"),
          function(net,node) {
            nd <- PnetFindNode(net,node)
            if (is.null(nd))
              stop("Can't find node named ",node)
            PnodeMargin(net,nd)
          })
setMethod("PnodeEAP",c("PnetShell","PnodeShell"),
    stop("Method PnodeEAP not implemented for PnetShell, PnodeShell.")
          function(net,node) c(NodeExpectedValue(node))) #clear SD attribute c()
setMethod("PnodeEAP",c("PnetShell","character"),
          function(net,node) {
            nd <- PnetFindNode(net,node)
            if (is.null(nd))
              stop("Can't find node named ",node)
            PnodeEAP(net,nd)
          })
setMethod("PnodeSD",c("PnetShell","PnodeShell"),
    stop("Method PnodeSD not implemented for PnetShell, PnodeShell.")
          function(net,node) {
            ev <- NodeExpectedValue(node)
            if (is.na(ev)) return(NA)
            attr(ev,"std_dev")
            })
setMethod("PnodeSD",c("PnetShell","character"),
          function(net,node) {
            nd <- PnetFindNode(net,node)
            if (is.null(nd))
              stop("Can't find node named ",node)
            PnodeSD(net,nd)
          })
setMethod("PnodeMedian",c("PnetShell","PnodeShell"),
    stop("Method PnodeMedian not implemented for PnetShell, PnodeShell.")
          function(net,node)
            names(which(cumsum(NodeBeliefs(node))>=.5))[1])
setMethod("PnodeMedian",c("PnetShell","character"),
          function(net,node) {
            nd <- PnetFindNode(net,node)
            if (is.null(nd))
              stop("Can't find node named ",node)
            PnodeMedian(net,nd)
          })
setMethod("PnodeMode",c("PnetShell","PnodeShell"),
    stop("Method PnodeMode not implemented for PnetShell, PnodeShell.")
          function(net,node)
            names(which.max(NodeBeliefs(node))))
setMethod("PnodeMode",c("PnetShell","character"),
          function(net,node) {
            nd <- PnetFindNode(net,node)
            if (is.null(nd))
              stop("Can't find node named ",node)
            PnodeMode(net,nd)
          })
