
PnetShell <- setClass("PnetShell",slots=character(),
                      contains=c("Pnet","VIRTUAL"))
PnodeShell <- setClass("PnodeShell",slots=character(),
                      contains=c("Pnet","VIRTUAL"))
ShellSession <- setClass("ShellSession",slots=character(),
                         contains=c("VIRTUAL"))


### Netica specific implementations for the generics.

setMethod("PnodeName","PnodeShell", function (node) {
    stop("Method PnodeName not implemented for PnodeShell.")
    nme <- NodeUserField(node,"Truename")
    if (is.null(nme) || is.na(nme)) {
        nme <- NodeName(node)
    }
    nme
})

setMethod("PnodeName<-","PnodeShell", function (node,value) {
    stop("Method PnodeName<- not implemented for PnodeShell.")
  NodeUserField(node,"Truename") <- value
  NodeName(node) <- as.IDname(value)
  invisible(node)
})


setMethod("PnodeTitle","PnodeShell", function (node)
    stop("Method PnodeTitle not implemented for PnodeShell.")
  NodeTitle(node))

setMethod("PnodeTitle<-","PnodeShell", function (node,value) {
    stop("Method PnodeTitle<- not implemented for PnodeShell.")
  NodeTitle(node) <- value
  invisible(node)
})

setMethod("PnodeDescription","PnodeShell", function (node)
    stop("Method PnodeDescription not implemented for PnodeShell.")
  NodeDescription(node))

setMethod("PnodeDescription<-","PnodeShell", function (node,value) {
    stop("Method PnodeDescription<- not implemented for PnodeShell.")
  NodeDescription(node) <- value
  invisible(node)
})

setMethod("PnodeLabels","PnodeShell", function (node)
    stop("Method PnodeLabels not implemented for PnodeShell.")
  NodeSets(node))

setMethod("PnodeLabels<-","PnodeShell", function (node,value) {
    stop("Method PnodeLabels<- not implemented for PnodeShell.")
  NodeSets(node) <- value
  invisible(node)
})


### States

setMethod("PnodeStates","PnodeShell", function (node)
    stop("Method PnodeStates not implemented for PnodeShell.")
  NodeStates(node))

setMethod("PnodeStates<-","PnodeShell", function (node,value) {
    stop("Method PnodeStates<- not implemented for PnodeShell.")
  ## Not 100% sure if this is safe, but want to simplify
  ## Interface.
  NodeStates(node,resize=TRUE) <- value
  invisible(node)
})

setMethod("PnodeNumStates","PnodeShell", function (node)
  lenght(PnodeStates(node))

setMethod("PnodeStateTitles","PnodeShell", function (node) {
    stop("Method PnodeStateTitles not implemented for PnodeShell.")
  NodeStateTitles(node)
})

setMethod("PnodeStateTitles<-","PnodeShell", function (node,value) {
    stop("Method PnodeStateTitles<- not implemented for PnodeShell.")
  NodeStateTitles(node) <- value
  invisible(node)
})


setMethod("PnodeStateDescriptions","PnodeShell", function (node) {
    stop("Method PnodeStateDescriptions not implemented for PnodeShell.")
  NodeStateComments(node)
})

setMethod("PnodeStateDescriptions<-","PnodeShell", function (node,value) {
    stop("Method PnodeStateDescriptions<- not implemented for PnodeShell.")
  NodeStateComments(node) <- value
  invisible(node)
})

setMethod("PnodeStateValues","PnodeShell", function (node)
    stop("Method PnodeStateValues not implemented for PnodeShell.")
  if (is.continuous(node)) {
    apply(PnodeStateBounds(node),1,median)
  } else {
    NodeLevels(node)
  })

setMethod("PnodeStateValues<-","PnodeShell", function (node,value) {
    stop("Method PnodeStateValues<- not implemented for PnodeShell.")
  if (is.continuous(node))
    stop("This function only available for discrete nodes, but ",
         PnodeName(node), " is continuous. Use PnodeStateBounds instead.")
  NodeLevels(node) <- value
  invisible(node)
})

setMethod("PnodeStateBounds","PnodeShell", function (node)
    stop("Method PnodeStateBounds not implemented for PnodeShell.")
  if (is.continuous(node)) {
    vals <- NodeLevels(node)
    k <- length(vals) -1L
    if (k < 1L) {
      bnds <- matrix(vals,0,2,
                     dimnames=list(character(),
                                   c("LowerBound","UpperBound")))
    } else {
      bnds <- matrix(c(vals[1L:k],vals[2L:(k+1L)]),k,2L,
                     dimnames=list(PnodeStates(node),
                                   c("LowerBound","UpperBound")))
    }
    bnds
  } else {
    stop("This function only available for continuous nodes, but ",
         PnodeName(node), " is discrete. Use PnodeStateValues instead.")
  })

setMethod("PnodeStateBounds<-","PnodeShell", function (node,value) {
    stop("Method PnodeStateBounds<- not implemented for PnodeShell.")
  if (!is.continuous(node))
    stop("This function only available for continuous nodes, but ",
         PnodeName(node), " is discrete. Use PnodeStateValues instead.")
  k <- nrow(value)
  mon1 <- isMonotonic(value[,1L])
  direct <- attr(mon1,"direction")
  mon2 <- isMonotonic(value[,2L])
  if (!mon1 || !mon2 || direct!=attr(mon2,"direction")) {
    stop ("State bounds for node",PnodeName(node),"are not monotonic.")
  }
  if (direct>0) {                       #Increasing
    if (!all(abs(value[2L:k,1L]-value[1L:(k-1L),2L])<.002)) {
      stop("Upper and lower bounds don't match for node ",PnodeName(node))
    }
    bnds <-c(value[1L:k,1L],value[k,2L])
  } else {                              #Decreasing
    if (!all(abs(value[1L:(k-1L),1L]-value[2L:k,2L])<.002)) {
      stop("Upper and lower bounds don't match for node ",PnodeName(node))
    }
    bnds <-c(value[1L,2L],value[1L:k,1L])
  }
  NodeLevels(node) <- bnds
  if (!is.null(rownames(value)) &&
      (length(PnodeStates(node)!=k) || all(nchar(PnodeStates(node))==0L))) {
    PnodeStates(node) <- rownames(value)
  }
  invisible(node)
})

setMethod("isPnodeContinuous","PnodeShell", function (node)
    stop("Method isPnodeContinuous not implemented for PnodeShell.")
  is.continuous(node))

#### Parents



setMethod("PnodeParents","PnodeShell", function (node)
    stop("Method PnodeParents not implemented for PnodeShell.")
  NodeParents(node)
)
setMethod("PnodeParents<-","PnodeShell", function (node,value) {
    stop("Method PnodeParents<- not implemented for PnodeShell.")
  if (is.null(value)) value <- list()
  NodeParents(node) <- value
  invisible(node)
})


### Parents

setMethod("PnodeParentNames","PnodeShell", function (node) {
    stop("Method PnodeParentNames not implemented for PnodeShell.")
  if (PnodeNumParents(node)==0) {
    character()
  } else {
    parents <- NodeParents(node)
    pnames <- sapply(parents,PnodeName)
    stubsp <- sapply(parents,function(nd) NodeKind(nd)=="Stub")
    if (any(stubsp))
      pnames[stubsp] <- names(parents)[stubsp]
    pnames
  }
})

setMethod("PnodeNumParents","PnodeShell", function (node)
  length(PnodeParents(node)))


setMethod("PnodeEvidence","PnodeShell",
    stop("Method PnodeEvidence not implemented for PnodeShell.")
          function(node) {
            if (is.continuous(node)) {
              res <- NodeValue(node)
            } else {
              res <- NodeFinding(node)
              if (res=="@NO FINDING") {
                res <- NA
              } else if (res=="@LIKELIHOOD") {
                res <- NodeLikelihood(node)
              } else if (res=="@NEGATIVE FINDINGS") {
                res <- NodeLikelihood(node)
              }
            }
            res
          })


setMethod("PnodeEvidence<-",c("PnodeShell","numeric"),
    stop("Method PnodeEvidence<- not implemented for PnodeShell, numeric")
          function (node,value) {
            if (length(value) == 1L) {
              NodeValue(node) <- value
            } else if (length(value)==PnodeNumStates(node)) {
              NodeLikelihood(node) <- value
            }
            invisible(node)
          })

setOldClass("difftime")
setMethod("PnodeEvidence<-",c("PnodeShell","difftime"),
    stop("Method PnodeEvidence<- not implemented for PnodeShell, difftime.")
          function (node,value) {
            units(value) <- "secs"
            NodeValue(node) <- as.numeric(value)
          })
setMethod("PnodeEvidence<-",c("PnodeShell","character"),
    stop("Method PnodeEvidence<- not implemented for PnodeShell, character.")
          function (node,value) {
            ov1 <- value
            sts <- NodeStates(node)
            if (!(ov1 %in% sts)) {
              ov1 <- sts[toupper(sts)==toupper(ov1)]
            }
            if (length(ov1) > 0L) {
              flog.trace("Setting observable %s to %s.",NodeName(node),ov1)
              NodeFinding(node) <- ov1
            } else {
              flog.warn("Observable %s has unknown value %s, skipping.",
                        NodeName(node), ov1)
            }
            invisible(node)
          })
setMethod("PnodeEvidence<-",c("PnodeShell","factor"),
          function (node,value) {
            PnodeEvidence(node) <- as.character(value)
            })
setMethod("PnodeEvidence<-",c("PnodeShell","NULL"),
    stop("Method PnodeEvidence<- not implemented for PnodeShell, NULL.")
          function (node,value) {
            RetractNodeFinding(node)
            })
setMethod("PnodeEvidence<-",c("PnodeShell","logical"),
    stop("Method PnodeEvidence<- not implemented for PnodeShell, logical.")
          function (node,value) {
            levs <- NodeLevels(node)
            if (length(levs) != 2L) {
              PnodeEvidence(node) <- as.character(value)
            } else {
              v1 <- names(levs)[levs==as.numeric(value)]
              if (length(v1) != 1L) {
                stop("When setting ",NodeName(node)," expected ",names(levs),
                     " got ",value)
              }
              PnodeEvidence(node) <- v1
            }
            node
          })

setMethod("PnodeEvidence<-",c("PnodeShell","ANY"),
          function(node,value) {
            warning("Don't know how to set ",NodeName(node)," to ",
                 as.character(value),".  Skipping.")
          })



##############################################################
#### Net Functions

setMethod("PnetName","PnetShell", function (net){
    stop("Method PnetName not implemented for PnetShell.")
    nme <- NetworkUserField(net,"Truename")
    if (is.null(nme) || is.na(nme)) {
        nme <- NetworkName(net)
    }
    nme
})

setMethod("PnetName<-","PnetShell", function (net, value) {
    stop("Method PnetName<- not implemented for PnetShell.")
  NetworkUserField(net,"Truename")
  NetworkName(net) <- as.IDname(value)
  invisible(net)
})


setMethod("PnetTitle","PnetShell", function (net) {
    stop("Method PnetTitle not implemented for PnetShell.")
  NetworkTitle(net)
})


setMethod("PnetTitle<-","PnetShell", function (net, value) {
    stop("Method PnetTitle<- not implemented for PnetShell.")
  NetworkTitle(net) <- value
  invisible(net)
})


## The HUB is the name of the CM for an EM, or "" for an CM.
setMethod("PnetHub","PnetShell", function (net) {
    stop("Method PnetHub not implemented for PnetShell.")
  NetworkUserField(net,"Hub")
})


## Value could be the actual model or its name.
setMethod("PnetHub<-","PnetShell", function (net, value) {
    stop("Method PnetHub<- not implemented for PnetShell.")
  NetworkUserField(net,"Hub") <-value
  invisible(net)
})

## Note:  This is not necessarily the same as the GetNeticaPathname() function.
setMethod("PnetPathname","PnetShell", function (net) {
    stop("Method PnetPathname not implemented for PnetShell.")
  value <- NetworkUserField(net,"Pathname")
  if (is.na(value) || is.null(value) || nchar(value)==0L) {
    value <- attr(net,"Filename")
  }
  value
})

setMethod("PnetPathname<-","PnetShell", function (net, value) {
    stop("Method PnetPathname<- not implemented for PnetShell.")
  NetworkUserField(net,"Pathname") <-value
  invisible(net)
})

setMethod("PnetDescription","PnetShell", function (net) {
    stop("Method PnetDescription not implemented for PnetShell.")
  NetworkComment(net)
})

setMethod("PnetDescription<-","PnetShell", function (net, value) {
    stop("Method PnetDescription<- not implemented for PnetShell.")
  NetworkComment(net) <- value
  invisible(net)
})


setMethod("PnetFindNode","PnetShell", function(net,name) {
    stop("Method PnetFindNode not implemented for PnetShell.")
  NetworkFindNode(net,as.IDname(name))
})

setMethod("PnetSerialize","PnetShell",
    stop("Method PnetSerialize not implemented for PnetShell.")
          function (net) {
            factory <- net$Session$SessionName
            name <- PnetName(net)
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            WriteNetworks(net,tmpfile)
            data <- serialize(readLines(tmpfile),NULL)
            list(name=name,factory=factory,data=data)
          })


setMethod("unserializePnet","ShellSession",
    stop("Method unserializePnet not implemented for ShellSession.")
          function(factory,data) {
            name <- data$name
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            writeLines(unserialize(data$data),tmpfile)
            oldnet <- factory$findNet(name)
            if (!is.null(oldnet) && is.active(oldnet)) {
              flog.warn("Replacing old version of network %s.",
                           name)
              DeleteNetwork(oldnet)
            }
            ReadNetworks(tmpfile,factory)
          })

setMethod("PnetCompile","PnetShell",function(net) {
    stop("Method PnetCompile not implemented for PnetShell.")
    CompileNetwork(net)
    })
