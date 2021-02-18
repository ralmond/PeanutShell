### Parameterized networks.


## Parameterized networks have the following properties:

## A node set called Pnodes which contains a list of all Pnodes to
## maximize.
## A field called "priorWeight" which gives the default prior weight
## to use.

## This is a total hack, but R won't let me modify Peanut after it is
## locked and loaded.
## setClassUnion("net.bridge","PnetShell")
## setIs("net.bridge","Pnet")

setMethod("as.Pnet","PnetShell",function(x) x)
setMethod("is.Pnet","PnetShell",function(x) TRUE)


## as.Pnet.PnetShell <- function (x) {
##   if (!("Pnet" %in% class(x)))
##     class(x) <- c(class(x),"Pnet")
##   x
## }

setMethod("PnetPriorWeight","PnetShell", function (net) {
    stop("Method PnetPriorWeight not implemented for PnetShell.")
  NetworkUserObj(net,"priorWeight")
})

setMethod("PnetPriorWeight<-","PnetShell", function (net,value) {
    stop("Method PnetPriorWeight<- not implemented for PnetShell.")
  NetworkUserObj(net,"priorWeight") <- value
  invisible(net)
})

setMethod("PnetPnodes","PnetShell", function (net) {
    stop("Method PnetPnodes not implemented for PnetShell.")
  NetworkNodesInSet(net,"pnodes")
})
setMethod("PnetPnodes<-","PnetShell", function (net, value) {
    stop("Method PnetPodes<- not implemented for PnetShell.")
  NetworkNodesInSet(net,"pnodes") <- value
  invisible(net)
})

## To fit PnetFactory Protocol

MakePnet.PnetShell <-function (sess,name,data) {
    stop("Method MakePnet.PnetShell not implemented.")
  pname <- as.character(data$Pathname)
  if (!is.null(pname) && nchar(pname) > 0L && file.exists(pname)) {
    net <-as.Pnet(ReadNetworks(pname,sess))
    PnetName(net) <- name
  } else {
    net <- as.Pnet(CreateNetwork(as.IDname(name),sess))
    NetworkUserField(net,"Truename") <- name
  }
  if (!is.null(data$Hub) && !is.na(data$Hub))
    PnetHub(net) <- trimws(as.character(data$Hub))
  if (!is.null(data$Title) && !is.na(data$Title))
    PnetTitle(net) <- as.character(data$Title)
  if (!is.null(data$Pathname) && !is.na(data$Pathname))
    PnetPathname(net) <- as.character(data$Pathname)
  if (!is.null(data$Description) && !is.na(data$Description))
    PnetDescription(net) <- as.character(data$Description)
  net
}

## Leave this as a no-op for now.
Free.PnetShell <- function (obj) {
    stop("Method Free.PnetShell not implemented.")
    invisible(NULL)
}


Save.PnetShell <- function (net,pathname) {
    stop("Method Save.PnetShell not implemented.")
  if (missing(pathname) || is.null(pathname))
    pathname <- PnetPathname(net)
  WriteNetworks(net,pathname)
}

Reload.PnetShell <- function (net,pathname) {
    stop("Method Reload.PnetShell not implemented.")
  if (missing(pathname) || is.null(pathname))
    pathname <- PnetPathname(net)
  DeleteNetwork(net)
  ReadNetworks(pathname)
}
Delete.PnetShell <- function (obj) {
    stop("Method Delete.PnetShell not implemented.")
  if (!is.null(obj))
    DeleteNetwork(obj)
}

### Hub and spoke model.

## To make a stub, copy the node into the new net.  It will become a stub when it is
## deleted later
setMethod("PnetMakeStubNodes","PnetShell", function (net,nodes) {
    stop("Method PnetMakeStubNodes not implemented for PnetShell.")
  if (!is.list(nodes)) nodes <- list(nodes)
  out <- CopyNodes(nodes,newnet=net)
  if (!is.list(out)) out <- list(out)
  out
})

## Deleting the node makes it a stub.
setMethod("PnetRemoveStubNodes","PnetShell", function (net,nodes) {
    stop("Method PnetRemoveStubNodes not implemented for PnetShell.")
  DeleteNodes(nodes)
})

setMethod("PnetAdjoin","PnetShell", function (hub, spoke) {
    stop("Method PnetAdjoin not implemented for PnetShell.")
  AdjoinNetwork(hub,spoke,paste("Spoke",NetworkName(spoke),sep="_"))
})

setMethod("PnetDetach","PnetShell", function (motif, spoke) {
    stop("Method PnetDetach not implemented for PnetShell.")
    ## Bug in RN_AbsorbNodes
    spokename <- paste("Spoke",NetworkName(spoke),sep="_")
    tryCatch(
        AbsorbNodes(NetworkNodesInSet(motif,spokename)),
        error = function (e) {
            flog.error("While absorbing nodes from %s in %s, got error %s",
                       spokename,NetworkName(motif),conditionMessage(e))
            flog.info("This could be a known Netica bug in version 5.04")
        })
    motif
})




## A parameterized node has the following fields:

## rules -- the name of the structure function
## link -- the name of the link function
## lnAlphas -- a list of discrimination parameters
## betas -- a list of difficulty parameters
## linkScale -- a list of scale parameters
## priorWeight -- a numeric value or a vector of numeric values for
## each row of the CPT.   Inherits from the net if not available.

setClassUnion("node.bridge","PnodeShell")
setIs("node.bridge","Pnode")

setMethod("as.Pnode","PnodeShell",function(x) {
    stop("Method as.Pnode not implemented for PnodeShell.")
  NodeSets(x) <- union("pnodes",NodeSets(x))
  if (is.na(NodeUserField(x,"Truename")))
    NodeUserField(x,"Truename") <- NodeName(x)
  x})
setMethod("is.Pnode","PnodeShell",function(x)
    stop("Method is.Pnode not implemented for PnodeShell.")
  "pnodes" %in% NodeSets(x)
  )


## as.Pnode.PnodeShell <- function (x) {
##   if (!("Pnode" %in% class(x)))
##     class(x) <- c(class(x),"Pnode")
##   x
## }

setMethod("PnodeNet","PnodeShell", function (node) {
    stop("Method PnodeNet not implemented for PnodeShell.")
  NodeNet(node)
})

setMethod("PnodeRules","PnodeShell", function (node) {
    stop("Method PnodeRules not implemented for PnodeShell.")
  NodeUserObj(node,"rules")
})

setMethod("PnodeRules<-","PnodeShell", function (node,value) {
    stop("Method PnodeRules<- not implemented for PnodeShell.")
  NodeUserObj(node,"rules") <- value
  node
})

setMethod("PnodeLink","PnodeShell", function (node) {
    stop("Method PnodeLink not implemented for PnodeShell.")
  NodeUserObj(node,"link")
})

setMethod("PnodeLink<-","PnodeShell", function (node,value) {
    stop("Method PnodeLink<- not implemented for PnodeShell.")
  NodeUserObj(node,"link") <- value
  node
})

setMethod("PnodeQ","PnodeShell", function (node) {
    stop("Method PnodeQ not implemented for PnodeShell.")
  NodeUserObj(node,"Q")
})

setMethod("PnodeQ<-","PnodeShell", function (node,value) {
    stop("Method PnodeQ<- not implemented for PnodeShell.")
  NodeUserObj(node,"Q") <- value
  node
})

setMethod("PnodeLnAlphas","PnodeShell", function (node) {
    stop("Method PnodeLnAlphas not implemented for PnodeShell.")
  NodeUserObj(node,"lnAlphas")
})

setMethod("PnodeLnAlphas<-","PnodeShell", function (node,value) {
    stop("Method PnodeLnAlphas<- not implemented for PnodeShell.")
  NodeUserObj(node,"lnAlphas") <- value
  node
})

setMethod("PnodeBetas","PnodeShell", function (node) {
    stop("Method PnodeBetas not implemented for PnodeShell.")
  NodeUserObj(node,"betas")
})

setMethod("PnodeBetas<-","PnodeShell", function (node,value) {
    stop("Method PnodeBetas<- not implemented for PnodeShell.")
  NodeUserObj(node,"betas") <- value
  node
})


setMethod("PnodeLinkScale","PnodeShell", function (node) {
    stop("Method PnodeLinkScale not implemented for PnodeShell.")
  NodeUserObj(node,"linkScale")
})

setMethod("PnodeLinkScale<-","PnodeShell", function (node,value) {
    stop("Method PnodeLinkScale not implemented for PnodeShell.")
  NodeUserObj(node,"linkScale") <- value
  node
})

setMethod("PnodePriorWeight","PnodeShell", function (node) {
    stop("Method PnodePriorWeight not implemented for PnodeShell.")
  NodeUserObj(node,"priorWeight")
})

setMethod("PnodePriorWeight<-","PnodeShell", function (node,value) {
    stop("Method PnodePriorWeight<- not implemented for PnodeShell.")
  NodeUserObj(node,"priorWeight") <- value
  node
})

setMethod("PnodePostWeight","PnodeShell", function (node) {
    stop("Method PnodePostWeight not implemented for PnodeShell.")
  NodeExperience(node)
})

setMethod("PnodeProbs","PnodeShell", function (node) {
    stop("Method PnodeProbs not implemented for PnodeShell.")
  NodeProbs(node)
})

setMethod("PnodeProbs<-","PnodeShell", function (node,value) {
    stop("Method PnodeProbs<- not implemented for PnodeShell.")
  NodeProbs(node) <- value
  node
})


setMethod("PnodeParentTvals","PnodeShell", function (node) {
    stop("Method PnodeParentTvals not implemented for PnodeShell.")
  lapply(NodeParents(node),PnodeStateValues)
})

setMethod("Pnode","PnodeShell",
    stop("Method Pnode not implemented for PnodeShell.")
          function (node, lnAlphas, betas, rules="Compensatory",
                           link="partialCredit",Q=TRUE,linkScale=NULL,
                           priorWeight=NULL) {
  if (missing(lnAlphas)) {
    if (is.list(rules)) {
      lnAlphas <- lapply(rules, function(rule) defaultAlphas(node,rule))
    } else {
      lnAlphas <- defaultAlphas(node,rules)
    }
  }
  PnodeLnAlphas(node) <- lnAlphas
  if (missing(betas)) {
    if (is.list(rules)) {
      betas <- lapply(rules, function(rule) defaultBetas(node,rule))
    } else {
      betas <- defaultBetas(node,rules)
    }
  }
  PnodeBetas(node) <- betas
  PnodeRules(node) <- rules
  PnodeLink(node) <- link
  PnodeQ(node) <- Q
  PnodeLinkScale(node) <- linkScale
  PnodePriorWeight(node) <- priorWeight
  as.Pnode(node)
})


### Build CPTs from parameters

setMethod("BuildTable","PnodeShell", function (node) {
    stop("Method BuildTable not implemented for PnodeShell.")
  if (length(PnodeBetas(node)) == 0L) {
    flog.warn("Beta vector for node %s is empty.",PnodeName(node))
  }
  if (length(PnodeAlphas(node)) == 0L) {
    flog.warn("Alpha vector for node %s is empty.",PnodeName(node))
  }
  frame <- calcDPCFrame(ParentStates(node),NodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnodeQ(node),
                        PnodeParentTvals(node))
  if (any(is.na(frame))) {
    flog.warn("Could not calculate CPT for node %s.",PnodeName(node))
  } else {
    node[] <- frame
  }
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
})


setMethod("calcPnetLLike","PnetShell", function (net,cases){
    stop("Method calcPnetLLike not implemented for PnetShell.")
  llike <- 0
  nextRec <- "FIRST"
  onodes <- NetworkNodesInSet(net,"onodes")
  pos <- 0
  stream <- CaseFileStream(cases,net$Session)
  WithOpenCaseStream(stream,
    while(!is.na(pos)) {
      ReadFindings(onodes,stream,nextRec)
      nextRec <- "NEXT"
      pos <- getCaseStreamPos(stream)
      w <- getCaseStreamLastFreq(stream)
      if (w<0) w<-1
      llike <- llike + w*log(FindingsProbability(net))
      lapply(onodes,RetractNodeFinding)
    })
  llike
})

setMethod("calcExpTables","PnetShell", function (net, cases, Estepit=1,
                                    tol=sqrt(.Machine$double.eps)) {
    stop("Method calcExpTable not implemented for PnetShell.")
  pnodes <- NetworkNodesInSet(net,"pnodes")
  casestream <- CaseFileStream(cases,session=net$Session)
  LearnCPTs(casestream,pnodes,"EM",Estepit,tol)
  invisible(net)
})


## This function is designed to suppress lack of convergence warnings,
## as we are frequently not wanting to run the M-step to convergence.
muffler <- function (w) {
  if (conditionMessage(w) == "" ||
      grepl("converge",conditionMessage(w)))
    invokeRestart("muffleWarning")
}


setMethod("maxCPTParam","PnodeShell", function (node, Mstepit=5,
                                    tol=sqrt(.Machine$double.eps)) {
    stop("Method maxCPTParam not implemented for PnetShell.")
  ## Get the posterior pseudo-counts by multiplying each row of the
  ## node's CPT by its experience.
  np <- length(NodeParents(node))
  if (np==0L) {
    counts <- NodeProbs(node)*NodeExperience(node)
  } else {
    counts <- sweep(NodeProbs(node),1L:np,NodeExperience(node),"*")
  }
  withCallingHandlers(
      est <- mapDPC(counts,ParentStates(node),NodeStates(node),
                    PnodeLnAlphas(node), PnodeBetas(node),
                    PnodeRules(node),PnodeLink(node),
                    PnodeLinkScale(node),PnodeQ(node),
                    PnodeParentTvals(node),
                    control=list(reltol=tol,maxit=Mstepit)
                    ),
      warning=muffler)
  PnodeLnAlphas(node) <- est$lnAlphas
  PnodeBetas(node) <- est$betas
  PnodeLinkScale(node) <- est$linkScale
  invisible(node)
})

### Implementation of the factory protocol

## No-op for now.  Explicitly call delete.
Free.PnodeShell <-function (obj) {
    stop("Method Free.PnodeShell not implemented.")
  invisible(NULL)
}

Delete.PnodeShell <- function (obj) {
    stop("Method Delete.PnodeShell not implemented.")
  DeleteNodes(obj)
}

MakePnode.PnodeShell <- function (net, name, data) {
    stop("Method MakePnode.PnodeShell not implemented.")
  if (nrow(data) != as.integer(data$Nstates[1]))
    stop("Must be one row in data for each state.")
  node <-PnetFindNode(net,name)
  cont <- isTRUE(as.logical(data$Continuous[1]))
  if (is.null(node)) {
    if (cont)
      node <- NewContinuousNode(net,as.IDname(name))
    else
      node <- NewDiscreteNode(net,as.IDname(name),
                              trimws(as.character(data$StateName)))
    NodeUserField(node,"Truename") <- name
  }
  node <- as.Pnode(node)
  if (!is.null(data$NodeTitle) && !is.na(data$NodeTitle))
    PnodeTitle(node) <- as.character(data$NodeTitle[1])
  if (!is.null(data$NodeDescription) &&
      !is.na(data$NodeDescription))
    PnodeDescription(node) <- as.character(data$NodeDescription[1])
  if (!is.null(data$NodeLabels) && !is.na(data$NodeLabels)) {
    labels <- strsplit(data$NodeLabels[1],",")[[1]]
    PnodeLabels(node) <- as.character(labels)
  }
  if (cont) {
    ## Need to set values to create states.
    valmat <- cbind(as.numeric(data$LowerBound),
                    as.numeric(data$UpperBound))
    if (any(is.na(valmat))) {
      warning("NAs in states bounds for node",name)
    } else {
      PnodeStateBounds(node) <-valmat
    }
  }
  PnodeStates(node) <- trimws(as.character(data$StateName))
  if (!is.null(data$StateTitle)) {
    titles <- as.character(data$StateTitle)
    if (all(!is.na(titles)))
      PnodeStateTitles(node) <- titles
  }
  if (!is.null(data$StateDescription)) {
    desc <- as.character(data$StateDescription)
    if (all(!is.na(desc)))
      PnodeStateDescriptions(node) <- desc
  }
  if (!cont && !is.null(data$StateValue) && !any(is.na(data$StateValue)))
    PnodeStateValues(node) <- as.numeric(data$StateValue)

  node

}
