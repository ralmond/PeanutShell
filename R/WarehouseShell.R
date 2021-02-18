PnetShellWarehouse <- setClass("PNetShellWarehouse",
                       slots=c(manifest="data.frame",
                               address="character",
                               key="character",
                               prefix="character")
                       )
PnetShellWarehouse <- function(manifest=data.frame(),
                        address=".",key=c("Name"),prefix="S")
  new("PnetShellWarehouse",manifest=manifest, address=address,
      key=key, prefix=prefix)

setIs("PnetShellWarehouse","PnetWarehouse")

setMethod(ClearWarehouse,"PnetShellWarehouse",
          function(warehouse) {
              stop("Method ClearWarehouse not implemented for PnetShellWarehouse.")
            objs <- objects(warehouse@session$nets)
            for (obj in objs) {
              net <- warehouse@session$nets[[obj]]
              if (is.NeticaBN(net) && is.active(net)) {
                flog.trace("Clearing Network %s",obj)
                DeleteNetwork(net)
              }
            }
          })

setMethod(WarehouseManifest,"PnetShellWarehouse",
          function(warehouse) {
              stop("Method WarehouseManifest not implemented for PnetShellWarehouse.")
              warehouse@manifest})
setMethod("WarehouseManifest<-",c("PnetShellWarehouse","data.frame"),
          function(warehouse,value) {
              stop("Method WarehouseManifest<- not implemented for PnetShellWarehouse.")
              for (ky in warehouse@key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<- value; warehouse})


setGeneric("WarehouseDirectory",
           function (warehouse) standardGeneric("WarehouseDirectory"))
setMethod("WarehouseDirectory","PnetShellWarehouse",
          function (warehouse) {
              stop("Method WarehouseDirectory not implemented for PnetShellWarehouse.")
              warehouse@address
          })
setGeneric("WarehouseDirectory<-",
           function (warehouse, value) standardGeneric("WarehouseDirectory<-"))
setMethod("WarehouseDirectory<-","PnetShellWarehouse",
          function (warehouse,value) {
              stop("Method WarehouseDirectory<- not implemented for PnetShellWarehouse.")
              warehouse@address <- value
            warehouse
          })



setMethod(WarehouseData,"PnetShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseData not implemented for PnetShellWarehouse.")
            key <- warehouse@key
            if (length(name) != length(key))
              stop("Expected name to contain elements",key)
            manifest <- warehouse@manifest
            whch = rep(TRUE,nrow(manifest))
            for (i in 1:length(key)) {
              whch <- whch & manifest[[key[i]]] == name[i]
            }
            dat <- manifest[whch,,drop=FALSE]
            ## Add directory information to pathnames.
            dir <- do.call("file.path",as.list(warehouse@address))
            if (length(dir) > 0L)
              dat$Pathname <- file.path(dir,dat$Pathname)
            dat
          })

setMethod(WarehouseFetch,"PnetShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseFetch not implemented for PnetShellWarehouse.")
            if (length(name) != 1L)
              stop("Expected name ",name," to be unique.")
            warehouse@session$nets[[as.IDname(name)]]
          })

setMethod("WarehouseSupply", c("ANY"), function(warehouse,name) {
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  if (!is.active(val)) {
    warehouse@session$nets[[as.IDname(name)]] <- NULL
    val <- WarehouseMake(warehouse,name)
  }
  val
})


setMethod(WarehouseMake,"PnetShellWarehouse",
              stop("Method WarehouseMake not implemented for PnetShellWarehouse.")
          function(warehouse,name) {
            if (length(name) != 1L)
              stop("Expected name to be unique.")
            dat <- WarehouseData(warehouse,name)
            if (nrow(dat) <1L)
              stop("Cannot find manifest data for network ",name)
            if (nrow(dat) >2L)
              warning("Multiple manifest data row for network ",name)
            sess <- warehouse@session
            if (!is.null(sess$nets[[as.IDname(name)]])) {
              warning("Deleting old network ",name)
              DeleteNetwork(sess$nets[[as.IDname(name)]])
            }
            MakePnet.NeticaBN(sess,name,dat)
          })

setMethod(WarehouseSave,c("PnetShellWarehouse","character"),
          function(warehouse,obj) {
              stop("Method WarehouseSave not implemented for PnetShellWarehouse, character.")
            net <- warehouse@session$nets[[as.IDname(obj)]]
            if (is.null(net)) {
              warning("Network named ",obj," does not exist, not saving.")
            } else {
              WarehouseSave(warehouse,net)
            }
          })

setMethod(WarehouseSave,c("PnetShellWarehouse","PnetShell"),
          function(warehouse,obj) {
              stop("Method WarehouseSave not implemented for PnetShellWarehouse, PnetShell.")
            name <- PnetName(obj)
            pname <- WarehouseData(warehouse,name)$Pathname
            WriteNetworks(obj,pname)
          })


setMethod(WarehouseFree,"PnetShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseFree not implemented for PnetShellWarehouse.")
            net <- WarehouseFetch(warehouse,name)
            if (is.null(net)) {
              flog.trace("Network for name %s not found, skipping.",name)
            } else {
              if (is.active(net))
                DeleteNetwork(net)
              if (!is.null(warehouse@session$nets[[name]]))
                rm(list=name,envir=warehouse@session$nets)
            }
          })

setMethod(WarehouseCopy,c("PnetShellWarehouse","PnetShell"),
          function(warehouse,obj,newname) {
              stop("Method WarehouseCopy not implemented for PnetShellWarehouse,PnetShell")
            newname <- as.legal.name(warehouse,newname)
            CopyNetworks(obj,newname)
          })

setMethod(is.legal.name,"PnetShellWarehouse",
          function(warehouse,name)
              stop("Method is.legal.name not implemented for PnetShellWarehouse.")
            is.IDname(name)
          )

setMethod(as.legal.name,"PnetShellWarehouse",
          function(warehouse,name)
              stop("Method as.legal.name not implemented for PnetShellWarehouse.")
            as.IDname(name,warehouse@prefix)
          )

setMethod(is.valid,"PnetShellWarehouse",
          function(warehouse,object)
              stop("Method is.valid not implemented for PnetShellWarehouse.")
            is.active(object)
          )


setMethod(WarehouseInventory,"PnetShellWarehouse",
          function(warehouse) {
              stop("Method WarehouseInventory not implemented for PnetShellWarehouse.")
            allKeys <- warehouse@manifest[,warehouse@key,drop=FALSE]
            built <- sapply(1L:nrow(allKeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allKeys[built, ,drop=FALSE]})

setMethod(is.PnetWarehouse,"PnetShellWarehouse",
          function(obj) {TRUE})

setMethod("WarehouseUnpack", "PnetShellWarehouse",
          function(warehouse,serial) {
              stop("Method WarehouseUnpack not implemented for PnetShellWarehouse.")
            unserializePnet(warehouse@session,serial)
            warehouse@session$nets[[as.IDname(serial$name)]]
          })



#######  Node Warehouse

PnodeShellWarehouse <- setClass("PnodeShellWarehouse",
                       slots=c(manifest="data.frame",
                               key="character",
                               prefix="character")
                       )
PnodeShellWarehouse <- function(manifest=data.frame(),
                        key=c("Model","NodeName"),prefix="V")
  new("PnodeShellWarehouse",manifest=manifest, 
      key=key, prefix=prefix)

setIs("PnodeShellWarehouse","PnodeWarehouse")

setMethod(ClearWarehouse,"PnodeShellWarehouse",
          function(warehouse) {
            warning("To clear warehouse, delete and recreate network.")
          })


setMethod(WarehouseManifest,"PnodeShellWarehouse",
          function(warehouse) {
              stop("Method WarehouseManifest not implemented for PnodeShellWarehouse.")
              warehouse@manifest})
setMethod("WarehouseManifest<-",c("PnodeShellWarehouse","data.frame"),
          function(warehouse,value) {
              stop("Method WarehouseManifest<- not implemented for PnodeShellWarehouse.")
            for (ky in warehouse@key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<-value;
            warehouse})

setMethod(WarehouseData,"PnodeShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseData not implemented for PnodeShellWarehouse.")
            key <- warehouse@key
            if (length(name) != length(key))
              stop("Expected name to contain elements",key)
            manifest <- warehouse@manifest
            whch = rep(TRUE,nrow(manifest))
            for (i in 1:length(key)) {
              whch <- whch & manifest[[key[i]]] == name[i]
            }
            manifest[whch,,drop=FALSE]
          })

setMethod(WarehouseFetch,"PnodeShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseFetch not implemented for PnodeShellWarehouse.")
            if (length(name) != 2L)
              stop("Expected key to look like (net, node).")
            sess <- warehouse@session
            sess$nets[[as.IDname(name[1])]]$nodes[[as.IDname(name[2])]]
          })

setMethod(WarehouseMake,"PnodeShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseMake not implemented for PnodeShellWarehouse.")
            if (length(name) != 2L)
              stop("Expected name to be of the form (net,node).")
            net <- warehouse@session$nets[[as.IDname(name[1])]]
            if (is.null(net)) {
              stop("Network ",name[1]," does not exist.")
            }
            if (!is.null(net$nodes[[as.IDname(name[2])]])) {
              warning("Deleting old node ",paste(name,collapse="::"))
              DeleteNodes(net$nodes[[as.IDname(name[2])]])
            }
            dat <- WarehouseData(warehouse,name)
            MakePnode.PnodeShell(net,name[2],dat)
          })


setMethod(WarehouseFree,"PnodeShellWarehouse",
          function(warehouse,name) {
              stop("Method WarehouseFree not implemented for PnodeShellWarehouse.")
            node <- WarehouseFetch(warehouse,name)
            if (is.null(node)) {
              flog.trace("Node for name %s not found, skipping.",name)
            } else {
              if (is.active(node))
                DeleteNodes(node)
            }
          })

setMethod(WarehouseSave,"PnodeShellWarehouse",
          function(warehouse,obj) {
              stop("Method WarehouseSave not implemented for PnodeShellWarehouse.")
          })   #Null Action.


setMethod(WarehouseCopy,c("PnodeShellWarehouse","PnodeShell"),
          function(warehouse,obj,newname) {
              stop("Method WarehouseCopy not implemented for PnodeShellWarehouse.")
            newname <- as.legal.name(warehouse,newname)
            if (length(newname) != 2L)
              stop("Expected key to look like (net, node).")
            newnet <- warehouse@session$nets[[newname[1]]]
            if (is.null(newnet))
              stop("Network ",newname[1]," does not exist.")
            CopyNodes(obj,newname[2],newnet=newnet)
          })

setMethod(is.legal.name,"PnodeShellWarehouse",
          function(warehouse,name)
              stop("Method is.legal.name not implemented for PnodeShellWarehouse.")
            is.IDname(name)
          )

setMethod(as.legal.name,"PnodeShellWarehouse",
          function(warehouse,name)
              stop("Method as.legal.name not implemented for PnodeShellWarehouse.")
            as.IDname(name,warehouse@prefix)
          )

setMethod(is.valid,"PnodeShellWarehouse",
          function(warehouse,object)
              stop("Method is.valid not implemented for PnodeShellWarehouse.")
            is.active(object)
          )

setMethod(is.PnodeWarehouse,"PnodeShellWarehouse",
          function(obj) {TRUE})

setMethod(WarehouseInventory,"PnodeShellWarehouse",
          function(warehouse) {
              stop("Method WarehouseInventory not implemented for PnodeShellWarehouse.")
            allKeys <- warehouse@manifest[,warehouse@key,drop=FALSE]
            built <- sapply(1L:nrow(allKeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allKeys[built, ,drop=FALSE]})
