library("bnlearn")
library("gRain")
library('visNetwork')
library('igraph')
library('graph')
library("igraph")
#library("tidyr")

setClass(
  "CommunityOfNetworks",
  representation(
    network1 = "bn.fit",
    network2 = "bn.fit",
    network3 = "bn.fit",
    network4 = "bn.fit",
    network5 = "bn.fit"
  )
)

setClass("BayesianNetwork",
         representation(network1 = "bn.fit"))


set.seed(9001)
fitted.simpler_cog_1 <- readRDS('data/cog1_age_APOE.RDS')
fitted.simpler_cog_2 <- readRDS('data/cog2_age_APOE.RDS')
fitted.simpler_cog_3 <- readRDS('data/cog3_age_APOE.RDS')
fitted.simpler_cog_4 <- readRDS('data/cog4_age_APOE.RDS')
fitted.simpler_cog_5 <- readRDS('data/cog5_age_APOE.RDS')
fitted.simpler_cog_6 <- readRDS('data/cog1_no_corr.RDS')

#dir = "/restricted/projectnb/necs/Ana_analysis/multiomics_analysis/BN_modeling/TICS_Memory_scores/data/"
llfs_1_q1<- readRDS("data/llfs_1_q1.RDS")
llfs_2_q1<- readRDS("data/llfs_1_q1.RDS")
llfs_3_q1<- readRDS("data/llfs_1_q1.RDS")
llfs_4_q1<- readRDS("data/llfs_1_q1.RDS")
llfs_5_q1<- readRDS("data/llfs_1_q1.RDS")

llfs_1_q2<- readRDS("data/llfs_1_q2.RDS")
llfs_2_q2<- readRDS("data/llfs_1_q2.RDS")
llfs_3_q2<- readRDS("data/llfs_1_q2.RDS")
llfs_4_q2<- readRDS("data/llfs_1_q2.RDS")
llfs_5_q2<- readRDS("data/llfs_1_q2.RDS")

llfs_1_q3<- readRDS("data/llfs_1_q3.RDS")
llfs_2_q3<- readRDS("data/llfs_1_q3.RDS")
llfs_3_q3<- readRDS("data/llfs_1_q3.RDS")
llfs_4_q3<- readRDS("data/llfs_1_q3.RDS")
llfs_5_q3<- readRDS("data/llfs_1_q3.RDS")

llfs_1_q4<- readRDS("data/llfs_1_q4.RDS")
llfs_2_q4<- readRDS("data/llfs_1_q4.RDS")
llfs_3_q4<- readRDS("data/llfs_1_q4.RDS")
llfs_4_q4<- readRDS("data/llfs_1_q4.RDS")
llfs_5_q4<- readRDS("data/llfs_1_q4.RDS")


fitted.simpler_cog_trp_ky_ <-
  readRDS('data/fitted.simpler_cog_trp_ky_.RDS')
fitted.simpler_cog_llfs_trp_ky <-
  readRDS('data/fitted.simpler_cog_llfs_trp_ky.RDS')
fitted.simpler_cog_tr_ky_llfs_gs <-
  readRDS('data/fitted.simpler_cog_KY_LLFS.RDS')
fitted.simpler_cog_tr_ky_necs_gs <-
  readRDS('data/fitted.simpler_cog_KY_NECS.RDS')

fitted.simpler_cog_1_NECS <- readRDS('data/cog1_NECS.RDS')
fitted.simpler_cog_2_NECS <- readRDS('data/cog2_NECS.RDS')
fitted.simpler_cog_3_NECS <- readRDS('data/cog3_NECS.RDS')
fitted.simpler_cog_4_NECS <- readRDS('data/cog4_NECS.RDS')
fitted.simpler_cog_5_NECS <- readRDS('data/cog5_NECS.RDS')

fitted.simpler_cog_1_NECS_genetics <-
  readRDS('data/cog1_NECS_viz.RDS')
fitted.simpler_cog_2_NECS_genetics <-
  readRDS('data/cog2_NECS_viz.RDS')
fitted.simpler_cog_3_NECS_genetics <-
  readRDS('data/cog3_NECS_viz.RDS')
fitted.simpler_cog_4_NECS_genetics <-
  readRDS('data/cog4_NECS_viz.RDS')
fitted.simpler_cog_5_NECS_genetics <-
  readRDS('data/cog5_NECS_viz.RDS')

NBetKYNECSGS <-
  new("BayesianNetwork",
      network1 = fitted.simpler_cog_tr_ky_necs_gs)

NBetKYLLFSGS <-
  new("BayesianNetwork",
      network1 = fitted.simpler_cog_tr_ky_llfs_gs)

BNetKYLLFS <-
  new("BayesianNetwork",
      network1 = fitted.simpler_cog_llfs_trp_ky)


BNetKYNECS <-
  new("BayesianNetwork",
      network1 = fitted.simpler_cog_trp_ky_)


ComNetSyntegra <-
  new(
    "CommunityOfNetworks",
    network1 = fitted.simpler_cog_1,
    network2 = fitted.simpler_cog_2,
    network3 = fitted.simpler_cog_3,
    network4 = fitted.simpler_cog_4,
    network5 = fitted.simpler_cog_5
  )



ComNetNECS <-
  new(
    "CommunityOfNetworks",
    network1 = fitted.simpler_cog_1_NECS,
    network2 = fitted.simpler_cog_2_NECS,
    network3 = fitted.simpler_cog_3_NECS,
    network4 = fitted.simpler_cog_4_NECS,
    network5 = fitted.simpler_cog_5_NECS
  )


ComNetNECSgenetics <-
  new(
    "CommunityOfNetworks",
    network1 = fitted.simpler_cog_1_NECS_genetics,
    network2 = fitted.simpler_cog_2_NECS_genetics,
    network3 = fitted.simpler_cog_3_NECS_genetics,
    network4 = fitted.simpler_cog_4_NECS_genetics,
    network5 = fitted.simpler_cog_5_NECS_genetics
  )

ComNetLLFSQ1<- new(
  "CommunityOfNetworks",
  network1 = llfs_1_q1,
  network2 = llfs_2_q1,
  network3 = llfs_3_q1,
  network4 = llfs_4_q1,
  network5 = llfs_5_q1
)

ComNetLLFSQ2<- new(
  "CommunityOfNetworks",
  network1 = llfs_1_q2,
  network2 = llfs_2_q2,
  network3 = llfs_3_q2,
  network4 = llfs_4_q2,
  network5 = llfs_5_q2
)

ComNetLLFSQ3<- new(
  "CommunityOfNetworks",
  network1 = llfs_1_q3,
  network2 = llfs_2_q3,
  network3 = llfs_3_q3,
  network4 = llfs_4_q3,
  network5 = llfs_5_q3
)

ComNetLLFSQ4<- new(
  "CommunityOfNetworks",
  network1 = llfs_1_q4,
  network2 = llfs_2_q4,
  network3 = llfs_3_q4,
  network4 = llfs_4_q4,
  network5 = llfs_5_q4
)

.getMBString <- function(obj, net, node) {
  mbList <- mb(slot(obj, net), node)
  mbString <- paste(mbList, collapse = ", ")
  
  return (mbString)
}

getMBTable <- function(Obj) {
  obj <- getNetObj(Obj)
  
  NodeList <- c()
  for (i in slotNames(obj)) {
    nodes(slot(obj, i))
    NodeList <-
      rbind(NodeList, data.frame(Node = unlist(nodes(slot(
        obj, i
      )))))
  }
  NodeUnique <- unique(NodeList)
  
  MBList <- c()
  for (net in slotNames(obj)) {
    for (node in NodeUnique$Node) {
      if (node %in% nodes(slot(obj, net))) {
        MBList <-
          rbind(MBList, data.frame(
            Node = node,
            Network = net,
            MB = (ifelse(
              length(mb(slot(obj, net), node)) == 0, " ",
              .getMBString(obj, net, node)
            ))
          ))
      }
    }
  }
  
  MBList <-
    as.data.frame(tidyr::pivot_wider(
      MBList,
      id_cols = Node,
      names_from = Network,
      values_from = MB
    ))
  
  return(MBList)
}
testNetwork <- "network1"
Obj <- "Syntegra data"
obj <- ComNetSyntegra
test_network <- slot(obj, "network1")
junction <- compile(as.grain(test_network))

getNetObj <- function(Obj) {
  if (Obj == "Syntegra data") {
    obj <- ComNetSyntegra
  }
  if (Obj == "Tryprophan pathway-NECS") {
    obj <- BNetKYNECS
  }
  if (Obj == "Tryprophan pathway-LLFS") {
    obj <- BNetKYLLFS
  }
  if (Obj == "Tryprophan pathway-NECS-greedy-search") {
    obj <- NBetKYNECSGS
  }
  if (Obj == "Tryprophan pathway-LLFS-greedy-search") {
    obj <- NBetKYLLFSGS
  }
  if (Obj == "NECS") {
    obj <- ComNetNECS
  }
  if (Obj == "NECS-genetics") {
    obj <- ComNetNECSgenetics
  }
  if (Obj == "LLFS-age-q1") {
    obj <- ComNetLLFSQ1
  }
  if (Obj == "LLFS-age-q2") {
    obj <- ComNetLLFSQ2
  }
  if (Obj == "LLFS-age-q3") {
    obj <- ComNetLLFSQ3
  }
  if (Obj == "LLFS-age-q4") {
    obj <- ComNetLLFSQ4
  }
  return(obj)
}

getTestNetwork <- function(obj, testNetwork) {
  testNet <- slot(obj, testNetwork)
  return(testNet)
}


getVizNetDataAnn <- function(Obj, testNetwork) {
  obj <- getNetObj(Obj)
  test_network <- getTestNetwork(obj, testNetwork)
  
  g <- as.graphNEL(test_network)
  net <- as(g, "matrix")
  g1 <- graph.adjacency(net, mode = "directed", weighted = NULL)
  data_ann <- toVisNetworkData(g1)
  
  return(data_ann)
}

getProbTable <-
  function(Obj,
           testNetwork,
           setEvi,
           node2,
           distType,
           node4) {
    obj <- getNetObj(Obj)
    test_network <- getTestNetwork(obj, testNetwork)
    
    junction <- compile(as.grain(test_network))
    
    values <- junction$universe$levels[[setEvi]]
    tabqGrainOut <- c()
    for (i in values) {
      setEvid <- setEvidence(junction, nodes = setEvi, states = i)
      qGrainOut <-
        qgrain(setEvid, nodes = c(node2), type = distType)
      qGrainOut <- as.data.frame(qGrainOut)
      #colnames(qGrainOut)<-  paste(setEvi, sep=" = ", i)
      #colnames(qGrainOut) <- "Probability"
      #rownames(qGrainOut) <-
      #  paste(paste(setEvi, " = ", i), node2, " = ", rownames(qGrainOut))
      #tabqGrainOut <- rbind(tabqGrainOut, qGrainOut)
      qGrainOut <- t(qGrainOut)
      rownames(qGrainOut) <- paste(setEvi, " = ", i)
      if (setEvi == node2)
      colnames(qGrainOut) <- paste(node2, " = ", colnames(qGrainOut))
      tabqGrainOut <- rbind(tabqGrainOut, qGrainOut)
      
    }
    tabqGrainOut<- t(tabqGrainOut)
    return (tabqGrainOut)
  }




getMargProbTable <- function(Obj, testNetwork, node4) {
  obj <- getNetObj(Obj)
  test_network <- getTestNetwork(obj, testNetwork)
  
  junction <- compile(as.grain(test_network))
  qGrainOut <- qgrain(junction, nodes = c(node4), type = "marginal")
  
  return(qGrainOut)
}

getCommunityNetworkTable <-
  function(Obj,
           testNetwork,
           setEvi,
           node2,
           distType,
           node4) {
    queryResultArray <- c()
    obj <- getNetObj(Obj)
    
    for (network in slotNames(obj)) {
      test_network <- getTestNetwork(obj, network)
      if (setEvi %in% nodes(test_network) &
          node2 %in% nodes(test_network)) {
        queryResult <-
          t(getProbTable(Obj, network, setEvi, node2, distType, node4))
        queryResultArray <- rbind(queryResultArray, queryResult)
      }
    }
    communityNetTable<- c()
    for (item in unique(rownames(queryResultArray))){
    tempTabl<- subset(queryResultArray, rownames(queryResultArray) %in% item)
    tempTablMean<- t(as.data.frame(sapply(as.data.frame(tempTabl), mean)))
    rownames(tempTablMean)<- item
    communityNetTable<- rbind(communityNetTable, tempTablMean)
    
    }
    
    
    #communityNetTablet<- t(communityNetTable)
    #colnames(communityNetTable) <- "Probability"
    
    communityNetTable<- t(communityNetTable)
    return(communityNetTable)
    
  }



# Load ggplot2
library(ggplot2)
