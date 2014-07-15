require(igraph)
install.packages('ROCR')
require(ROCR)

sampleData1 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph1.txt')
G1 <- graph.data.frame(sampleData1, directed=TRUE)
sampleData2 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph2.txt')
G2 <- graph.data.frame(sampleData2, directed=TRUE)
sampleData3 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph3.txt')
G3 <- graph.data.frame(sampleData3, directed=TRUE)
sampleData4 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph4.txt')
G4 <- graph.data.frame(sampleData4, directed=TRUE)
sampleData5 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph5.txt')
G5 <- graph.data.frame(sampleData5, directed=TRUE)
sampleData6 <- read.table('/Users/cuongnguyen/Dropbox/00.Class/0.S14/MAT499/sampleGraph6.txt')
G6 <- graph.data.frame(sampleData6, directed=TRUE)

get.vertex.attribute(graph=G4, name="name",index=get.shortest.paths(graph=G4, from="10", to="8")[[1]])
get.vertex.attribute(graph=G4, name="name",index=get.shortest.paths(graph=G4, from="10", to="8", mode ="out")[[1]])
get.vertex.attribute(graph=G4, name="name",index=get.shortest.paths(graph=G4, from="4", to="8", mode ="out")[[1]])
get.vertex.attribute(graph=G4, name="name",index=get.shortest.paths(graph=G3, from="4", to="8", mode ="out")[[1]])


path[[1]]
# Plot grpah from t=1 to t=4:
plot.igraph(G1, xlab='Graph G1 (Time = 1)', 
            vertex.size = 25, 
            edge.label= sampleData1$V3 , edge.color = "black", edge.lty = 1,
            edge.width = 2 , edge.arrow.width = 0.5, edge.label.color = "brown", margin = -0.2)


plot.igraph(G4, xlab='Graph G1 (Time = 1)', 
            vertex.size = 25, 
            edge.label= sampleData1$V3 , edge.color = "black", edge.lty = 1,
            edge.width = 2 , edge.arrow.width = 0.5, edge.label.color = "brown", margin = -0.2)

plot.igraph(G2, main='Graph 2', layout=layout.grid, vertex.size = 20, edge.label= sampleData2$V3 )
plot.igraph(G3, layout=layout.grid, vertex.size = 20, edge.label= sampleData3$V3 )
plot.igraph(G4, vertex.size = 20, edge.label= sampleData4$V3 )
plot.igraph(G5, layout=layout.grid, vertex.size = 20, edge.label= sampleData5$V3 )
plot.igraph(G6, layout=layout.grid, vertex.size = 20, edge.label= sampleData6$V3 )

#rglplot.igraph(sampleGraph)

# Build a list of edges from the latest graph, i.e. sampleGraph4
G1.e <- get.edgelist(G1)  # Extract all edges
G2.e <- get.edgelist(G2)  # Extract all edges
G3.e <- get.edgelist(G3)  # Extract all edges
G4.e <- get.edgelist(G4)  # Extract all edges
G5.e <- get.edgelist(G5)  # Extract all edges
G6.e <- get.edgelist(G6)  # Extract all edges

# Extract features (x) and target (t) for G3

G1.e <- cbind(G1.e, paste(G1.e[,1], G1.e[,2]))  # add a column of edge names
G2.e <- cbind(G2.e, paste(G2.e[,1], G2.e[,2]))  # add a column of edge names
G4.e <- cbind(G4.e, paste(G4.e[,1], G4.e[,2]))  # add a column of edge names

G1.e <- data.frame(G1.e)
G2.e <- data.frame(G2.e)
G4.e <- data.frame(G4.e)


G3.e <- cbind(G3.e, paste(G3.e[,1], G3.e[,2]))  # add a column of edge names
G3.e <- cbind(G3.e, matrix(nrow=length(G3.e[,1]),ncol=27))

G3.e <- data.frame(G3.e)

names(G3.e) <- c('from',"to","nodeName",'inT.1','inT.2','weight','fromInDegree','fromOutDegree',
                 'toInDegree','toOutDegree','fromECentrality', 'toECentrality',
                 'fromCloseness', 'toCloseness','betweeness','fromInDegreeT.1','fromOutDegreeT.1',
                 'toInDegreeT.1','toOutDegreeT.1','fromECentralityT.1', 'toECentralityT.1',
                 'fromClosenessT.1', 'toClosenessT.1','fromInDegreeT.2','fromOutDegreeT.2',
                 'toInDegreeT.2','toOutDegreeT.2')

  ## Extracting Features for G3.e from G3
G3.e$inT.1 <-  (G3.e$nodeName %in% G2.e$X3) 
G3.e$inT.2 <-  (G3.e$nodeName %in% G1.e$X3) 
G3.e$weight <- get.edge.attribute(G3,name='V3')
G3.e$fromInDegree <- degree(G3,v=G3.e[,1],mode='in')
G3.e$fromOutDegree <- degree(G3,v=G3.e[,1],mode='out')
G3.e$toInDegree <- degree(G3,v=G3.e[,2],mode='in')
G3.e$toOutDegree <- degree(G3,v=G3.e[,2],mode='out')
G3.e$fromECentrality <- alpha.centrality(graph=G3,nodes=G3.e[,1])
G3.e$toECentrality <- alpha.centrality(graph=G3,nodes=G3.e[,2])
G3.e$fromCloseness <- closeness(graph=G3, vids=G3.e[,1],mode = c("out", "in", "all", "total") )
G3.e$toCloseness <- closeness(graph=G3, vids=G3.e[,2],mode = c("out", "in", "all", "total") )
G3.e$betweeness <- edge.betweenness(G3, e=E(G3), directed=TRUE, weights=NULL)

  ## Extracting Features for G3.e from G2
G3.e$fromInDegreeT.1 <- degree(G2,v=G3.e[,1],mode='in')
G3.e$fromOutDegreeT.1 <- degree(G2,v=G3.e[,1],mode='out')
G3.e$toInDegreeT.1 <- degree(G2,v=G3.e[,2],mode='in')
G3.e$toOutDegreeT.1 <- degree(G2,v=G3.e[,2],mode='out')
G3.e$fromECentralityT.1 <- alpha.centrality(graph=G2,nodes=G3.e[,1])
G3.e$toECentralityT.1 <- alpha.centrality(graph=G2,nodes=G3.e[,2])
G3.e$fromClosenessT.1 <- closeness(graph=G2, vids=G3.e[,1],mode = c("out", "in", "all", "total") )
G3.e$toClosenessT.1 <- closeness(graph=G2, vids=G3.e[,2],mode = c("out", "in", "all", "total") )

  ## Extracting Features for G3.e from G1 [TODO!]

  ## Extracting Target for G3.e from G4 [TODO!]
G3.e$survive <- (G3.e$nodeName %in% G4.e$X3) 
G3.e
attach(G3.e)
model <- lm(survive ~ fromInDegree + toInDegree + inT.1 + inT.2 + weight + fromInDegree+fromOutDegree+
          toInDegree+toOutDegree+fromECentrality+toECentrality + fromCloseness + toCloseness + betweeness)
summary(model)
logit.model <- glm(formula=survive ~ weight+fromECentrality+toECentrality+fromCloseness+toCloseness+betweeness, family=binomial(logit))
summary(logit.model)
predict(logit.model,type="response")

# Extract features (x) and target (t) for G4

# Extract features (x) and target (t) for G5



G3.e$inG1 <-  ((G3.e$from %in% G1.e[,1]) & (G3.e$to %in% G1.e[,2]))
G3.e$inG2 <-  ((G3.e$from %in% G2.e[,1]) & (G3.e$to %in% G2.e[,2]))
G3.e$inG3 <-  ((G3.e$from %in% G3.e[,1]) & (G3.e$to %in% G3.e[,2]))
G3.e$weight <- get.edge.attribute(G4,name='V3')
G3.e$fromInDegree <- degree(G4,v=G4.e[,1],mode='in')
G3.e$fromOutDegree <- degree(G4,v=G4.e[,1],mode='out')
G3.e$toInDegree <- degree(G4,v=G4.e[,2],mode='in')
G3.e$toOutDegree <- degree(G4,v=G4.e[,2],mode='out')
G3.e$fromECentrality <- alpha.centrality(graph=G4,nodes=G4.e[,1])
G3.e$toECentrality <- alpha.centrality(graph=G4,nodes=G4.e[,2])

G3.e$fromCloseness <- closeness(graph=G4, vids=G4.e[,1],mode = c("out", "in", "all", "total") )
G3.e$toCloseness <- closeness(graph=G4, vids=G4.e[,2],mode = c("out", "in", "all", "total") )
G3.e$betweeness <- edge.betweenness(G4, e=E(G4), directed=TRUE, weights=NULL)

G3

# Possible variables to consider: betweeness, closeness, 