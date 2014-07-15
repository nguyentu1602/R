# 00: Install relevant packages -------------------------------------------
  # Link: http://stats.stackexchange.com/questions/6155/graph-theory-analysis-and-visualization
  #       http://cran.r-project.org/web/views/gR.html
  #       http://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html
  #       http://www.graphviz.org
  #       http://sna.stanford.edu/rlabs.php

install.packages('igraph'); require(igraph) # To install package igraph

# To install package Rgraphviz and RCytoscape
source("http://bioconductor.org/biocLite.R"); 
biocLite("Rgraphviz")  # To install package Rgraphviz
biocLite("RCytoscape")
require(Rgraphviz)
require(RCytoscape)

library(RCytoscape)
cy = CytoscapeConnection()
pluginVersion (cy)

# To test package with example:
adj.mat <- matrix(sample(c(0,1), 100, replace=TRUE), nr=10)
g <- graph.adjacency(adj.mat)
plot(g)

# To install all packages in the SNA Stanford lab:
# source("http://sna.stanford.edu/setup.R")
install.packages('NetData')
install.packages('ergm')
install.packages('reshape')
install.packages('sna')
install.packages('numDeriv')
install.packages('MatchIt')
install.packages('coin')
install.packages('Hmisc')
install.packages('lattice')
install.packages('nFactors')
install.packages('NetCluster')
install.packages('igraphtosonia')
install.packages('network')

require(NetData)
require(ergm)
require(reshape)
require(sna)
require(numDeriv)
require(MatchIt)
require(coin)
require(Hmisc)
require(lattice)
require(nFactors)
require(NetCluster)
require(igraphtosonia)
require(network)



# 01: Social Network Analysis Lab Stanford --------------------------------
require(igraph)
g <- erdos.renyi.game(300, 1.5/100)
g <- barabasi.game(300, 1.5/100)
summary(g)

plot.igraph(g, layout=layout.fruchterman.reingold, vertex.size=3, vertex.label=NA, frame=TRUE)
