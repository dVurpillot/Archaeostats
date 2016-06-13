library(igraph)
library(ape)

#Data
links2 <- read.csv("A MODIFIER /Graphe.csv", sep=";", header=T, as.is=T)
links2 <- data.frame(links2[,-1], row.names=links2[,1])

#Graph setup
head(links2)
net2 <- graph.incidence(links2, directed = F, mode = "in")

#Edges setup
E(net2)$width <- degree(net2)/10
E(net2)$arrow.size <- .2

#Vectors setup
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$color2 <- c("steel blue", "red")[V(net2)$type+1]
V(net2)$color3 <- c("white", "black")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
V(net2)$label.cex=.6
V(net2)$label.font=2

#Plot1
plot(net2, vertex.label.color=V(net2)$color3, vertex.size=(2-V(net2)$type)*3, edge.curved=.1)

#Vectors setup
V(net2)$degree <- degree(net2)
V(net2)$size<-degree(net2)/8                                    
V(net2)$frame.color <- NA

#Plot2
plot(net2,				#the graph to be plotted
     layout=layout.graphopt,	# the layout method. see the igraph documentation for details
     main='graphopt',	#specifies the title
     edge.curved=.1
)

#Community
wc <- cluster_walktrap(net2)
modularity(wc)
membership(wc)

require(ape)
#Dendrogram
plot_dendrogram(wc, mode="phylo",  palette = categorical_pal(8))
plot_dendrogram(wc, mode="hclust",  palette = categorical_pal(8))

#Plot3
plot(wc, net2,				#the graph to be plotted
     layout=layout.graphopt,	# the layout method. see the igraph documentation for details
     main='graphopt',	#specifies the title
     edge.curved=.1
)