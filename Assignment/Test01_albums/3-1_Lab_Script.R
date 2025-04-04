# !!Remember to set your working directory!!

library(vosonSML)
library(igraph)

# Load your chosen network graph

network_graph <- readRDS("RedditActor.rds")


# Inspect the graph object

length(V(network_graph))
V(network_graph)$name[1:20]


# Find all maximum components that are weakly connected

comps <- components(network_graph, mode = c("weak"))

comps$no
comps$csize
head(comps$membership, n = 30) 


# Get sub-graph with most members

largest_comp <- which.max(comps$csize)

comp_subgraph <- network_graph |> 
  induced_subgraph(vids = which(comps$membership == largest_comp))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(comp_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(comp_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(comp_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(comp_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(comp_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(comp_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(comp_subgraph, directed = FALSE), decreasing = TRUE)[1:20]


# Remember to save your data
save.image(file = "3-1_Lab_Data.RData")
