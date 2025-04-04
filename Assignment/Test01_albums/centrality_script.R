library(vosonSML)
library(igraph)

# Load Created reddit_actor_taylor 
network_graph <- readRDS('reddit_actor_taylor.rds')


# Inspect the graph object

length(V(network_graph))
V(network_graph)$name[1:20]


# Find all maximum components that are weakly connected

comps <- components(network_graph, mode = c("weak"))

comps$no
comps$csize
head(comps$membership, n = 30) 

# Find number of unique actors

num_unique_actors <- sum(comps$csize)


# Get sub-graph with most members

largest_comp <- which.max(comps$csize)

comp_subgraph <- network_graph |> 
  induced_subgraph(vids = which(comps$membership == largest_comp))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(comp_subgraph, mode = "in"), decreasing = TRUE)[1:5]
sort(degree(comp_subgraph, mode = "out"), decreasing = TRUE)[1:5]
sort(degree(comp_subgraph, mode = "total"), decreasing = TRUE)[1:5]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(comp_subgraph, mode = "in"), decreasing = TRUE)[1:5]
sort(closeness(comp_subgraph, mode = "out"), decreasing = TRUE)[1:5]
sort(closeness(comp_subgraph, mode = "total"), decreasing = TRUE)[1:5]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(comp_subgraph, directed = FALSE), decreasing = TRUE)[1:5]


# Remember to save your data
save.image(file = "centrality_script.RData")
