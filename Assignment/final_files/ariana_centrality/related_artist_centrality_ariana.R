# ---- Load Packages ----

library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)

# ---- Data Selection & Exploration ----

# Collecting Data from Reddit about Ariana Grande
thread_urls <- c(
  "https://www.reddit.com/r/ArianaGrande/comments/1accru1/latest_eternal_sunshine_cover/", 
  "https://www.reddit.com/r/ArianaGrande/comments/198z6xq/multiple_album_covers_to_come/",
  "https://www.reddit.com/r/ArianaGrande/comments/malt9p/unpopular_opinion_i_think_positions_is_her_best/",
  "https://www.reddit.com/r/ArianaGrande/comments/malt9p/unpopular_opinion_i_think_positions_is_her_best/",
  "https://www.reddit.com/r/ArianaGrande/comments/snjd8x/thank_u_next_came_out_3_years_ago_whats_everyones/",
  "https://www.reddit.com/r/ArianaGrande/comments/k49vy4/whats_your_favorite_song_off_of_the_sweetener/")


# Collect threads with their comments sorted by best comments first
rd_data <- Authenticate("reddit") |>  
  Collect(threadUrls = thread_urls,
          sort = "best", 
          waitTime = c(6, 8),
          writeToFile = TRUE, 
          verbose = TRUE)


# Save & export the collected data from Reddit
saveRDS(rd_data, file = "rd_data.rds") 
write.csv(rd_data, file = "rd_data.csv")


# View the collected Reddit data

View(rd_data)


# Load Reddit data

rd_data <- readRDS("rd_data.rds")

# Remove rows that have 'NA'

rd_data <- rd_data[complete.cases(rd_data), ]
View(rd_data)

# Reddit Actor Graph ------------------------------------------------------

# Create actor network 

rd_actor_network <- rd_data |> 
  Create("actor") |> 
  AddText(rd_data,
          verbose = TRUE) 

# Create graph from the network

rd_actor_graph <- rd_actor_network |> Graph()
rd_actor_graph
V(rd_actor_graph)$name <- V(rd_actor_graph)$user
rd_actor_graph

# Save and write graph to file

saveRDS(rd_actor_graph, file = "reddit_actor_ariana.rds")

write_graph(rd_actor_graph, file = "reddit_actor_ariana.graphml", format = "graphml")


# Load your chosen network graph

network_graph <- readRDS("reddit_actor_ariana.rds")


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


# Display top 10 nodes from the sub-graph ordered by degree centrality

sort(degree(comp_subgraph, mode = "in"), decreasing = TRUE)[1:10]
sort(degree(comp_subgraph, mode = "out"), decreasing = TRUE)[1:10]
sort(degree(comp_subgraph, mode = "total"), decreasing = TRUE)[1:10]


# Display top 10 nodes from the sub-graph ordered by closeness centrality

sort(closeness(comp_subgraph, mode = "in"), decreasing = TRUE)[1:10]
sort(closeness(comp_subgraph, mode = "out"), decreasing = TRUE)[1:10]
sort(closeness(comp_subgraph, mode = "total"), decreasing = TRUE)[1:10]


# Display top 10 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(comp_subgraph, directed = FALSE), decreasing = TRUE)[1:10]

# Transform netowork into an undirected graph

undir_network_graph <- as.undirected(network_graph, mode = "collapse")


# Run Louvain algorithm

louvain_comm <- cluster_louvain(undir_network_graph)


# See sizes of communities

sizes(louvain_comm)


# Visualise the Louvain communities

plot(louvain_comm, 
     undir_network_graph, 
     vertex.label = V(undir_network_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Run Girvan-Newman (edge-betweenness) algorithm

eb_comm <- cluster_edge_betweenness(undir_network_graph)


# See sizes of communities

sizes(eb_comm)

# Visualise the edge-betweenness communities

plot(eb_comm, 
     undir_network_graph, 
     vertex.label = V(undir_network_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

# Visualise the edge-betweenness hierarchy

is_hierarchical(eb_comm)
as.dendrogram(eb_comm)
plot_dendrogram(eb_comm)

plot_dendrogram(eb_comm, mode = "dendrogram", xlim=c(400,450))




