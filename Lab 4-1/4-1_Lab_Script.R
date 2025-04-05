library(vosonSML)
library(igraph)

# Load your chosen network graph

network_graph <- readRDS("reddit_actor_taylor.rds")

# Run the Page Rank algorithm

rank_yt_actor <- sort(page_rank(network_graph)$vector, decreasing = TRUE)
rank_yt_actor[1:10]