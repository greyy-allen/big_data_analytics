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
  "https://www.reddit.com/r/OliviaRodrigo/comments/16c5pl9/guts_album_discussion_megathread/", 
  "https://www.reddit.com/r/OliviaRodrigo/comments/1c0ua6w/why_do_you_think_sour_did_so_well/",
  "https://www.reddit.com/r/OliviaRodrigo/comments/1cqt3pu/guts_world_tour_megathread_part_3/",
  "https://www.reddit.com/r/OliviaRodrigo/comments/1cqm0jl/sour_is_the_most_streamed_female_album_in_spotify/")


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

saveRDS(rd_actor_graph, file = "reddit_actor_olivia.rds")

write_graph(rd_actor_graph, file = "reddit_actor_olivia.graphml", format = "graphml")


# Load your chosen network graph

network_graph <- readRDS("reddit_actor_olivia.rds")


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

# ---- Topic Modelling ----

library(tidyr)
library(textclean)
library(tm)
library(topicmodels)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidytext)

# Load a dataset 

rd_data <- readRDS("rd_data.rds")

# Remove rows that have 'NA'

rd_data <- rd_data[complete.cases(rd_data), ]


# Clean the text

clean_text <- rd_data$comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |>
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()

# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))


# Perform further pre-processing 

text_corpus <- text_corpus |>
  tm_map(content_transformer(tolower)) |> 
  tm_map(removeWords, stopwords(kind = "SMART")) |> 
  # tm_map(stemDocument) |> # optional
  tm_map(stripWhitespace)


# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]

# Optional: Remove objects and run garbage collection for faster processing

save(dtm, file = "doc_term_matrix.RData")
rm(list = ls(all.names = TRUE))
gc() 
load("doc_term_matrix.RData")

# Create LDA model with k topics

lda_model <- LDA(dtm, k = 6)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

found_topics <- tidy(lda_model, matrix = "beta")
View(found_topics)


# Visualise the top 10 terms per topic

top_terms <- found_topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta)

top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Remember to save your data
save.image(file = "olivia_topic_modelling.RData")


