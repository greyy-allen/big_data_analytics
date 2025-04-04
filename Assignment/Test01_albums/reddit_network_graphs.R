library(vosonSML)
library(igraph)
library(dplyr)
library(textclean)
library(tidytext)
library(tidyr)
library(tm)


# Load Reddit data
rd_data <- readRDS("rd_data.rds")

# Remove rows that have 'NA'
rd_data <- rd_data[complete.cases(rd_data), ]
View(rd_data)


# -----------------------------------Reddit Actor Network ------------------------------------------------------

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
saveRDS(rd_actor_graph, file = "reddit_actor_taylor.rds")
write_graph(rd_actor_graph, file = "reddit_actor_taylor.rds", format = "graphml")


# -----------------------------------Reddit Semantic Graph ---------------------------------------------------

# Clean the text with help from the textclean package
rd_clean_text <- rd_data$comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> # ` vs '
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()  # |> 
# replace_emoji() |> # optional
# replace_emoticon() # optional

rd_clean_df <- data.frame(rd_clean_text)

#  Tokenisation: split into bigrams (two neighbouring words)
rd_bigrams <- rd_clean_df |> unnest_tokens(output = bigram,
                                           input = rd_clean_text,
                                           token = "ngrams",
                                           n = 2)

# Count and split bigrams
rd_bigrams_table <- rd_bigrams |> 
  count(bigram, sort = TRUE) |> 
  separate(bigram, c("left", "right"))

# Remove stop words
rd_bigrams_nostops <- rd_bigrams_table |> 
  anti_join(stop_words, join_by(left == word)) |> 
  anti_join(stop_words, join_by(right == word))

# Remove rows that have 'NA'
rd_bigrams_nostops <- rd_bigrams_nostops[complete.cases(rd_bigrams_nostops), ]

# If you have lots of bigrams, keep only those that occur at least X times (here X is 2)
rd_bigrams_nostops <- rd_bigrams_nostops |> filter(n >= 2)

# Convert to a data frame and export as CSV for visualisation later
rd_words <- data.frame(words = c(rd_bigrams_nostops$left, rd_bigrams_nostops$right))
write.csv(rd_words, file = "rd_words.csv") 

# Create graph
rd_bigram_graph <- graph_from_data_frame(rd_bigrams_nostops, directed = FALSE)

# Remove loops and multiple edges
rd_bigram_graph <- simplify(rd_bigram_graph)

# Save and write graph to file
saveRDS(rd_bigram_graph, file = "reddit_semantic_taylor.rds")
write_graph(rd_bigram_graph, file = "reddit_semantic_taylor.graphml", format = "graphml")

# Remember to save your data
save.image(file = "reddit_graphs_taylor.RData")
