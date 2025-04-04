# ---- Load Packages ----

# Load Packages for General use

library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)

# Load Packages for Spotify

library(spotifyr)
library(ggridges)

# Load Package for Sentiment Analysis

library(syuzhet)

# Load Package for Decision Tree

library(C50)
library(caret)
library(e1071)

# Load Package for Topic Modelling

library(tidyr)
library(textclean)
library(tm)
library(topicmodels)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidytext)

# ---- Data Selection & Exploration ----

# ---- 2 Data Collection ----

# Collecting Data from Reddit about Taylor Swift
thread_urls <- c(
  "https://www.reddit.com/r/TaylorSwift/comments/1eqp1n0/the_eras_tour_megathread_london_england_august/", 
  "https://www.reddit.com/r/TaylorSwift/comments/yy00i0/ticketmaster_has_cancelled_the_general_sale/",
  "https://www.reddit.com/r/TaylorSwift/comments/1emljc8/eras_tour_shows_in_vienna_cancelled/",
  "https://www.reddit.com/r/TaylorSwift/comments/yyp0ib/taylors_response_on_the_ticket_crisis/")


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

# Remove rows that have 'NA'

rd_data <- rd_data[complete.cases(rd_data), ]
View(rd_data)

# ---- 3 Top 5 Actor Networks ----

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

write_graph(rd_actor_graph, file = "taylor_actor_top_5.graphml", format = "graphml")

# Page Rank scores

pagerank_scores <- sort(page_rank(rd_actor_graph)$vector, decreasing=TRUE)
pagerank_scores[1:5]

# ---- 4 Unique Actors ----

# Number of unique actors

num_unique_actors <- length(V(rd_actor_graph))
print(num_unique_actors)

# Save data for actor networks

save.image(file = "actor_networks_image.RData")


# ---- 5 Spotify ----

# Spotify credentials

app_id <- "7e9f2297a08e4b66ba790a39a1330998"
app_secret <- "8acb49b84d0d4714865b2c0b8f85405e"
token <- "1"

# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

# Get Spotify data for 'Taylor Swift'

find_my_artist <- search_spotify("Taylor Swift", type = "artist")
View(find_my_artist)

# Retrieve album data of artist

albums <- get_artist_albums("06HL4z0CvFAxyc27GXpf02", 
                            include_groups = c("album", "single", "appears_on", "compilation"))
View(albums)

# Initialize variable to collect songs

all_tracks <- data.frame()

# Looping through each albums to get track 

for (album_id in albums$id) {
  album_tracks <- get_album_tracks(album_id)
  all_tracks <- bind_rows(all_tracks, album_tracks)
}

View(all_tracks)

# Unnest the `artists` column

collaborators <- all_tracks %>%
  select(id, artists) %>%   
  unnest(artists, names_sep = "_")       

View(collaborators)

# Keep Taylor Swift and remove others only from collaborators list
collaborators_cleaned <- collaborators %>%
  group_by(id) %>%
  summarize(collaborators_list = list(artists_name), .groups = 'drop')

View(collaborators_cleaned)

# Create edges for the network
taylor_edges <- collaborators_cleaned %>%
  mutate(taylor_swift_present = "Taylor Swift" %in% unlist(collaborators_list)) %>%
  filter(taylor_swift_present) %>%
  unnest(collaborators_list) %>%
  group_by(id) %>%
  summarise(from = list(collaborators_list), .groups = 'drop') %>%
  unnest(from) %>%
  expand_grid(to = from) %>%
  filter(from != to)

# Create the igraph object
graph_collaborators <- graph_from_data_frame(taylor_edges, directed = FALSE)

# Save the graph to GraphML
write_graph(graph_collaborators, file = "artist_collaborations.graphml", format = "graphml")


# Get track features for 'Taylor Swift'

taylor_features <- get_artist_audio_features("Taylor Swift")
View(taylor_features)

data.frame(colnames(taylor_features))

taylor_features_subset <- taylor_features[ , 9:20]
View(taylor_features_subset)

# Plot valence scores for each track

ggplot(taylor_features, aes(x = valence, y = track_name)) +
  geom_density_ridges(scale = 2) +
  theme_ridges() +
  ggtitle("Happiness in Taylor Swift Tracks",
          subtitle = "Based on valence from Spotify's Web API")

# Plot valence scores for each track

ggplot(taylor_features, aes(x = danceability, y = track_name)) +
  geom_density_ridges(scale = 2) +
  theme_ridges() +
  ggtitle("Danceability in Taylor Swift Tracks",
          subtitle = "Based on danceability from Spotify's Web API")


# Retrieve information about Taylor Swift' related artists

related_bm <- get_related_artists("06HL4z0CvFAxyc27GXpf02")
View(related_bm)

# Create a new variable to store all data about the related artists
related_all <- related_bm

for (artist in related_bm$id){
  related <- get_related_artists(artist)
  related_all <- rbind(related_all, related)
}

View(related_all)

# Remove duplicate rows
sp_data <- unique(related_all)
View(sp_data)

# Perform some processing on the data frame and then export as a CSV
sp_data$num_genres <- lengths(sp_data$genres) # add column: number of genres

sp_data <- sp_data[ , -4] |>      # remove list-column "images"
  unnest_wider(genres, names_sep = "_")   # split list-column "genres"

saveRDS(sp_data, file = "sp_data.rds")
write.csv(sp_data, file = "sp_data.csv")


# Taylor Swift spotify image

save.image(file = "spotify_taylor.RData")

# ---- Text Pre-processing ----

# ---- 6 Term-Document Matrices ----

# Clean the text with help from the textclean package (processing emoji as optional)

clean_text <- rd_data$comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> # ` vs '
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation() # |> 

# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))


# Perform further pre-processing 

text_corpus <- text_corpus |>
  tm_map(content_transformer(tolower)) |> 
  tm_map(removeWords, stopwords(kind = "SMART")) |> 
  tm_map(stemDocument) |> 
  tm_map(stripWhitespace)

# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)


# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

feq_10 <- head(freq, n = 10)

# Create a data frame from the top 10 frequent words

feq_10_df <- data.frame(
  term = names(feq_10),   
  frequency = feq_10      
)

# Plot the top 10 frequent words

ggplot(feq_10_df, aes(x = reorder(term, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Most Frequent Terms",
       x = "Terms",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 7 Semantic Networks ----

# Intialize dataframe for clean_text
  

clean_df <- data.frame(clean_text)


#  Tokenisation: split into bigrams (two neighbouring words)

rd_bigrams <- clean_df |> unnest_tokens(output = bigram,
                                        input = clean_text,
                                        token = "ngrams",
                                        n = 2)
View(rd_bigrams)

rd_bigrams_table <- rd_bigrams |> 
  count(bigram, sort = TRUE) |> 
  separate(bigram, c("left", "right"))

View(rd_bigrams_table)

rd_bigrams_nostops <- rd_bigrams_table |> 
  anti_join(stop_words, join_by(left == word)) |> 
  anti_join(stop_words, join_by(right == word)) # different to above because now table

View(rd_bigrams_nostops)


# Remove rows that have 'NA'

rd_bigrams_nostops <- rd_bigrams_nostops[complete.cases(rd_bigrams_nostops), ]
View(rd_bigrams_nostops)


# If you have lots of bigrams, keep only those that occur at least X times (here X is 2)

rd_bigrams_nostops <- rd_bigrams_nostops |> filter(n >= 2)
View(rd_bigrams_nostops)


# Create a semantic network graph

rd_bigram_graph <- graph_from_data_frame(rd_bigrams_nostops, directed = FALSE)
rd_bigram_graph

vcount(rd_bigram_graph)
ecount(rd_bigram_graph)
rd_bigram_graph <- simplify(rd_bigram_graph)
vcount(rd_bigram_graph)
ecount(rd_bigram_graph)

plot(rd_bigram_graph, vertex.size = 4, edge.arrow.size = 0.5)

write_graph(rd_bigram_graph, file = "taylor_bigram.graphml", format = "graphml")

rank_rd_bigram <- sort(page_rank(rd_bigram_graph)$vector, decreasing=TRUE)
rank_rd_bigram[1:10]

# Save image for Text Pre processing

save.image(file = "preprocess_text_taylor.RData")

# ---- Social Network Analysis ----

# ---- 8 Centrality ----

# Remove rows of rd_data that have 'NA'

rd_data <- rd_data[complete.cases(rd_data), ]
View(rd_data)

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

write_graph(rd_actor_graph, file = "reddit_actor_taylor.graphml", format = "graphml")

# Load your chosen network graph

network_graph <- readRDS("reddit_actor_taylor.rds")


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


# ---- 9 Edge Betweenness ----

# Transform into an undirected graph

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

# Saving Data for Community Analysis

save.image(file = "social_network_analysis_taylor.RData")

# ---- 10 Sentiment Analysis ----

# Assign sentiment scores to comments

sentiment_scores <- get_sentiment(clean_text, method = "afinn") |> sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)


# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)

# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Comments")


# Assign emotion scores to comments

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)

# Calculate proportion of emotions across all comments

emo_sums <- emo_scores_df[,2:9] |> 
  sign() |> 
  colSums() |> 
  sort(decreasing = TRUE) |> 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Comments")

# Remember to save your data
save.image(file = "sentiment_analysis_taylor.RData")

# ---- 11 Decision Tree ----

# Get top 50 songs and their audio features

top50_features <- get_playlist_audio_features("spotify", "37i9dQZF1DXcBWIGoYBM5M")
daily_mix <- get_playlist_audio_features("spotify", "37i9dQZF1E37jeepbwb32r")

View(top50_features)
View(daily_mix)

data.frame(colnames(top50_features))
data.frame(colnames(daily_mix))

top50_features_subset <- top50_features[ , 6:17]
View(top50_features_subset)

daily_mix_features_subset <- daily_mix[ , 6:17]
View(daily_mix_features_subset)

top50_features_subset <- top50_features_subset |> rename(track_id = track.id)
daily_mix_features_subset <- daily_mix_features_subset |> rename(track_id = track.id)

# Add the 'isTaylor' column (class variable) to each data frame
# to indicate which songs are by Taylor and which are not

top50_features_subset["isTaylor"] <- 0
daily_mix_features_subset["isTaylor"] <- 0

taylor_features_subset["isTaylor"] <- 1

# Remove any songs by Taylor that appear in the top 50
# and combine the two data frames into one dataset

top50_features_notaylor <- anti_join(top50_features_subset,
                                    taylor_features_subset,
                                    by = "track_id")

daily_mix_features_notaylor <- anti_join(daily_mix_features_subset,
                                    taylor_features_subset,
                                    by = "track_id")

comb_data <- rbind(top50_features_notaylor, taylor_features_subset)
comb_data_mix <- rbind(daily_mix_features_notaylor, taylor_features_subset)

# Format the dataset so that we can give it as input to a model:
# change the 'isTaylor' column into a factor
# and remove the 'track_id' column

comb_data$isTaylor <- factor(comb_data$isTaylor)
comb_data_mix$isTaylor <- factor(comb_data_mix$isTaylor)

comb_data <- select(comb_data, -track_id)
comb_data_mix <- select(comb_data_mix, -track_id)

# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]
comb_data_mix <- comb_data_mix[sample(1:nrow(comb_data_mix)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]

split_point_mix <- as.integer(nrow(comb_data_mix)*0.8)
training_set_mix <- comb_data_mix[1:split_point_mix, ]
testing_set_mix <- comb_data_mix[(split_point_mix + 1):nrow(comb_data_mix), ]


# Train the decision tree model

dt_model <- train(isTaylor~ ., data = training_set, method = "C5.0")

dt_model_mix <- train(isTaylor~ ., data = training_set_mix, method = "C5.0")


# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to testing set size

predicted_label <- predict(dt_model, testing_set[prediction_row, ]) # predict the label for this row
predicted_label <- as.numeric(levels(predicted_label))[predicted_label] # transform factor into numeric value

if (predicted_label == testing_set[prediction_row, 5]){
  print(paste0("Prediction is: ", predicted_label, ". Correct!"))
} else {
  paste0("Prediction is: ", predicted_label, ". Wrong.")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isTaylor)


# Daily Mix Test

prediction_row_mix <- 1 # MUST be smaller than or equal to testing set size

predicted_label_mix <- predict(dt_model, testing_set_mix[prediction_row_mix, ]) # predict the label for this row
predicted_label_mix <- as.numeric(levels(predicted_label_mix))[predicted_label_mix] # transform factor into numeric value

if (predicted_label_mix == testing_set_mix[prediction_row_mix, 10]){
  print(paste0("Prediction is: ", predicted_label_mix, ". Correct!"))
} else {
  paste0("Prediction is: ", predicted_label_mix, ". Wrong.")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model_mix, reference = testing_set_mix$isTaylor)





# Remember to save your data
save.image(file = "decision_tree_taylor.RData")


# ---- 12 Topic Modelling ----

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

install.packages("tidytext")
library(tidytext)

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
save.image(file = "taylor_topic_modelling.RData")
