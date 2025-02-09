# !!Remember to set your working directory!!

# Load packages required for this session into library

library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)


# Part 1: Reddit Text Analysis ----

# Specify the threads from which to collect data

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
          verbose = TRUE) # use 'verbose' to show download progress


# Save & export the collected data

saveRDS(rd_data, file = "rd_data.rds") # save as an R object
write.csv(rd_data, file = "rd_data.csv") # export as a CSV


# View the collected Reddit data

View(rd_data)


# Remove rows that have 'NA'

rd_data <- rd_data[complete.cases(rd_data), ]
View(rd_data)


# Clean the text with help from the textclean package

clean_text <- rd_data$comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> # ` vs '
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation() |> 
  replace_emoji() |> # optional
  replace_emoticon() # optional
# order matters!

clean_text[1:10]


# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content


# Perform further pre-processing 

text_corpus <- text_corpus |>
  tm_map(content_transformer(tolower)) |> 
  tm_map(removeWords, stopwords(kind = "SMART")) |> 
  # tm_map(stemDocument) |> # optional
  tm_map(stripWhitespace)

text_corpus[[1]]$content
text_corpus[[5]]$content


# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)


# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)


# Plot word frequency

word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

ggplot(subset(word_frequ_df, freq > 2), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

ggplot(subset(word_frequ_df, freq > freq[12]), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")


# Part 2: Reddit Word Co-Occurrence ----

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
rd_bigram_graph <- simplify(rd_bigram_graph) # remove loops and multiple edges
vcount(rd_bigram_graph)
ecount(rd_bigram_graph)

plot(rd_bigram_graph, vertex.size = 4, edge.arrow.size = 0.5)

write_graph(rd_bigram_graph, file = "RedditBigram.graphml", format = "graphml")

rank_rd_bigram <- sort(page_rank(rd_bigram_graph)$vector, decreasing=TRUE)
rank_rd_bigram[1:5]

# Remember to save your data
save.image(file = "2-2_Lab_Data.RData")
