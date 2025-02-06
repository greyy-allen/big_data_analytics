# !!Remember to set your working directory!!

# Sentiment & Emotion Analysis --------------------------------------------

# Load packages required for this session into library

library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(syuzhet)
library(ggplot2)


# Load a dataset you want to work with (e.g., "rd_data" or "yt_data")

rd_data <- readRDS("rd_data.rds")


# Clean the text

clean_text <- rd_data$comment |> # change 'comment' to 'Comment' for YouTube
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |>
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()


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



# Decision Tree -----------------------------------------------------------

library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)


# Set up Spotify authentication variables

app_id <- "XXX"
app_secret <- "XXX"
token <- "1"


# Authenticate to Spotify using the spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get songs from Drake and their audio features

drake_features <- get_artist_audio_features("Drake")
View(drake_features)

data.frame(colnames(drake_features))

drake_features_subset <- drake_features[ , 9:20]
View(drake_features_subset)


# Get top 50 songs and their audio features

top50_features <- get_playlist_audio_features("spotify", "37i9dQZF1DXcBWIGoYBM5M")
View(top50_features)

data.frame(colnames(top50_features))

top50_features_subset <- top50_features[ , 6:17]
View(top50_features_subset)

top50_features_subset <- top50_features_subset |> rename(track_id = track.id)


# Add the 'isDrake' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top50_features_subset["isDrake"] <- 0
drake_features_subset["isDrake"] <- 1


# Remove any songs by Drake that appear in the top 50
# and combine the two data frames into one dataset

top50_features_nodrake <- anti_join(top50_features_subset,
                                     drake_features_subset,
                                     by = "track_id")
comb_data <- rbind(top50_features_nodrake, drake_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isDrake' column into a factor
# and remove the 'track_id' column

comb_data$isDrake <- factor(comb_data$isDrake)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model

dt_model <- train(isDrake~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to testing set size

predicted_label <- predict(dt_model, testing_set[prediction_row, ]) # predict the label for this row
predicted_label <- as.numeric(levels(predicted_label))[predicted_label] # transform factor into numeric value

if (predicted_label == testing_set[prediction_row, 12]){
  print(paste0("Prediction is: ", predicted_label, ". Correct!"))
} else {
  paste0("Prediction is: ", predicted_label, ". Wrong.")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isDrake)

# Remember to save your data
save.image(file = "5-2_Lab_Data.RData")
