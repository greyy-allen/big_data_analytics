# !!Remember to set your working directory!!

# Part 1: YouTube User Analysis ----

# Load packages required for this session into library

library(vosonSML)
library(igraph)


# Set up YouTube authentication variables

my_api_key <- ""


# Authenticate to YouTube and collect data

yt_auth <- Authenticate("youtube", apiKey = my_api_key)

video_url <- c("https://www.youtube.com/watch?v=jNQXAC9IVRw")

yt_data <- yt_auth |> Collect(videoIDs = video_url,
                              maxComments = 200,
                              writeToFile = TRUE,
                              verbose = TRUE) # use 'verbose' to show download progress


# Save & export the collected data

saveRDS(yt_data, file = "yt_data.rds") # save as an R object
write.csv(yt_data, file = "yt_data.csv") # export as a CSV


# View the collected YouTube data

View(yt_data)


# Inspect the comment authors

authors <- yt_data$AuthorDisplayName
authors[1:10]

authors[duplicated(authors)]
table(authors[duplicated(authors)])


# Create actor network and graph from the data

yt_actor_network <- yt_data |> Create("actor")
yt_actor_graph <- yt_actor_network |> Graph()

plot(yt_actor_graph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)


# Write graph to file
# Make sure to set your working directory to where you want to save the file
# before you execute the next line

write_graph(yt_actor_graph, file = "YouTubeActor.graphml", format = "graphml")


# Run Page Rank algorithm to find important users

rank_yt_actor <- sort(page_rank(yt_actor_graph)$vector, decreasing=TRUE)
rank_yt_actor[1:5]


# Overwrite the 'name' attribute in your graph with the 'screen name' attribute
# to replace YouTube IDs with more meaningful names,
# then run the Page Rank algorithm again

V(yt_actor_graph)$name <- V(yt_actor_graph)$screen_name

rank_yt_actor <- sort(page_rank(yt_actor_graph)$vector, decreasing = TRUE)
rank_yt_actor[1:5] # <NA> because this is the original video


# Are the top page rank users also the ones who commented the most?

table(authors[duplicated(authors)])



# Part 2: Spotify artist analysis ----

# Load packages required for this session into library

library(spotifyr)
library(ggplot2)
library(ggridges)


# Set up authentication variables

app_id <- "7e9f2297a08e4b66ba790a39a1330998"
app_secret <- "8acb49b84d0d4714865b2c0b8f85405e"
token <- "1"


# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get Spotify data on 'Bruno Mars'

find_my_artist <- search_spotify("Bruno Mars", type = "artist")
View(find_my_artist)


# Retrieve album data of artist

albums <- get_artist_albums("0du5cEVh5yTK9QJze8zA0C", 
                            include_groups = c("album", "single", "appears_on", "compilation"))
View(albums)

songs <- get_album_tracks("4PgleR09JVnm3zY1fW3XBA")
View(songs)


# Retrieve song data

song <- get_track_audio_features("6b8Be6ljOzmkOmFslEb23P")
View(song)


# Get audio features for 'Bruno Mars'

audio_features <- get_artist_audio_features("0du5cEVh5yTK9QJze8zA0C") # artist ID for Bruno Mars
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name), ]


# Plot happiness (valence) scores for each album

ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Happiness in Bruno Mars Albums",
          subtitle = "Based on valence from Spotify's Web API")


# Retrieve information about Bruno Mars' related artists

related_bm <- get_related_artists("0du5cEVh5yTK9QJze8zA0C")
View(related_bm)


# Create a network of artists related to Bruno Mars

edges <- c()

for (artist in related_bm$id){
  related <- get_related_artists(artist)
  artist_name <- get_artist(artist)$name
  for (relatedartist in related$name){
    edges <- append(edges, artist_name)
    edges <- append(edges, relatedartist)
  }
}

edges[1:10]


# Convert network to graph and save as external file

related_artists_graph <- graph(edges)

plot(related_artists_graph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
write_graph(related_artists_graph, file = "RelatedArtists.graphml", format = "graphml")

# Remember to save your data
save.image(file = "2-1_Lab_Data.RData")
