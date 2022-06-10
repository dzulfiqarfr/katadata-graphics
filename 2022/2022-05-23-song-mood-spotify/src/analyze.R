pathProject <- "2022/2022-05-23-song-mood-spotify"

here::i_am(paste(pathProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(lubridate)
library(spotifyr)


# Data ----

## Playlists of weekly top songs by country/region ----

pathTopSong <- here(
  pathProject,
  "data",
  "spotify-top-song-playlist-2022-05-20-raw.csv"
)

if (!file.exists(pathTopSong)) {
  
  tokenSpotify <- get_spotify_access_token()
  
  # I have added all the available Top Songs playlists on Spotify to my
  # account. Aside from these playlists, I had no other playlists at the time
  # of writing. Thus, I won't need to filter the response of the following
  # request
  
  argsTopSongGetUserPlaylist <- list(offset = list(0, 50))
  
  topSongPlaylistRaw <- pmap(
    argsTopSongGetUserPlaylist,
    get_user_playlists,
    user_id = "dzulfiqarfathur",
    limit = 50
  ) |> 
    bind_rows() 
  
  topSongPlaylistID <- topSongPlaylistRaw |> 
    select(name, id) |> 
    arrange(name) |> 
    deframe()
  
  topSongTrackRaw <- map(topSongPlaylistID, get_playlist_tracks)
  
  topSongTrackWithArtist <- topSongTrackRaw |> 
    map(
      select,
      added_at,
      track.artists,
      track.id,
      track.name
    ) |> 
    map(
      hoist,
      track.artists,
      artist.id = "id",
      artist.name = "name"
    ) |> 
    map(select, -track.artists)
  
  topSongTrackID <- topSongTrackWithArtist |> map(pull, track.id)
  
  topSongFeatureRaw <- map(topSongTrackID, get_track_audio_features)
  
  topSongFeatureSub <- topSongFeatureRaw |> 
    map(select, -c(type, uri, track_href, analysis_url)) |> 
    map(rename, track.id = id)
  
  argsTopSongBindRows <- list(
    track = topSongTrackWithArtist, 
    audio_feature = topSongFeatureSub
  )
  
  topSongTrackAndFeature <- argsTopSongBindRows |> 
    map(bind_rows, .id = "playlist")
  
  topSongTrackWithFeature <- left_join(
    topSongTrackAndFeature[["track"]],
    topSongTrackAndFeature[["audio_feature"]],
    by = c("playlist", "track.id")
  ) |> 
    unnest_longer(col = c(artist.id, artist.name)) |> 
    rename_with(function(.x) str_replace_all(.x, "\\.", "_"))

  topSongTrackWithFeature |> write_csv(pathTopSong)
  
} else {
  
  topSongTrackWithFeature <- read_csv(pathTopSong)
  
}

# Simplify playlist labels and drop duplicates *within* playlists
topSongTrackWithFeatureClean <- topSongTrackWithFeature |> 
  separate(
    playlist,
    into = c(NA, "playlist_top_song"),
    sep = "\\s-\\s"
  ) |> 
  group_by(playlist_top_song) |>
  distinct(track_id, .keep_all = TRUE) |> 
  ungroup()

topSongValenceMean <- topSongTrackWithFeatureClean |> 
  group_by(playlist_top_song) |> 
  summarize(valence_mean = mean(valence, na.rm = TRUE)) |> 
  ungroup()

topSongValenceMean |> 
  write_csv(
    here(
      pathProject,
      "result",
      "playlist-top-song-valence-mean.csv"
    )
  )

# Compare playlists of top songs in Indonesia and Malaysia on danceability
# and valence since both countries are expected to have a somewhat similar 
# preference. Then compare the two with the world
topSongTrackWithFeatureSub <- topSongTrackWithFeatureClean |> 
  filter(playlist_top_song %in% c("Indonesia", "Malaysia", "Global")) |> 
  select(playlist_top_song, track_id, valence, danceability)

topSongTrackWithFeatureSub |> 
  write_csv(
    here(
      pathProject,
      "result",
      "playlist-top-song-idn-mys-wld-valence-danceability.csv"
    )
  )

topSongValenceDist <- topSongTrackWithFeatureSub |> 
  group_by(playlist_top_song) |> 
  count(cut_width(valence, 0.1)) |> 
  ungroup() |> 
  rename(valence_count = n, bins = 2)

topSongValenceDist |> 
  write_csv(
    here(
      pathProject,
      "result",
      "playlist-top-song-idn-mys-wld-valence-distribution.csv"
    )
  )

topSongDanceabilityDist <- topSongTrackWithFeatureSub |> 
  group_by(playlist_top_song) |> 
  count(cut_width(danceability, 0.075)) |> 
  ungroup() |> 
  rename(danceability_count = n,  bins = 2)

topSongDanceabilityDist |> 
  write_csv(
    here(
      pathProject,
      "result",
      "playlist-top-song-idn-mys-wld-danceability-distribution.csv"
    )
  )


## Playlists of major popular genres ----

pathGenre <- here(
  pathProject,
  "data",
  "spotify-major-popular-genre-playlist-2022-05-20-raw.csv"
)

if (!file.exists(pathGenre)) {
  
  tokenSpotify <- get_spotify_access_token()
  
  # Reference for the major popular genres: 
  # https://en.wikipedia.org/wiki/Music_genre
  # Danddut is added to make it more relevant for the context of this analysis
  genreMajor <- c(
    "electronic", 
    "hip hop", 
    "jazz", 
    "pop", 
    "punk",
    "reggae",
    "rock",
    "metal", 
    "r&b", 
    "dangdut"
  )
  
  # Use each genre as its name to create a named vector. This helps create
  # a `genre` column when binding all tibbles of the genres by rows later
  genreMajor <- set_names(genreMajor, genreMajor)
  
  genrePlaylistRaw <- map(
    genreMajor,
    search_spotify,
    type = "playlist",
    market = "ID",
    limit = 50
  )
  
  genrePlaylistSub <- genrePlaylistRaw |> 
    map(
      select,
      description,
      id,
      name,
      owner.display_name,
      owner.id,
      tracks.total
    ) |> 
    bind_rows(.id = "genre")
  
  genrePlaylistID <- genrePlaylistSub |> 
    select(genre, id) |> 
    deframe()
  
  genreTrackRaw <- genrePlaylistID |> map(get_playlist_tracks)
  
  genreTrackMerged <- genreTrackRaw |> bind_rows(.id = "genre") 
  
  # Drop duplicates *within* genres, while allowing duplicates *across* genres
  genreTrackDupeClean <- genreTrackMerged |> 
    group_by(genre) |> 
    distinct(track.id, .keep_all = TRUE) |> 
    ungroup()
  
  genreTrackWithArtist <- genreTrackDupeClean |> 
    select(
      genre,
      track.artists,
      track.id,
      track.name,
      track.album.release_date
    ) |> 
    hoist( 
      track.artists,
      artist.id = "id",
      artist.name = "name"
    ) |> 
    select(-track.artists)
  
  # Create a tibble containing a sequence of integers from 1 to the number of
  # observations of the track data based on major genre playlists
  genreObservation <- nrow(genreTrackWithArtist)
  
  indexGroup <- tibble(
    index = seq(1, genreObservation, 100),
    index_group = seq(1, genreObservation, 100)
  )
  
  # Use `indexGroup` to split the tibble of track IDs into a list of vectors
  # with length less than or equal to 100. This helps overcome the limit of 
  # requesting audio features using `spotifyr::get_audio_features()` later. 
  # The function caps the number of songs to request at 100 at a time
  genreTrackID <- genreTrackWithArtist |> 
    select(track.id) |> 
    mutate(index = row_number()) |> 
    left_join(indexGroup, by = "index") |> 
    fill(index_group) |> 
    split(~ index_group) |> 
    map(pull, track.id)
  
  genreFeatureRaw <- genreTrackID |> 
    map(get_track_audio_features) |> 
    bind_rows()
  
  genreFeatureSub <- genreFeatureRaw |> 
    select(-c(type, uri, track_href, analysis_url)) |> 
    rename(track.id = id)
  
  # Drop duplicates within genres that occur after joins
  genreTrackWithFeature <- genreTrackWithArtist |> 
    left_join(genreFeatureSub, by = "track.id") |> 
    group_by(genre) |> 
    distinct(track.id, .keep_all = TRUE) |> 
    ungroup() |> 
    unnest_longer(col = c(artist.id, artist.name)) |> 
    rename_with(function(.x) str_replace_all(.x, "\\.", "_"))
  
  genreTrackWithFeature |> write_csv(pathGenre)
  
} else {
  
  genreTrackWithFeature <- read_csv(pathGenre)
  
}

# Drop tracks with missing `valence`, a duration longer than 10 minutes and
# similar characteristics to speech-like recordings, such as talk show
# audio book and poetry. Then, drop duplicates *within* genres
genreTrackWithFeatureClean <- genreTrackWithFeature |>
  filter(
    !is.na(valence),
    duration_ms <= 600000,
    speechiness <= 0.66
  ) |> 
  group_by(genre) |> 
  distinct(track_id, .keep_all = TRUE) |> 
  ungroup()

genreValenceMean <- genreTrackWithFeatureClean |> 
  group_by(genre) |> 
  summarize(valence_mean = mean(valence, na.rm = TRUE)) |> 
  ungroup()

genreValenceMean |> 
  write_csv(
    here(
      pathProject,
      "result",
      "playlist-major-genre-valence-mean.csv"
    )
  )

# Get the album release year of each track so we can compute the mean valence 
# of songs for each year
genreTrackWithFeatureDateClean <- genreTrackWithFeatureClean |> 
  mutate(
    track_album_release_date = as.Date(track_album_release_date),
    track_album_release_year = year(track_album_release_date)
  )

genreAnnualValenceMean <- genreTrackWithFeatureDateClean |> 
  group_by(genre, track_album_release_year) |> 
  summarize(valence_mean = mean(valence, na.rm = TRUE)) |> 
  ungroup()

genreAnnualValenceMean |> 
  write_csv(
    here(
      pathProject,
      "result",
      "playlist-major-genre-valence-mean-per-annum.csv"
    )
  )
