pathProject <- "2022/2022-05-23-song-mood-spotify"

here::i_am(paste(pathProject, "src", "visualize.R", sep = "/"))

set.seed(12345)


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(ggbeeswarm)
library(paletteer)
library(ggtext)
library(dfrtheme)
library(patchwork)


# Helper ----

scale_fill_lake_superior <- function() {
  ggplot2::scale_fill_gradient2(
    low = "#C87D4BFF",
    mid = "grey",
    high = "#324B64FF",
    midpoint = 0.5
  )
}

clean_genre_lab <- function(data) {
  
  if (!any("genre" %in% names(data))) {
    stop("`data` must contain `genre` column")
  }
  
  dataGenreLabClean <- data |> 
    dplyr::mutate(
      genre = stringr::str_to_sentence(genre),
      genre = stringr::str_replace_all(genre, "R&b", "R&B")
    )
  
  return(dataGenreLabClean)
  
}


# Palette ----
 
spotifyPalettePrimary <- paletteer_d("nord::lake_superior")
spotifyPaletteSecondary <-  paletteer_d("nord::silver_mine")


# Plot ----

## Playlist of top songs ----

### Mean valence of top song playlists by country ----

topSongValenceMean <- read_csv(
  here(pathProject, "result", "playlist-top-song-valence-mean.csv")
)

topSongValenceMeanPrep <- topSongValenceMean |> 
  mutate(playlist_top_song = fct_reorder(playlist_top_song, valence_mean))

captionBase <- paste0(
  "Sumber: Spotify; analisis *Katadata*<br>",
  "Grafik: Katadata/Dzulfiqar Fathur Rahman"
)

captionTopSong <- paste0(
  "\\*Terakhir diperbarui pada 13 Mei 2022<br>",
  captionBase
)

plotTopSongValenceMean <- ggplot(
  topSongValenceMeanPrep,
  aes(valence_mean, "y")
) +
  geom_quasirandom(
    aes(fill = valence_mean),
    groupOnX = FALSE,
    size = 4.5,
    pch = 21,
    color = "white",
    alpha = 0.75,
    show.legend = FALSE
  ) +
  geom_richtext(
    data = tibble(
      x = c(0.35, 0.65),
      y = c("y", "y"),
      label = c("&larr; Lebih sedih", "Lebih bahagia &rarr;")
    ),
    mapping = aes(x, y, label = label),
    size = dfr_convert_font_size(),
    nudge_y = 0.45,
    color = c(spotifyPalettePrimary[[3]], spotifyPalettePrimary[[5]]),
    fontface = "bold",
    label.color = NA,
    label.padding = unit(0, "cm")
  ) +
  geom_label(
    data = tibble(
      x = c(0.3881200 - 0.0225, 0.6549600 + 0.015),
      y = c("y", "y"),
      label = c("Indonesia", "Brasil")
    ),
    mapping = aes(x, y, label = label),
    size = dfr_convert_font_size(),
    color = "#757575",
    nudge_y = -0.035,
    label.size = NA,
    label.padding = unit(0, "cm")
  ) +
  scale_x_continuous(
    breaks = seq(0.3, 0.7, 0.1),
    limits = c(0.3, 0.7),
    position = "top"
  ) +
  scale_fill_lake_superior() +
  labs(
    title = paste0(
      "Preferensi pendengar Indonesia terhadap lagu-lagu sedih ",
      "lebih besar"
    ),
    subtitle = paste0(
      "Rata-rata valensi daftar putar Lagu Teratas di Spotify\\* ",
      "menurut negara/daerah"
    ),
    x = NULL,
    y = NULL,
    caption = captionTopSong
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here(pathProject, "result", "playlist-top-song-valence-mean.svg"),
  plot = plotTopSongValenceMean,
  width = 8,
  height = 4.5
)


### Distribution: valence and danceability in selected top song playlists ----

topSongValenceDanceability <- read_csv(
  here(
    pathProject,
    "result",
    "playlist-top-song-idn-mys-wld-valence-danceability.csv"
  )
)

topSongValenceDanceabilityPrep <- topSongValenceDanceability |> 
  mutate(
    playlist_top_song = fct_relevel(
      playlist_top_song, 
      c("Indonesia", "Malaysia", "Global")
    )
  )

plotTopSongSubValenceDist <- ggplot(
  topSongValenceDanceabilityPrep,
  aes(valence, fill = ..x..)
) +
  geom_histogram(
    binwidth = 0.1,
    color = "white",
    show.legend = FALSE
  ) +
  scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(
    breaks = seq(0, 12, 3),
    limits = c(0, 12),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_lake_superior() +
  facet_wrap(~ playlist_top_song) +
  labs(
    subtitle = "Valensi",
    x = "Lebih bahagia &rarr;",
    y = "Jumlah"
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    plot.subtitle = element_markdown(face = "bold"),
    panel.grid.major.x = element_blank(),
    strip.text = element_markdown(face = "plain")
  )

plotTopSongSubDanceabilityDist <- ggplot(
  topSongValenceDanceabilityPrep,
  aes(danceability, fill = ..x..)
) +
  geom_histogram(
    binwidth = 0.075,
    color = "white",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    labels = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 20, 5),
    limits = c(0, 20),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_gradient2(
    low = spotifyPaletteSecondary[[1]],
    mid = "grey",
    high = spotifyPaletteSecondary[[5]],
    midpoint = 0.5
  ) +
  facet_wrap(~ playlist_top_song) +
  labs(
    subtitle = "*Danceability*",
    x = "Lebih cocok untuk berdansa/menari &rarr;",
    y = "Jumlah"
  ) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    plot.subtitle = element_markdown(face = "bold"),
    panel.grid.major.x = element_blank(),
    strip.text = element_blank()
  )

pwTopSongValenceDanceabilityDist <- plotTopSongSubValenceDist +
  plotTopSongSubDanceabilityDist +
  plot_layout(nrow = 2) +
  plot_annotation(
    title = "Pendengar Indonesia dan Malaysia punya preferensi yang mirip",
    subtitle = paste0(
      "Distribusi lagu-lagu dalam daftar putar Lagu Teratas di Spotify\\* ",
      "berdasarkan valensi dan *danceability*<br>menurut negara/daerah"
    ),
    caption = captionTopSong,
    theme = dfr_theme()
  )

ggsave(
  here(
    pathProject,
    "result",
    "playlist-top-song-idn-mys-wld-valence-danceability-distribution.svg"
  ),
  plot = pwTopSongValenceDanceabilityDist,
  width = 8,
  height = 6.5
)


## Playlist of major popular genres ----

### Mean valence of major popular genre playlists ----

genreValenceMean <- read_csv(
  here(pathProject, "result", "playlist-major-genre-valence-mean.csv")
)

genreValenceMeanPrep <- genreValenceMean |>
  clean_genre_lab() |> 
  mutate(genre = fct_reorder(genre, valence_mean))

plotGenreValenceMean <- ggplot(
  genreValenceMeanPrep,
  aes(valence_mean, genre)
) +
  geom_col(
    aes(fill = valence_mean),
    width = 0.6, 
    color = "white",
    show.legend = FALSE
  ) +
  geom_richtext(
    data = tibble(x = 0.75, y = "Punk", label = "Lebih bahagia &rarr;"),
    mapping = aes(x, y, label = label),
    size = dfr_convert_font_size(),
    color = spotifyPalettePrimary[[5]],
    label.color = NA,
    label.padding = unit(0, "cm")
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    labels = c(0, 0.25, 0.5, 0.75, 1),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_fill_lake_superior() +
  labs(
    title = paste0(
      "Dangdut cenderung terdengar lebih positif, sementara jazz lebih sedih"
    ),
    subtitle = paste0(
      "Rata-rata valensi lagu-lagu yang dipilih di Spotify ",
      "menurut genre populer utama"
    ),
    x = NULL,
    y = NULL,
    caption = captionBase
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks.y = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here(pathProject, "result", "playlist-major-genre-valence-mean.svg"),
  plot = plotGenreValenceMean,
  width = 8,
  height = 5
)


### Mean valence of major popular genre playlists per release year ----

genreAnnualValenceMean <- read_csv(
  here(
    pathProject,
    "result",
    "playlist-major-genre-valence-mean-per-annum.csv"
  )
)

genreAnnualValenceMeanPrep <- genreAnnualValenceMean |> clean_genre_lab()

plotGenreAnnualValenceMean <- ggplot(
  genreAnnualValenceMeanPrep,
  aes(track_album_release_year, valence_mean)
) +
  geom_point(
    aes(fill = valence_mean),
    size = 1.75,
    pch = 21,
    color = "white",
    alpha = 0.25,
    show.legend = FALSE
  ) +
  geom_smooth(
    aes(color = ..y..),
    method = "loess",
    se = FALSE,
    lwd = 1.25,
    show.legend = FALSE
  ) +
  geom_text(
    data = tibble(x = 1985, y = 0.6, label = "Garis tren", genre = "Dangdut"),
    mapping = aes(x, y, label = label),
    nudge_x = -20,
    size = dfr_convert_font_size(),
    color = "#757575"
  ) +
  scale_x_continuous(
    breaks = seq(1940, 2020, 20),
    limits = c(1940, 2022),
    labels = c(1940, "'60", "'80", 2000, "'20")
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    labels = c(0, 0.25, 0.5, 0.75, 1),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_lake_superior() +
  scale_color_gradient2(
    low = spotifyPalettePrimary[[3]],
    mid = "grey",
    high = spotifyPalettePrimary[[5]],
    midpoint = 0.5
  ) +
  labs(
    title = paste0(
      "Lagu-lagu terdengar semakin sedih, murung atau marah dari tahun ke tahun"
    ),
    subtitle = paste0(
      "Rata-rata valensi lagu-lagu yang dipilih di Spotify ",
      "per tahun rilis album menurut genre populer utama"
    ),
    x = NULL,
    y = "Lebih sedih &rarr;",
    caption = captionBase
  ) +
  facet_wrap(~ genre, scales = "free_x", nrow = 2) +
  dfr_theme() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(
    pathProject,
    "result",
    "playlist-major-genre-valence-mean-per-annum.svg"
  ),
  plot = plotGenreAnnualValenceMean,
  width = 10,
  height = 5.75
)