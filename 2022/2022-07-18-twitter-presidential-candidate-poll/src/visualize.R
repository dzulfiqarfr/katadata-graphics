pathProject <- "2022/2022-07-18-twitter-presidential-candidate-poll"

here::i_am(paste(pathProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(dfrtheme)
library(scales)
library(paletteer)


# Plot ----

palette <- paletteer_d("ggthemes::manyeys")


## Poll vote share, average tweet per day and average reaction per tweet ----

modPollTweetResult <- read_csv(
  here(pathProject, "result", "poll-tweet-model-result.csv")
)

annoTextPollTweet <- modPollTweetResult |> 
  filter(
    candidate %in% c(
      "Ganjar Pranowo", 
      "Prabowo Subianto",
      "Anies Baswedan",
      "Agus Yudhoyono",
      "Khofifah Parawansa",
      "Ridwan Kamil",
      "Sandiaga Uno"
    )
  )

ggplot(modPollTweetResult, aes(tweet_per_day_prepoll, share_of_respondents)) +
  geom_hline(yintercept = 0, lwd = 12/22) +
  geom_point(
    aes(size = reaction_per_tweet_latest_pp),
    pch = 21,
    fill = palette[[3]],
    color = "white",
    alpha = 0.9
  ) +
  geom_text(
    aes(label = candidate), 
    data = annoTextPollTweet,
    size = dfr_convert_font_size(),
    hjust = "inward"
  ) +
  scale_x_log10(
    name = "Rata-rata twit per hari<sup>†</sup><br>(skala logaritmik)",
    breaks = c(0.3, 1, 3, 10, 30),
    labels = c(0.3, 1, 3, 10, 30),
    limits = c(0.1, 35)
  ) +
  scale_y_continuous(
    name = "Proporsi responden<br>(%)",
    breaks = seq(0, 30, 5),
    limits = c(0, 30),
    position = "right"
  ) +
  scale_size_binned(
    name = "Jumlah reaksi\nper twit**",
    breaks = c(13.20000, 50, 500, 3571.5),
    labels = c(13.20000, 50, 500, 3571.5),
    range = c(2, 10)
  ) +
  guides(
    size = guide_bins(
      title.hjust = 0,
      label.hjust = 0,
      label.position = "left",
      axis.colour = "#757575",
      override.aes = list(color = "#757575", fill = NA)
    )
  ) +
  labs(
    title = "Aktif di Twitter, lebih baik dalam jajak pendapat",
    subtitle = paste0(
      "Proporsi responden yang memilih sebagai presiden*<br>",
      "dan aktivitas di Twitter"
    ),
    caption = paste0(
      "\\*Survei pada 14-19 April 2022<br>",
      "\\*\\*Retwit dan favorit. Data dalam satu bulan sebelum survei<br>",
      "atau tanggal terakhir yang tersedia<br>",
      "<sup>†</sup>Per 13 April 2022<br>",
      "Sumber: Twitter; Indikator; analisis *Katadata*<br>",
      "Grafik: Katadata/Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.9),
    legend.title = element_text(color = "#757575", size = rel(0.9166667)),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  here(pathProject, "result", "poll-tweet-scatter-plot.svg"),
  width = 4.5,
  height = 5.5
)


## Daily new tweets ----

tweetRecentDaily <- read_csv(
  here(pathProject, "result", "tweet-per-day-recent.csv")
)

ggplot(tweetRecentDaily, aes(date_tweet_created)) +
  geom_point(
    aes(y = tweet), 
    size = 1,
    pch = 21,
    fill = palette[[1]],
    color = "white",
    alpha = 0.75
  ) +
  geom_line(
    aes(y = rolling_mean_7_day),
    color = palette[[3]],
    lwd = 1
  ) +
  scale_x_date(
    name = NULL, 
    labels = label_date("%b", locale = "id")
  ) +
  scale_y_continuous(
    name = NULL,
    breaks = seq(0, 50, 10),
    limits = c(0, 55),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Aktivitas Twitter mayoritas politisi cenderung stabil",
    subtitle = "Jumlah twit baru harian, 8 Maret-13 Juli 2022",
    caption = paste0(
      "Sumber: Twitter; analisis *Katadata*<br>",
      "Grafik: Katadata/Dzulfiqar Fathur Rahman"
    )
  ) +
  facet_wrap(~ candidate, nrow = 3, scales = "free_x") +
  dfr_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

ggsave(
  here(pathProject, "result", "tweet-per-day-recent-small-multiple.svg"),
  width = 7.25,
  height = 7.25
)


## Twitter account age ----

twitterAgeYear <- modPollTweetResult |> 
  mutate(
    account_age_year = account_age / 365,
    candidate = fct_reorder(candidate, account_age_year, .desc = TRUE)
  )

annoTextTwitterAge <- twitterAgeYear |> 
  filter(!(candidate %in% c("Puan Maharani", "Suharso Monoarfa")))

ggplot(twitterAgeYear, aes(account_age_year, candidate)) +
  geom_col(
    width = 0.75,
    fill = palette[[3]],
    color = "white"
  ) +
  geom_label(
    aes(x + 0.25, y, label = y),
    data = tibble(
      x = c(0.7835616, 2.115068),
      y = c("Puan Maharani", "Suharso Monoarfa")
    ),
    size = dfr_convert_font_size(),
    hjust = 0,
    vjust = 0.5,
    label.size = NA,
    label.padding = unit(0, "cm")
  ) +
  geom_text(
    aes(0.25, label = candidate),
    data = annoTextTwitterAge,
    size = dfr_convert_font_size(),
    color = "white",
    hjust = 0,
    vjust = 0.5
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = seq(0, 15, 3),
    limits = c(0, 15),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_y_discrete(name = NULL) +
  labs(
    title = "Wajah lama, warganet baru",
    subtitle = "Usia akun Twitter\\* (tahun)",
    caption = paste0(
      "\\*Per 13 Juli 2022<br>",
      "Sumber: Twitter; analisis *Katadata*<br>",
      "Grafik: Katadata/Dzulfiqar Fathur Rahman"
    )
  ) +
  dfr_theme() +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here(pathProject, "result", "twitter-age-bar-chart.svg"),
  width = 4.5,
  height = 5.5
)