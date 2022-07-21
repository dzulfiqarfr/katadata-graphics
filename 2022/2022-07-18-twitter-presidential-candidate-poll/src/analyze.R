pathProject <- "2022/2022-07-18-twitter-presidential-candidate-poll"

here::i_am(paste(pathProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(lubridate)
library(zoo)
library(broom)
library(rtweet)


# Data ----

twitter <- read_csv(
  here(pathProject, "data", "author-presidential-candidate-twitter.csv")
)

twitterHandle <- twitter |> 
  filter(!is.na(twitter_handle)) |> 
  pull(twitter_handle)

pathTwitterProfile <- here(
  pathProject, "data", "twitter-presidential-candidate-profile-raw.csv"
)

if (!file.exists(pathTwitterProfile)) {
  twitterProfile <- map_df(twitterHandle, lookup_users)
  
  write_csv(twitterProfile, pathTwitterProfile)
} else {
  twitterProfile <- read_csv(pathTwitterProfile)
}

pathTweet <- here(
  pathProject, "data", "twitter-presidential-candidate-timeline-raw.csv"
)

if (!file.exists(pathTweet)) {
  resp <- map(
    twitterHandle,
    get_timeline,
    n = Inf,
    retryonratelimit = TRUE
  )
  
  # Convert `possibly_sensitive` column to `list` to prevent error
  # when binding the data. This column consists mostly of missing values
  tweet <- resp |> 
    set_names(twitterHandle) |> 
    map(mutate, possibly_sensitive = as.list(possibly_sensitive)) |> 
    bind_rows(.id = "twitter_handle")
  
  write_csv(tweet, pathTweet)
} else {
  tweet <- read_csv(pathTweet)
}

poll <- read_csv(
  here(pathProject, "data", "indikator-poll-2022-04-14-cleaned.csv")
)


# Analysis ----

## Twitter profile summary: tweet per day before poll ----

# Join `twitterProfile` and `twitter` to add the candidates' full name to the
# former. It will facilitate mutating join between the former and poll data
twitterProfile <- twitterProfile |> 
  left_join(twitter, by = c("screen_name" = "twitter_handle")) |> 
  relocate(candidate)

twitterProfileSub <- twitterProfile |> 
  select(
    candidate,
    twitter_handle = screen_name, 
    dttm_join_twitter = created_at,
    follower = followers_count, 
    tweet_total = statuses_count
  )

# Get the start date of the poll to set the cutoff for subsetting the data
pollDateStart <- sample(poll$date_survey_start, size = 1)

twitterProfileAge <- twitterProfileSub |> 
  mutate(
    date_join_twitter = date(dttm_join_twitter), 
    account_age = pollDateStart - date_join_twitter
  ) |> 
  select(!date_join_twitter)

tweet <- tweet |> 
  mutate(date_tweet_created = date(created_at)) |> 
  rename(dttm_tweet_created = created_at) |> 
  relocate(date_tweet_created, .after = dttm_tweet_created)

# Calculate the number of tweets made by each politician after the poll
tweetAfterPoll <- tweet |> 
  filter(dttm_tweet_created >= pollDateStart) |> 
  group_by(twitter_handle) |> 
  summarize(tweet_after_poll = n()) |> 
  ungroup()

# Calculate the average tweet each politician wrote per day in his or her
# lifetime on Twitter before the poll. For politicians with no tweets
# after the poll, use their total tweet data instead
tweetPerDayPrepoll <- twitterProfileAge |> 
  left_join(tweetAfterPoll) |> 
  mutate(
    tweet_prepoll = tweet_total - tweet_after_poll,
    tweet_per_day_prepoll = case_when(
     !is.na(tweet_prepoll) ~ tweet_prepoll / as.integer(account_age),
     TRUE ~ tweet_total / as.integer(account_age)
    )
  )


## Tweet reaction: retweet and favorite ----

# Subset tweets written one month prior to the poll. For politicians with no
# recent tweets, subset the latest one month worth of tweets available
tweetLatestMonthPrepoll <- tweet |> 
  filter(date_tweet_created < pollDateStart) |> 
  group_by(twitter_handle) |> 
  mutate(
    date_cutoff_end = max(dttm_tweet_created, na.rm = TRUE),
    date_cutoff_start = date_cutoff_end - dmonths()
  ) |> 
  filter(
    date_tweet_created %within% interval(date_cutoff_start, date_cutoff_end)
  ) |> 
  ungroup()
  
tweetLatestPrepollDateInt <- tweetLatestMonthPrepoll |> 
  select(twitter_handle, contains("cutoff")) |> 
  distinct(twitter_handle, .keep_all = TRUE)

# Calculate the number of tweets, retweets and favorites per day, excluding
# retweets. Then, combine retweet and favorite counts to compute the number
# of reactions
reactionLatestMonthPrepoll <- tweetLatestMonthPrepoll |> 
  filter(!str_detect(full_text, "^RT")) |> 
  group_by(twitter_handle) |> 
  summarize(
    tweet_latest_prepoll = n(),
    rt_latest_prepoll = sum(retweet_count, na.rm = TRUE),
    fav_latest_prepoll = sum(favorite_count, na.rm = TRUE),
    reaction_latest_prepoll = rt_latest_prepoll + fav_latest_prepoll
  ) |>
  ungroup() |> 
  mutate(
    reaction_per_tweet_latest_pp = reaction_latest_prepoll / tweet_latest_prepoll
  )

reactionLatestMonthPrepoll <- reactionLatestMonthPrepoll |> 
  left_join(tweetLatestPrepollDateInt) |> 
  relocate(date_cutoff_start, date_cutoff_end, .after = twitter_handle)

reactionLatestMonthPrepoll |> 
  write_csv(
    here(pathProject, "result", "twitter-reaction-latest-month-prepoll.csv")
  )


## Twitter timeline: daily new tweets ----

# For each politician, find the earliest tweet available upon latest request.
# Among these tweets, take the most recent one. Set its date as the cutoff
# to subset the data. This will give us a subset of data with more 
# politicians and their recent tweets, given the date interval
tweetRecentDateCutoff <- tweet |> 
  group_by(twitter_handle) |> 
  summarize(date_tweet_created_min = min(date_tweet_created, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(date_tweet_created_min == max(date_tweet_created_min)) |> 
  pull(date_tweet_created_min)

tweetRecent <- tweet |> 
  group_by(twitter_handle) |> 
  filter(date_tweet_created >= tweetRecentDateCutoff) |> 
  ungroup()

tweetRecentDaily <- tweetRecent |> 
  group_by(twitter_handle, date_tweet_created) |> 
  summarize(tweet = n()) |>
  ungroup(date_tweet_created) |> 
  mutate(rolling_mean_7_day = rollmean(tweet, 7, fill = NA, align = "right")) |> 
  ungroup()

tweetRecentDaily <- tweetRecentDaily |> 
  left_join(twitter) |> 
  relocate(candidate)

# Drop politicians who have not been tweeting that much lately, but try to
# preserve at least 12 politicians
tweetRecentDailySub <- filter(tweetRecentDaily, twitter_handle != "ZUL_Hasan")

tweetRecentDailySub |> 
  write_csv(here(pathProject, "result", "tweet-per-day-recent.csv"))


## Model: poll and Twitter ----

# Drop politicians without a Twitter account or who hasn't tweeted in a while
pollTweetSummary <- poll |> 
  left_join(tweetPerDayPrepoll) |>
  left_join(reactionLatestMonthPrepoll) |> 
  filter(
    !is.na(twitter_handle),
    date_cutoff_end > ymd("2020-01-01")
  )

modPollTweet <- lm(
  share_of_respondents ~ tweet_per_day_prepoll + reaction_per_tweet_latest_pp,
  data = pollTweetSummary
)

modPollTweetResult <- augment(modPollTweet, pollTweetSummary)

modPollTweetResult |>
  write_csv(here(pathProject, "result", "poll-tweet-model-result.csv"))

modPollTweetFinding <- tidy(modPollTweet)

modPollTweetSummary <- glance(modPollTweet)