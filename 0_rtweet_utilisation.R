# test tweetr
install.packages("rtweet")
library(rtweet)
## view rtweet's authorization vignette
vignette("auth", package = "rtweet")
app_name <- "mwk_twitter_app"
consumer_key <- "XYznzPFOFZR2a39FwWKN1Jp41"
consumer_secret <- "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD"

## create token
token <- create_token(app_name, consumer_key, consumer_secret)
## print token
token

## save token to home directory
path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)
## create env variable TWITTER_PAT (with path to saved token)
env_var <- paste0("TWITTER_PAT=", path_to_token)
## save as .Renviron file (or append if the file already exists)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)

## refresh .Renviron variables
readRenviron("~/.Renviron")

# Search tweets
## search for a keyword
rt <- search_tweets(q = "rstats")
## search for a phrase
rt <- search_tweets(q = "data science")
## search for multiple keywords
rt <- search_tweets(q = "rstats AND python")

# By default, search_tweets() returns 100 tweets. 
# To return more (rate limit is 18,000 per 15 minutes), set n to a higher number.

## search tweets (q = search query; n = desired number of tweets to return)
rt <- search_tweets(q = "rstats", n = 1000)

# Use OR between search terms to find any match.
## search for any mention of a list of words
rt <- search_tweets("statistics OR statistical OR quantitative")

#Specify a language of the tweets and exclude retweets.
## search for tweets in english that are not retweets
rt <- search_tweets("rstats", lang = "en", include_rts = FALSE)

#Search by geo-location.
## search for tweets in english that are not retweets
rt <- search_tweets("lang:en", geocode = lookup_coords("Chicago, IL"))


# Other data functions
# search_users()
# lookup_users()
# get_trends()
# stream_tweets()
# lists_members()
# lists_statuses()

# Other utility functions
# lookup_coords()
# tweet_shot()
# post_*()
# ts_data()
# lat_lng()
# emojis
# stopwordslangs

## function to round time (created_at)
round_time <- function(x, secs) as.POSIXct(hms::round_hms(x, secs))
## function to calculate sentiment scores
sent_scores <- function(x) syuzhet::get_sentiment(plain_tweets(x)) - .5
## calc data set with sentiment variable
tt_sent <- tt %>%
  mutate(days = round_time(created_at, 60 * 60 * 24),
         sentiment = sent_scores(text))
## aggregate by rounded time interval
tt_sent  %>% 
  group_by(days) %>%
  summarise(sentiment = sum(sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = weeks, y = sentiment)) +
  geom_point(aes(colour = sentiment > 0)) + 
  geom_smooth(method = "loess", span = .2) + 
  scale_color_manual(values = c("#dd3333", "#22aa33")) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "#000000cc") + 
  theme_minimal(base_family = "Helvetica Neue")



#############################

## search for 18000 tweets using the rstats hashtag
rt <- search_tweets("#rstats", n = 18000, include_rts = FALSE)

## preview tweets data
rt

## preview users data
users_data(rt)

## plot time series (if ggplot2 is installed)
ts_plot(rt)

## plot time series of tweets
ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## search for 250,000 tweets containing the word data
rt <- search_tweets("data", n = 250000, retryonratelimit = TRUE)