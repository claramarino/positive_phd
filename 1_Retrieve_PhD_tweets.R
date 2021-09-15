rm(list=ls())

library(rtweet)

# CREATE AND STORE API TOKEN

# ## store api keys 
# api_key <- "TwiJsSmZwH1fTo8NuEhe7tMwL"
# api_secret_key <- "jytiJsoSlzF73SINE5WucXpEH9kQp8fEsHppLgczXMBrDmV477"
# access_token <- "1317022641937592320-QpSnm1Dwacu6SJdETF7NyrW68oi45J"
# access_token_secret <- "yAsubHRzj2ex6vDOfSkhLqqJbTalqpkrOG56OlMGImBfz"
# 
# ## authenticate via web browser
# token <- create_token(
#   app = "marino_biom",
#   consumer_key = api_key,
#   consumer_secret = api_secret_key,
#   access_token = access_token,
#   access_secret = access_token_secret)
# 
# saveRDS(token, "tweet_token")
token <- readRDS(file = "tweet_token")

# RETRIEVE TWEETS WITH QUERY

# test for phdlife
rt_phd_life <- search_tweets(q = "phdlife")
saveRDS(rt_phd_life, "Output/1_rt_phd_life")

# select all tweets mentioning phd
# select only english language
# retry on rate limit for getting more than 18000 tweets

rt_phd <- search_tweets(q = "phd", n = 50000, lang = "en", token = token,
                        retryonratelimit = T)

saveRDS(rt_phd, paste0("Output/1_rt_phd_", Sys.Date()))

# careful, check that phd is contained at least once in the text
# remove all tweets from phd accounts which do not talk about phd 

names(rt_phd)

# when looking at tweets, "phd" is not excluding enough for phd experience
# try other query
q_phd_exp = "phdlife OR phdspeaks OR phdvoice OR phdchat OR phdtips OR phdstudent"
rt_phd_exp <- search_tweets(q = q_phd_exp, n = 50000, lang = "en", token = token,
                        retryonratelimit = T)
saveRDS(rt_phd_exp, paste0("Output/1_rt_phd_exp_", Sys.Date()))

# when the tweets have been published
range(rt_phd_exp$created_at)

