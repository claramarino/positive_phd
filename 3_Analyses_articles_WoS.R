# analyses scientific corpus

rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(qdap)
library(tidytext)


# load article abstracts
abs_150 <- read.csv2("wos_01-10-21_150papers.csv")

abs_text <- abs_150$Abstract
abs_title <- abs_150$Article.Title

plain_text <- iconv(abs_text, from="UTF-8", to="ASCII", sub="")
plain_text_stm <- get_sentiment(plain_text, method = "bing")
plain_text_nrc <- get_nrc_sentiment(plain_text)

length(which(plain_text_stm<0))
length(which(plain_text_stm>0))

abs_words <- abs_150 %>%
  select(Abstract) %>%
  unnest_tokens(word, Abstract) %>%
  anti_join(stop_words)

abs_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("firebrick", "chartreuse3"),
                   max.words = 100)

abs_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))


abs_150_stm <- bind_cols(abs_150, plain_text_stm)
names(abs_150_stm)[69] <- "stm_bing"

cor.test(log(abs_150_stm$Times.Cited..All.Databases+1), abs_150_stm$stm_bing)

ggplot(abs_150_stm, aes(x = stm_bing, y = log(Times.Cited..All.Databases+1))) +
  geom_point() + geom_smooth()
