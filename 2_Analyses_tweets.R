rm(list=ls())

library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(qdap)
library(tidytext)


# load tweets
rt_phd_exp <- readRDS("Output/1_rt_phd_exp_2021-09-15")

text = rt_phd_exp$text
text[2]


####---------------------PACKAGE tm ---------------------####
# create a corpus, word freq, associations 

tweets_source <- VectorSource(text)
# Make a volatile corpus: tweets_corpus
tweets_corpus <- VCorpus(tweets_source)
# Print out the tweets_corpus
tweets_corpus

# Clean tweets
clean_tw <- tm_map(tweets_corpus, removePunctuation)
clean_tw <- tm_map(clean_tw, removeWords, stopwords("english"))
clean_tw <- tm_map(clean_tw, stripWhitespace)

inspect(clean_tw[[2]])
inspect(tweets_corpus[[2]])

# document term matrix
dtm <- DocumentTermMatrix(clean_tw)
inspect(dtm)

# cherche les termes qui apparaissent plus de n fois
findFreqTerms(dtm, 100)

# find associations with phd
findAssocs(dtm, terms = "phd", corlimit = 0.5)
findAssocs(dtm, terms = "like", corlimit = 0.2)

# remove sparse terms (terms occuring only in one document)
dtm_high <- removeSparseTerms(dtm, 0.4)

frequent_terms <- freq_terms(text, 100, stopwords = stopwords("english"))
plot(frequent_terms[1:30,])

fre = frequent_terms$FREQ/max(frequent_terms$FREQ)
wordcloud(words = frequent_terms$WORD, freq = fre)


####--------------------- Sentiments ------------------------####

tweets <- rt_phd_exp %>%
  select(text)
tweet_words <- tweets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


plain_text <- iconv(text, from="UTF-8", to="ASCII", sub="")
plain_text_stm <- get_sentiment(plain_text)
plain_text_nrc <- get_nrc_sentiment(plain_text)

length(which(plain_text_stm<0))

tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent")))%>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent"))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("firebrick", "chartreuse3"),
                   max.words = 100)

neg_pos_count <- tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent"))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)


colSums(neg_pos_count)
max(neg_pos_count[,1])
max(neg_pos_count[,2])
names(neg_pos_count)


# using nrc but weird result----------------------------

# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment(tweets)


sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
