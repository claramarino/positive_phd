rm(list=ls())

library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(tm)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(qdap)
library(tidytext)


# load tweets

# rt_phd_exp1 <- readRDS("Output/1_rt_phd_exp_2021-09-15")
# rt_phd_exp2 <- readRDS("Output/1_rt_phd_exp_2021-09-23")
# rt_phd_exp3 <- readRDS("Output/1_rt_phd_exp_2021-09-29")
# rt_phd_exp4 <- readRDS("Output/1_rt_phd_exp_2021-10-07")
# rt_phd_exp5 <- readRDS("Output/1_rt_phd_exp_2021-10-13")
# rt_phd_exp6 <- readRDS("Output/1_rt_phd_exp_2021-10-20")
# rt_phd_exp7 <- readRDS("Output/1_rt_phd_exp_2021-10-27")
# rt_phd_exp8 <- readRDS("Output/1_rt_phd_exp_2021-11-03")
# rt_phd_exp9 <- readRDS("Output/1_rt_phd_exp_2021-11-17")
# rt_phd_exp10 <- readRDS("Output/1_rt_phd_exp_2021-11-24")
# rt_phd_exp11 <- readRDS("Output/1_rt_phd_exp_2021-12-02")
# rt_phd_exp12 <- readRDS("Output/1_rt_phd_exp_2021-12-08")
# 
# rt_phd_exp <- bind_rows(rt_phd_exp1, rt_phd_exp2, rt_phd_exp3,
#                         rt_phd_exp4, rt_phd_exp5, rt_phd_exp6,
#                         rt_phd_exp7, rt_phd_exp8, rt_phd_exp9,
#                         rt_phd_exp10, rt_phd_exp11, rt_phd_exp12) %>%
#   distinct()
# 
# saveRDS(rt_phd_exp, "Output/2_rt_phd_exp_all_dates")

rt_phd_exp <- readRDS("Output/2_rt_phd_exp_all_dates")

text = rt_phd_exp$text


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

# Takes time (+20min)

# plain_text <- iconv(text, from="UTF-8", to="ASCII", sub="")
# plain_text_stm <- get_sentiment(plain_text, method = "bing")
# plain_text_nrc <- get_nrc_sentiment(plain_text)
# 
# saveRDS(plain_text_stm, "Output/2_stm_bing_all_tweets")
# saveRDS(plain_text_nrc, "Output/2_stm_nrc_all_tweets")

plain_text_stm <- readRDS("Output/2_stm_bing_all_tweets")
plain_text_nrc <- readRDS("Output/2_stm_nrc_all_tweets")

# get_nrc_sentiment("struggle")
length(which(plain_text_stm<0))
length(which(plain_text_stm>0))

# sur l'ensemble des tweets, 
# est ce que les pos/neg sont plus liké/rt ?

mean(rt_phd_exp$retweet_count[which(plain_text_stm>0)])
sd(rt_phd_exp$retweet_count[which(plain_text_stm>0)])
mean(rt_phd_exp$retweet_count[which(plain_text_stm<0)])
sd(rt_phd_exp$retweet_count[which(plain_text_stm<0)])

wilcox.test(rt_phd_exp$retweet_count[which(plain_text_stm>0)],
       rt_phd_exp$retweet_count[which(plain_text_stm<0)])

posit_rt = rt_phd_exp$retweet_count[which(plain_text_stm>0)]
neg_rt = rt_phd_exp$retweet_count[which(plain_text_stm<0)]

posit_fav = rt_phd_exp$favorite_count[which(plain_text_stm>0)]
neg_fav = rt_phd_exp$favorite_count[which(plain_text_stm<0)]

hist(log(neg_rt+1))
hist(log(posit_rt+1))

ks.test(log(neg_rt+1), log(posit_rt+1))

df <- data.frame(
  Sentiment = c(rep("positive", length(posit_rt)), rep("negative",length(neg_rt))),
  Retweet_count = c(posit_rt, neg_rt),
  Favorite_count = c(posit_fav, neg_fav)) %>%
  mutate(Retweet_ln = log(Retweet_count + 1),
         Favorite_ln = log(Favorite_count + 1))

source("Z:/THESE/6_Projects/mechanisms_birds/R/R_rainclouds.R")

prt <- ggplot(df, aes(x= 1, y = Retweet_ln, fill = Sentiment)) +
  geom_flat_violin(aes(fill = Sentiment),
                   position = position_nudge(x = .15, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_boxplot(aes(x = 1, y = Retweet_ln, fill = Sentiment),
               outlier.shape = NA, alpha = .8, width = .3, colour = "grey30")+
  stat_summary(fun = "mean", color="violetred3", shape=18, size = 1)+ 
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  labs(fill="", colour="") +
  theme_classic()+
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()

prt


pfav <- ggplot(df, aes(x= 1, y = Favorite_ln, fill = Sentiment)) +
  geom_flat_violin(aes(fill = Sentiment),
                   position = position_nudge(x = .15, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_boxplot(aes(x = 1, y = Favorite_ln, fill = Sentiment),
               outlier.shape = NA, alpha = .8, width = .3, colour = "grey30")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  labs(fill="", colour="") +
  theme_classic()+
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.title.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()

pfav


# filtrer que les tweets oiginaux (pas de rt)
no_rt <- bind_cols(rt_phd_exp, plain_text_nrc, plain_text_stm) %>%
  filter(is_retweet=="FALSE")
names(no_rt)[101] <- "stm_bing"


### LExique Bing

# nombre de tweets dans chaque caté
dim(no_rt %>% filter(stm_bing>0)) #positif
dim(no_rt %>% filter(stm_bing<0)) #negatif
dim(no_rt %>% filter(stm_bing==0)) #neutre

# est-ce que les neg sont + liké que les posi ?
mean(pull(no_rt %>% filter(stm_bing>0), favorite_count ))
mean(pull(no_rt %>% filter(stm_bing<0), favorite_count ))
t.test(pull(no_rt %>% filter(stm_bing>0), favorite_count ),
       pull(no_rt %>% filter(stm_bing<0), favorite_count ))
wilcox.test(pull(no_rt %>% filter(stm_bing>0), favorite_count ),
       pull(no_rt %>% filter(stm_bing<0), favorite_count ))

# est-ce que les neg sont + rt que les posi ?
mean(pull(no_rt %>% filter(stm_bing>0), retweet_count ))
mean(pull(no_rt %>% filter(stm_bing<0), retweet_count ))

t.test(pull(no_rt %>% filter(stm_bing>0), retweet_count ),
       pull(no_rt %>% filter(stm_bing<0), retweet_count))
wilcox.test(pull(no_rt %>% filter(stm_bing>0), retweet_count ),
            pull(no_rt %>% filter(stm_bing<0), retweet_count))

# Visualization
ggplot(no_rt, aes(x = stm_bing, y = favorite_count)) +
  geom_point()

ggplot(no_rt, aes(x = stm_bing)) +
  geom_histogram()


no_rt_bin <- no_rt %>% 
  mutate(bing_bin = if_else(stm_bing>0, "positive", 
                            if_else(stm_bing==0, "neutral","negative"))) %>%
  select(bing_bin, stm_bing, favorite_count, retweet_count, created_at)

ggplot(no_rt_bin %>% filter(favorite_count!=0), 
       aes(x = bing_bin, y = log(favorite_count+1)))+
  geom_point(position = position_jitter(width = .2), 
             col = "darkcyan", alpha = 0.4) + 
  geom_violin(alpha = 0.5) +
  stat_summary(fun = "mean", color="violetred3", shape=18, size = 1)+ 
  xlab("") +
  theme_classic2()

ggplot(no_rt_bin %>% filter(retweet_count!=0), 
       aes(x = bing_bin, y = log(retweet_count+1)))+
  geom_point(position = position_jitter(width = .2), 
             col = "darkcyan", alpha = 0.4) + 
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun = "mean", color="violetred3", shape=18, size = 1)+ 
  xlab("") +
  theme_classic2()


ggplot(no_rt_bin, aes(x = created_at, y = stm_bing)) +
  geom_point() + geom_smooth(method = "lm")

# nb de pers qui tweet du positif
length(unique(pull(no_rt %>% filter(stm_bing>0), user_id)))
# nb de pers qui tweet du negatif
length(unique(pull(no_rt %>% filter(stm_bing<0), user_id)))

par(mfrow=c(1,2))

pos_par_user <- no_rt %>% filter(stm_bing>0) %>%
  group_by(user_id) %>%
  count()
hist(pos_par_user$n[pos_par_user$n>1], 100)

neg_par_user <- no_rt %>% filter(stm_bing<0) %>%
  group_by(user_id) %>%
  count()
hist(neg_par_user$n[neg_par_user$n>1], 100)



####-------------- Wordclouds ---------------####

par(mfrow=c(1,1))

tweets <- rt_phd_exp %>%
  filter(is_retweet=="FALSE") %>% # attention à sorir si on veut tous les tweets
  select(text)
tweet_words <- tweets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# All words
tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent", "phdgenie")))%>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Filter with sentiment
# bonne raison d'utiliser bing :
# - classification plus simple des mots
# - moins de mots mais que les sentimentaux

tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent"))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("firebrick", "chartreuse3"),
                   max.words = 100)

frq_bing <- tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent"))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) 





#-------------- using nrc but weird result----------------------------

sum(rt_phd_exp$favorite_count[which(plain_text_nrc$negative>0)])/
  length(rt_phd_exp$favorite_count[which(plain_text_nrc$negative>0)])
sum(rt_phd_exp$favorite_count[which(plain_text_nrc$positive>0)])/
  length(rt_phd_exp$favorite_count[which(plain_text_nrc$positive>0)])


# est-ce que les neg sont + liké que les posi ?
mean(pull(no_rt %>% filter(negative>0), favorite_count ))
mean(pull(no_rt %>% filter(positive>0), favorite_count ))

# est-ce que les neg sont + rt que les posi ?
mean(pull(no_rt %>% filter(negative>0), retweet_count ))
mean(pull(no_rt %>% filter(positive>0), retweet_count ))

dim(no_rt %>% filter(negative>0))
dim(no_rt %>% filter(positive>0))

# est ce que les tweets neg sont plus neg ou les posi sont plus posi
mean(pull(no_rt %>% filter(negative>0), negative ))
mean(pull(no_rt %>% filter(positive>0), positive ))

# test avec lexique nrc
neg_pos_count_nrc <- tweet_words %>%
  filter(!(word %in% c("t.co","https","1","2","3","4","5","6",
                       "phdvoice","phdchat","phdlife", "phdtips",
                       "phdspeaks","phdstudent"))) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)
neg_pos_count_nrc[,6:7] %>%
  comparison.cloud(colors = c("firebrick", "chartreuse3"),
                   max.words = 100)


colSums(neg_pos_count)
max(neg_pos_count[,1])
max(neg_pos_count[,2])
names(neg_pos_count)


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
