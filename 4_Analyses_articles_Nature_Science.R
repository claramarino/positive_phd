# analyses scientific corpus => 
# only opinion/perspectives in Nature and Science
# Query = PhD student OR PhD cancidate in title
# n = 93 papiers
# remove obituaries
# some papers were in image format => not included for now
rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(qdap)
library(tidytext)

# install.packages("pdftools")
# library(pdftools)

# setwd(dir = paste0(getwd(), "/data_pdf"))
# files <- list.files(pattern = "pdf$")
# 
# papers_pdf <- lapply(files, pdf_text)
# 
# papers_pdf[[5]]
# files
# => ne fonctionne aps bien pbm de police de certains pdf
# + bcp sont inclus avec d'autres papiers dans le meme pdf
# ça aurait diminué la pertience de la sélection

papers_excel <- read.csv("data_pdf/all_papers_filtered.csv", sep = ";")
papers_to_keep <- papers_excel %>% 
  filter(integration=="oui") %>%
  distinct()

papers_to_keep[duplicated(papers_to_keep$title),"title"]

abs_text <- papers_to_keep$text
abs_title <- papers_to_keep$title

plain_text <- iconv(abs_text, from="UTF-8", to="ASCII", sub="")
plain_text_stm <- get_sentiment(plain_text, method = "bing")
plain_text_nrc <- get_nrc_sentiment(plain_text)

length(which(plain_text_stm<0))
length(which(plain_text_stm>0))

abs_words <- papers_to_keep %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

abs_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("firebrick", "chartreuse3"),
                   max.words = 100)


nb_stmt <- abs_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

table(nb_stmt$sentiment)

nb_stmt %>% group_by(sentiment) %>% summarise(total = sum(n), mean = mean(n))

hist(nb_stmt$n)
abs_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))

