#install.packages("")
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
library("widyr")
library("ggplot2")
library("igraph")
library("ggraph")
library("lexiconPT")
library("stringr")
library("tidyr")
library("tm")
library("wordcloud")
library("textstem")
library("readxl")

# Sentando o diretório padrão, rode PWD no terminal
setwd("./R")
getwd()

rm_accent <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google", "all", "all", "all")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all", "nubank", "itau", "bb")
combinacoes <- data.frame(store, company)

lexicon <- oplexicon_v2.1
#glimpse(sentiLex_lem_PT02)

pal <- brewer.pal(8,"Dark2")
stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

reviews_words <- read.csv("./Datasets/total-words-20000.csv")


for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  output <- paste0(companyName, "-", storeName)
  fileWordcloudPositve <- paste0("Output/normal_sentiment_oplexicon_v2.1/", output, "-postive-wordcloud.png")
  fileWordcloudNeutral <- paste0("Output/normal_sentiment_oplexicon_v2.1/", output, "-neutral-wordcloud.png")
  fileWordcloudNegative <- paste0("Output/normal_sentiment_oplexicon_v2.1/", output, "-negative-wordcloud.png")
  
  #cat("Company:", lojas$company[company], "- Store:", lojas$store[store], "- Output:", output, "\n")
  
  reviews_words_by_company <- reviews_words %>% filter(company == companyName, store == storeName)
  
  corpus = VCorpus(VectorSource(reviews_words_by_company))
  #corpus = tm_map(corpus, removeNumbers) 
  corpus = tm_map(corpus, content_transformer(rm_accent))
  corpus = tm_map(corpus, removeWords, stopwords_final)
  #corpus = tm_map(corpus, stripWhitespace) 
  
  content <- sapply(corpus, as.character)
  df <- data.frame(text = content)
  names(df) <- c("id", "company", "store", "word")
  df <- df %>% filter(word != "")
  
  text_sentiment <-
    inner_join(reviews_words_by_company, lexicon, by = c("word" = "term")) %>%
    count(word, type, polarity, sort = TRUE)
  
  png(fileWordcloudPositve, width = 600, height = 600, res = 200)
  positive_wordcloud <- text_sentiment %>% 
    filter(polarity == 1) %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
  dev.off()
  
  png(fileWordcloudNeutral, width = 600, height = 600, res = 200)
  neutral_wordcloud <- text_sentiment %>% 
    filter(polarity == 0) %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
  dev.off()
  
  png(fileWordcloudNegative, width = 600, height = 600, res = 200)
  negative_wordcloud <- text_sentiment %>% 
    filter(polarity == -1) %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
  dev.off()
}
