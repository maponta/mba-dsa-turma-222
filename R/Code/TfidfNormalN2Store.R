library("pacman")

pacman::p_load(
  dplyr,
  ggraph,
  textstem,
  wordcloud,
  tm,
  tidytext,
  stringr,
  SnowballC,
  widyr,
  ggraph,
  igraph,
  lexiconPT,
  ggplot2,
  tidyr,
  text2vec,
  jsonlite,
  stringi
)

# Sentando o diretório padrão, rode PWD no terminal
setwd("./R")
getwd()

rm_accent <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

trim_ngram <- function(string) {
  string <- trimws(string)
  return(string)
}

custom_stopwords <- fromJSON("../Datasets/custom_stopwords_n2.json")
default_stopwords <- fromJSON("../Datasets/default_stopwords.json")
stopwords_final <- c(default_stopwords, custom_stopwords)

reviews_words <- read_xlsx("./Datasets/2023-total-volume-20000.xlsx")
reviews_words <- reviews_words %>%
  mutate(store = tolower(store))
reviews_words$id <- seq.int(nrow(reviews_words))
reviews_words <- reviews_words %>% select(id, company, store, content, at)

output <- paste0(companyName, "-", storeName)
fileBow <- paste0("Output/normal_tfidf_n2_store/", "all", "-bow-n2-wordcloud.png")
fileBar <- paste0("Output/normal_tfidf_n2_store/", "all", "-bow-n2-bar.png")
  
reviews_words_by_company <- reviews_words %>% filter(company != "all", store != "all")

corpus = VCorpus(VectorSource(reviews_words_by_company$content))
corpus = tm_map(corpus, content_transformer(tolower)) 
#corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, content_transformer(rm_accent))
corpus = tm_map(corpus, removeWords, stopwords_final)
corpus = tm_map(corpus, stripWhitespace) 

content <- sapply(corpus, as.character)
df <- data.frame(text = content)
names(df) <- c("word")
df$store <- reviews_words_by_company$store
df <- df %>% filter(word != "")

df <- df %>%
  unnest_tokens(word, word, token = "ngrams", n = 2)

df <- df %>% 
  filter(word != "") %>%
  group_by(store) %>% 
  count(word, store, sort = TRUE)
  
tf_idf <- df %>% bind_tf_idf(word, store, n)
  
company_graph <- tf_idf %>% 
  group_by(store) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 
  
graph <- company_graph %>% ggplot(aes(tf_idf, word, fill = store)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~store, ncol = 2, scales = "free")
  
ggsave(fileBar, plot = graph, width = 10, height = 6, units = "in")
