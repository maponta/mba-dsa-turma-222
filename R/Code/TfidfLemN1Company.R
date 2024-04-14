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
  readxl,
  stringi,
  tokenizers
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

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

reviews_words_lem <- read_xlsx("./Datasets/total-words-20000-lemmatizated.xlsx")
reviews_words_lem$id <- seq.int(nrow(reviews_words_lem))
reviews_words_lem <- reviews_words_lem %>% select(id, company, store, content)
reviews_words_lem <- reviews_words_lem %>%
  mutate(store = tolower(store))
reviews_words_lem <- reviews_words_lem %>%
  unnest_tokens(word, content, token = "ngrams", n = 1)

fileBow <- paste0("Output/lem_tfidf_n1_company/", "all", "-bow-n1-wordcloud.png")
fileBar <- paste0("Output/lem_tfidf_n1_company/", "all", "-bow-n1-bar.png")

reviews_words_by_company <- reviews_words_lem %>% filter(company != "all", store != "all")

df <- reviews_words_by_company %>% 
  filter(word != "") %>%
  group_by(company) %>% 
  count(word, company, sort = TRUE)

tf_idf <- df %>% bind_tf_idf(word, company, n)

tf_idf_itau <- tf_idf %>% filter(company == "itau")
tf_idf_bb <- tf_idf %>% filter(company == "bb")
tf_idf_nubank <- tf_idf %>% filter(company == "nubank")

company_graph <- tf_idf %>% 
  group_by(company) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 

graph <- company_graph %>% ggplot(aes(tf_idf, word, fill = company)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~company, ncol = 2, scales = "free")

ggsave(fileBar, plot = graph, width = 10, height = 6, units = "in")
