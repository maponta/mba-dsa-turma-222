library("pacman")

pacman::p_load(
  corrplot,
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
  stringr,
  tidyr,
  text2vec
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

clean_ngram <- function(string) {
  string <- gsub("'", "", string)
  string <- gsub("`", "", string)
  string <- gsub("\n", "", string)
  string <- trimws(string)
  return(string)
}

topFinnProducts <- c("conta", "cartao", "pix", "credito")
store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all")
combinacoes <- data.frame(store, company)

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

reviews_words_lem <- read_xlsx("./Datasets/total-words-20000-lemmatizated.xlsx")
reviews_words_lem <- transform(reviews_words_lem_base, company = "all") %>%
  rbind(reviews_words_lem_base)
reviews_words_lem <- transform(reviews_words_lem_base, store = "all") %>%
  rbind(reviews_words_lem)
reviews_words_lem$id <- seq.int(nrow(reviews_words_lem))
reviews_words_lem <- reviews_words_lem %>% select(id, company, store, content)
reviews_words_lem <- reviews_words_lem %>%
  mutate(store = tolower(store))

for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  #companyName <- "nubank"
  #storeName <- "google"
  output <- paste0(companyName, "-", storeName)
  fileTopProductsPositive <- paste0("Output/lem_top_products_words_correlation/", output, "-positive-top-products-correlation.png")
  fileTopProductsNegative <- paste0("Output/lem_top_products_words_correlation/", output, "-negative-top-products-correlation.png")
  
  #cat("Company:", lojas$company[company], "- Store:", lojas$store[store], "- Output:", output, "\n")
  
  reviews_words_by_company <- reviews_words_lem %>% filter(company == companyName, store == storeName)
  
  corpus = VCorpus(VectorSource(reviews_words_by_company$content))
  corpus = tm_map(corpus, content_transformer(clean_ngram))
  corpus = tm_map(corpus, content_transformer(tolower)) 
  corpus = tm_map(corpus, content_transformer(rm_accent))
  corpus = tm_map(corpus, removeWords, stopwords_final)
  corpus = tm_map(corpus, stripWhitespace) 
  corpus = tm_map(corpus, content_transformer(trim_ngram))
  
  content <- sapply(corpus, as.character)
  df <- data.frame(word = content)
  df <- df %>%
    mutate(id = row_number())
  df <- df %>%
    unnest_tokens(word, word, token = "ngrams", n = 1)
  df <- df %>% filter(word != "")
  
  word_cors <- df %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, id, sort = TRUE)
  
  wcplot_positive <- word_cors %>%
    filter(item1 %in% topFinnProducts) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    labs(title = paste0(output, ' Positive'), x = "Palavra", y = "Frequência") +
    coord_flip()
  
  wcplot_negative <- word_cors %>%
    filter(item1 %in% topFinnProducts) %>%
    group_by(item1) %>%
    slice_min(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    labs(title = paste0(output, ' Negative'), x = "Palavra", y = "Frequência") +
    coord_flip()
  
  ggsave(fileTopProductsPositive, plot = wcplot_positive, width = 10, height = 6, units = "in")
  ggsave(fileTopProductsNegative, plot = wcplot_negative, width = 10, height = 6, units = "in")
}

# set.seed(2016)
# 
# word_cors %>%
#   filter(correlation > .45) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
#   geom_node_point(color = "lightblue", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_void()