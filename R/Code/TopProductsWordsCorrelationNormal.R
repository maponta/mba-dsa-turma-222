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

topFinnProducts <- c("conta", "cartao", "pix", "limite")
store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all")
combinacoes <- data.frame(store, company)

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

reviews_words <- read_xlsx("./Datasets/2023-total-volume-20000.xlsx")
reviews_words <- reviews_words %>%
  mutate(store = tolower(store))
reviews_words <- transform(reviews_words, company = "all") %>%
  rbind(reviews_words)
reviews_words$id <- seq.int(nrow(reviews_words))
reviews_words <- reviews_words %>% select(id, company, store, content)

for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  #companyName <- "nubank"
  #storeName <- "google"
  output <- paste0(companyName, "-", storeName)
  fileTopProductsPositive <- paste0("Output/normal_top_products_words_correlation/", output, "-positive-top-products-correlation.png")
  fileTopProductsNegative <- paste0("Output/normal_top_products_words_correlation/", output, "-negative-top-products-correlation.png")

  #cat("Company:", lojas$company[company], "- Store:", lojas$store[store], "- Output:", output, "\n")
  
  reviews_words_by_company <- reviews_words %>% filter(company == companyName, store == storeName)
  
  corpus = VCorpus(VectorSource(reviews_words_by_company$content))
  corpus = tm_map(corpus, content_transformer(tolower)) 
  #corpus = tm_map(corpus, removeNumbers) 
  corpus = tm_map(corpus, content_transformer(rm_accent))
  corpus = tm_map(corpus, removeWords, stopwords_final)
  
  content <- sapply(corpus, as.character)
  df <- data.frame(text = content)
  names(df) <- c("word")
  df$id <- reviews_words_by_company$id
  df <- df %>% filter(word != "")
  
  df <- df %>%
    unnest_tokens(word, word, token = "ngrams", n = 1)
  
  df <- df %>% filter(word != "")
  
  word_pairs <- df %>%
    pairwise_count(word, id, sort = TRUE)
  
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