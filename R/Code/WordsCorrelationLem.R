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

store <- c("apple", "google", "apple", "google", "apple", "google")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb")
combinacoes <- data.frame(store, company)

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

reviews_words_lem <- read_xlsx("./Datasets/total-words-20000-lemmatizated.xlsx")
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
  file1 <- paste0("Output/lem_words_correlation/", output, "-network-correlation-0.3.png")
  file2 <- paste0("Output/lem_words_correlation/", output, "-network-correlation-0.3_0.5.png")
  file3 <- paste0("Output/lem_words_correlation/", output, "-network-correlation-0.5.png")
  fileCorrTable <- paste0("Output/lem_words_correlation/", output, "-correlation-table-bow-ng1-10w.png")
  fileCSV <- paste0("Output/lem_words_correlation/", output, "-network-correlation.csv")
  
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
  
  #word_pairs <- df %>%
  #  pairwise_count(word, id, sort = TRUE)
  
  word_cors <- df %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, id, sort = TRUE)
  
  word_cors_filtered1 <- word_cors %>% filter(correlation >= 0.3 ) %>% head(n = 100)
  word_cors_filtered2 <- word_cors %>% filter(correlation >= 0.3, correlation <= 0.5 ) %>% head(n = 100)
  word_cors_filtered3 <- word_cors %>% filter(correlation >= 0.5 ) %>% head(n = 100)
  
  # Gráfico network
  
  graph1 <- word_cors_filtered1 %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
  graph2 <- word_cors_filtered2 %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
  graph3 <- word_cors_filtered3 %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
  print(graph)
  ggsave(file1, plot = graph1, width = 6, height = 4, dpi = 300, bg = "white")
  ggsave(file2, plot = graph2, width = 6, height = 4, dpi = 300, bg = "white")
  ggsave(file3, plot = graph3, width = 6, height = 4, dpi = 300, bg = "white")
  #write.csv(word_cors, firevle = fileCSV, row.names = FALSE)
  
  matriz_index <- df %>% count(word, sort = TRUE) %>% head(10)
  matriz <- matrix(0, nrow = 10, ncol = 10, dimnames = list(matriz_index$word, matriz_index$word))
  matriz_result <- matrix(0, nrow = 10, ncol = 10, dimnames = list(matriz_index$word, matriz_index$word))
  
  word_cors_clean <- word_cors %>% 
    mutate(correlation = round(correlation, digits = 1))
  
  for (i in 1:nrow(matriz)) {
    linha <- rownames(matriz)[i]
    for (j in 1:ncol(matriz)) {
      coluna <- colnames(matriz)[j]
      
      valor <- word_cors_clean %>%
        filter(item1 == linha, item2 == coluna) %>%
        pull(correlation)
      
      valor <- ifelse(linha == coluna, 1, valor)
      valor <- ifelse(all(is.na(valor)), 0, valor)
      #cat("Linha:", linha, ", Coluna:", coluna, ", Valor:", valor, "\n")
      
      matriz_result[i, j] <- valor
    }
  }
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) 
  png(fileCorrTable, width = 1000, height = 1000, res = 200)
  corrplot(matriz_result, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")
  dev.off()
}
