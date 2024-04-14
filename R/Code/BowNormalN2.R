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
  jsonlite
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

store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google", "all", "all", "all")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all", "nubank", "itau", "bb")
combinacoes <- data.frame(store, company)

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords_n2.json")
default_stopwords <- fromJSON("../Datasets/default_stopwords.json")
stopwords_final <- c(default_stopwords, custom_stopwords)

reviews_words <- read.csv("./Datasets/total-words-20000.csv")

for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  #companyName <- "nubank"
  #storeName <- "google"
  output <- paste0(companyName, "-", storeName)
  fileBow <- paste0("Output/normal_bow_n2/", output, "-bow-n2-wordcloud.png")
  fileBar <- paste0("Output/normal_bow_n2/", output, "-bow-n2-bar.png")
  
  reviews_words_by_company <- reviews_words %>% filter(company == companyName, store == storeName)
  reviews_words_by_company <- reviews_words_by_company %>%
    group_by(id) %>%
    summarize(text = paste(word, collapse = " "))
  
  corpus = VCorpus(VectorSource(reviews_words_by_company$text))
  corpus = tm_map(corpus, content_transformer(tolower)) 
  #corpus = tm_map(corpus, removeNumbers) 
  corpus = tm_map(corpus, content_transformer(rm_accent))
  corpus = tm_map(corpus, removeWords, stopwords_final)
  corpus = tm_map(corpus, stripWhitespace) 
  corpus = tm_map(corpus, content_transformer(trim_ngram))
  
  content <- sapply(corpus, as.character)
  df <- data.frame(text = content)
  names(df) <- c("word")
  df <- df %>% filter(word != "")
  
  df <- df %>%
    unnest_tokens(word, word, token = "ngrams", n = 2)
  
  df <- df %>%
    filter(!is.na(word))
  
  bow <- df %>% count(word, sort = TRUE)
  
  png(fileBow, width = 1500, height = 1500, res = 200)
  bow_wordcloud <- bow %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 20, colors="blue"))
  dev.off()
  
  graph_data <- head(bow, 15)
  graph_data <- graph_data %>%
    mutate(word = forcats::fct_reorder(word, desc(n))) 
  
  graph <- ggplot(graph_data, aes(x = n, y = forcats::fct_rev(forcats::fct_infreq(word)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label=n), vjust=0.5, hjust=1.2) + 
    labs(title = "Bag of words n2", x = "Frequência", y = "Palavra")
  
  ggsave(fileBar, plot = graph, width = 10, height = 6, units = "in")
}

#numeros[order(numeros, decreasing = TRUE)]

