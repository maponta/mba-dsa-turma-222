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

store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google", "all", "all", "all")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all", "nubank", "itau", "bb")
combinacoes <- data.frame(store, company)

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

reviews_words <- read.csv("./Datasets/total-words-20000.csv")

for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  #companyName <- "nubank"
  #storeName <- "google"
  output <- paste0(companyName, "-", storeName)
  fileBow <- paste0("Output/normal_bow_n1/", output, "-bow-n1-wordcloud.png")
  fileBar <- paste0("Output/normal_bow_n1/", output, "-bow-n1-bar.png")
  
  reviews_words_by_company <- reviews_words %>% filter(company == companyName, store == storeName)
  
  corpus = VCorpus(VectorSource(reviews_words_by_company))
  corpus = tm_map(corpus, content_transformer(tolower)) 
  #corpus = tm_map(corpus, removeNumbers) 
  corpus = tm_map(corpus, content_transformer(rm_accent))
  corpus = tm_map(corpus, removeWords, stopwords_final)
  corpus = tm_map(corpus, stripWhitespace) 
  
  content <- sapply(corpus, as.character)
  df <- data.frame(text = content)
  names(df) <- c("id", "company", "store", "word")
  df <- df %>% filter(word != "")
  
  bow <- df %>% count(word, sort = TRUE)
  
  png(fileBow, width = 1500, height = 1500, res = 200)
  bow_wordcloud <- bow %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors="blue"))
  dev.off()
  
  graph_data <- head(bow, 15)
  graph_data <- graph_data %>%
    mutate(word = forcats::fct_reorder(word, desc(n))) 
  
  graph <- ggplot(graph_data, aes(x = n, y = forcats::fct_rev(forcats::fct_infreq(word)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label=n), vjust=0.5, hjust=1.2) + 
    labs(title = "Bag of words n1", x = "Frequência", y = "Palavra")
  
  ggsave(fileBar, plot = graph, width = 10, height = 6, units = "in")
}

#numeros[order(numeros, decreasing = TRUE)]

