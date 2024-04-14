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

clean_ngram <- function(string) {
  string <- gsub("'", "", string)
  string <- gsub("`", "", string)
  string <- gsub("\n", "", string)
  string <- trimws(string)
  return(string)
}

trim_ngram <- function(string) {
  string <- trimws(string)
  return(string)
}

store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google", "all", "all", "all")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all", "itau", "bb", "nubank")
combinacoes <- data.frame(store, company)

custom_stopwords <- fromJSON("../Datasets/custom_stopwords_n2.json")
default_stopwords <- fromJSON("../Datasets/default_stopwords.json")
stopwords_final <- c(default_stopwords, custom_stopwords)

reviews_words_lem_base <- read_xlsx("./Datasets/total-words-20000-lemmatizated.xlsx")
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
  fileBow <- paste0("Output/lem_bow_n2/", output, "-bow-n2-lem-wordcloud.png")
  fileBar <- paste0("Output/lem_bow_n2/", output, "-bow-n2-lem-bar.png")
  
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
    unnest_tokens(word, word, token = "ngrams", n = 2)
  
  #names(df) <- c("word")
  df <- df %>% filter(word != "")
  
  bow <- df %>% count(word, sort = TRUE)
  
  png(fileBow, width = 1500, height = 1500, res = 200)
  bow_wordcloud <- bow %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors="blue"))
  dev.off()
  
  print(paste('Wordcloud generated', output))
  
  graph_data <- head(bow, 15)
  graph_data <- graph_data %>%
    mutate(word = forcats::fct_reorder(word, desc(n))) 
  
  graph <- ggplot(graph_data, aes(x = n, y = forcats::fct_rev(forcats::fct_infreq(word)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label=n), vjust=0.5, hjust=1.2) + 
    labs(title = "Bag of words n1 lem", x = "Frequência", y = "Palavra")
  
  print(paste('ggplot generated', output))
  
  ggsave(fileBar, plot = graph, width = 10, height = 6, units = "in")
  
  print(paste(output, 'finished'))
}
