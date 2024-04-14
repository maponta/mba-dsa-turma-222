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
  stringi
)

# 07-08-2023

# Sentando o diretório padrão, rode PWD no terminal
setwd("./R")
getwd()

rm_accent <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

store <- c("apple", "google")
company <- c("itau", "itau")
combinacoes <- data.frame(store, company)

custom_stopwords <- fromJSON("../Datasets/custom_stopwords_n2.json")
default_stopwords <- fromJSON("../Datasets/default_stopwords.json")
stopwords_final <- c(default_stopwords, custom_stopwords)

#TODO: change lem variables to normal
reviews_words <- read_xlsx("./Datasets/2023-total-volume-20000.xlsx")
reviews_words$id <- seq.int(nrow(reviews_words))
reviews_words <- reviews_words %>% select(id, company, store, content, at)
reviews_words <- reviews_words %>%
  mutate(store = tolower(store))
reviews_words$at <- as.POSIXct(reviews_words$at, format = "%Y-%m-%d %H:%M:%S")
reviews_words <- subset(reviews_words, as.Date(at) != as.Date("2023-08-07"))

for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  output <- paste0(companyName, "-", storeName)
  fileBow <- paste0("Output/normal_bow_n1_itau_not_07082023/", output, "-bow-n1-normal-wordcloud.png")
  fileBar <- paste0("Output/normal_bow_n1_itau_not_07082023/", output, "-bow-n1-normal-bar.png")
  
  reviews_words_by_company <- reviews_words %>% filter(company == companyName, store == storeName)
  
  tokens <- reviews_words_by_company %>%
    unnest_tokens(word, content)
  
  tokens <- tokens %>% select(id, company, store, word)
  
  #chartr("áàãâéèêíìóòõôúù", "aaaaeeeiioooouu", "olá")
  
  corpus = VCorpus(VectorSource(tokens$word))
  corpus = tm_map(corpus, content_transformer(tolower)) 
  #corpus = tm_map(corpus, removeNumbers) 
  #corpus = tm_map(corpus, removePunctuation) # Não está funcionando
  corpus = tm_map(corpus, content_transformer(rm_accent))
  corpus = tm_map(corpus, removeWords, stopwords_final)
  corpus = tm_map(corpus, stripWhitespace) 
  
  content <- sapply(corpus, as.character)
  df <- data.frame(text = content)
  names(df) <- c("word")
  df <- df %>% filter(word != "")
  
  bow <- df %>% count(word, sort = TRUE)
  
  png(fileBow, width = 1200, height = 1200, res = 200)
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
    labs(title = "Bag of words n1 normal", x = "Frequência", y = "Palavra")
  
  print(paste('ggplot generated', output))
  
  ggsave(fileBar, plot = graph, width = 10, height = 6, units = "in")
  
  print(paste(output, 'finished'))
  
}
