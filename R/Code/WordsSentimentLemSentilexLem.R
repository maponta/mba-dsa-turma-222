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

store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google", "all", "all", "all")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all", "nubank", "itau", "bb")
combinacoes <- data.frame(store, company)

lexicon <- sentiLex_lem_PT02
#glimpse(sentiLex_lem_PT02)

stopwords_ptbr <- stopwords("pt")
custom_stopwords <- fromJSON("../Datasets/custom_stopwords.json")
stopwords_final <- c(stopwords_ptbr, custom_stopwords)

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
  output <- paste0(companyName, "-", storeName)
  fileWordcloudPositive <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-postive-wordcloud.png")
  fileWordcloudNeutral <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-neutral-wordcloud.png")
  fileWordcloudNegative <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-negative-wordcloud.png")
  fileWordcloudGeral <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-geral-wordcloud.png")
  fileBarPositive <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-postive-graph-bar.png")
  fileBarNeutral <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-neutral-graph-bar.png")
  fileBarNegative <- paste0("Output/lem_sentiment_sentiLex_lem/", output, "-negative-graph-bar.png")
  
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
    unnest_tokens(word, word, token = "ngrams", n = 1)
  
  #names(df) <- c("word")
  df <- df %>% filter(word != "")
  
  text_sentiment <-
    inner_join(df, lexicon, by = c("word" = "term")) %>%
    count(word, grammar_category, polarity, sort = TRUE)
  
  text_sentiment_negative <- text_sentiment %>% 
    filter(polarity == -1)
  text_sentiment_neutral <- text_sentiment %>% 
    filter(polarity == 0)
  text_sentiment_positive <- text_sentiment %>% 
    filter(polarity == 1)
  
  png(fileWordcloudPositive, width = 1500, height = 1500, res = 200)
  positive_wordcloud <- text_sentiment_positive %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors="blue"))
  dev.off()
  
  png(fileWordcloudNeutral, width = 1500, height = 1500, res = 200)
  neutral_wordcloud <- text_sentiment_neutral %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors="blue"))
  dev.off()
  
  png(fileWordcloudNegative, width = 1500, height = 1500, res = 200)
  negative_wordcloud <- text_sentiment_negative %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors="blue"))
  dev.off()
  
  all_words <- rbind(head(text_sentiment_positive,25), head(text_sentiment_negative,25))
  colors <- ifelse(all_words$polarity == 1, "blue", "red")
  all_words$colors <- colors
    
  png(fileWordcloudGeral, width = 1500, height = 1500, res = 200)
  geral_wordcloud <- all_words %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=colors, ordered.colors=TRUE))
  dev.off()
  
  positive_graph_data <- head(text_sentiment_popositive_graph_data <- head(text_sentiment_popositive_graph_data <- head(text_sentiment_positive, 15)))
  positive_graph_data <- positive_graph_data %>%
    mutate(word = forcats::fct_reorder(word, desc(n))) 
  
  positive_graph <- ggplot(positive_graph_data, aes(x = n, y = forcats::fct_rev(forcats::fct_infreq(word)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label=n), vjust=0.5, hjust=1.2) + 
    labs(title = "Bag of words n1 lem", x = "Frequência", y = "Palavra")
  
  print(paste('ggplot positive generated', output))
  
  ggsave(fileBarPositive, plot = positive_graph, width = 10, height = 6, units = "in")
  
  neutral_graph_data <- head(text_sentiment_neutral, 15)
  neutral_graph_data <- neutral_graph_data %>%
    mutate(word = forcats::fct_reorder(word, desc(n))) 
  
  neutral_graph <- ggplot(neutral_graph_data, aes(x = n, y = forcats::fct_rev(forcats::fct_infreq(word)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label=n), vjust=0.5, hjust=1.2) + 
    labs(title = "Bag of words n1 lem", x = "Frequência", y = "Palavra")
  
  print(paste('ggplot neutral generated', output))
  
  ggsave(fileBarNeutral, plot = neutral_graph, width = 10, height = 6, units = "in")
  
  negative_graph_data <- head(text_sentiment_negative, 15)
  negative_graph_data <- negative_graph_data %>%
    mutate(word = forcats::fct_reorder(word, desc(n))) 
  
  negative_graph <- ggplot(negative_graph_data, aes(x = n, y = forcats::fct_rev(forcats::fct_infreq(word)))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label=n), vjust=0.5, hjust=1.2) + 
    labs(title = "Bag of words n1 lem", x = "Frequência", y = "Palavra")
  
  print(paste('ggplot negative generated', output))
  
  ggsave(fileBarNegative, plot = negative_graph, width = 10, height = 6, units = "in")
  
  
  print(paste(output, ' finished'))
}
