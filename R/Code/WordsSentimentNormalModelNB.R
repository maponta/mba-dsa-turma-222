# Setando diretório padrão - PWD
setwd("./R")
getwd()

# Carregando pacotes
library("pacman")

pacman::p_load(
  readxl,
  tidytext,
  tm,
  wordcloud,
  stringr,
  e1071,
  gmodels,
  SnowballC,
  caret,
  ggplot2,
  widyr,
  igraph,
  ggraph,
  lexiconPT,
  tidyr,
  textstem,
  dplyr,
  jsonlite
)

# Carregando o Modelo NB generic reviews
model_nb <- readRDS("Output/models/nb_generic_reviews_v1.0.rds")
custom_stopwords <- fromJSON("../Datasets/default_stopwords.json")
# Carregando os comentários normais
reviews_words_base <- read_xlsx("./Datasets/2023-total-volume-20000.xlsx")
reviews_words <- transform(reviews_words_base, company = "all") %>%
  rbind(reviews_words_base)
reviews_words <- transform(reviews_words_base, store = "all") %>%
  rbind(reviews_words)
reviews_words$id <- seq.int(nrow(reviews_words))
reviews_words <- reviews_words %>% select(id, company, store, content, at, score)
reviews_words <- reviews_words %>%
  mutate(store = tolower(store))

rm_accent <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

store <- c("apple", "google", "apple", "google", "apple", "google", "apple", "google")
company <- c("nubank", "nubank", "itau", "itau", "bb", "bb", "all", "all")
combinacoes <- data.frame(store, company)

for (i in 1:nrow(combinacoes)) {
  companyName <- combinacoes$company[i]
  storeName <- combinacoes$store[i]
  #companyName <- "itau"
  #storeName <- "apple"
  output <- paste0(companyName, "-", storeName)
  fileBowSentiment <- paste0("Output/normal_sentiment_model_nb/", output, "-bow-n1-sentiment.png")
  fileBowScore <- paste0("Output/normal_sentiment_model_nb/", output, "-bow-n1-score.png")
  fileCount <- paste0("Output/normal_sentiment_model_nb/", output, "-count-score.csv")
  filePositive <- paste0("Output/normal_sentiment_model_nb/", output, "-positive-score.csv")
  fileNegative <- paste0("Output/normal_sentiment_model_nb/", output, "-negative-score.csv")
  fileCsv <- paste0("Output/normal_sentiment_model_nb/", output, "-sentiment.csv")
  
  reviews_words_by_company <- reviews_words %>% 
    filter(company == companyName, store == storeName)
  
  print(paste(output, ' starting train set'))
  
  #Train set
  corpus = VCorpus(VectorSource(reviews_words_by_company$content)) 
  corpus = tm_map(corpus, content_transformer(tolower)) 
  corpus = tm_map(corpus, removeNumbers) 
  corpus = tm_map(corpus, removePunctuation) 
  corpus = tm_map(corpus, removeWords, custom_stopwords) 
  corpus = tm_map(corpus, stemDocument) 
  corpus = tm_map(corpus, stripWhitespace) 
  as.character(corpus[[1]])
  
  content <- sapply(corpus, as.character)
  df <- data.frame(text = content)
  names(df) <- c("word")
  
  print(paste(output, ' train set finished'))
  
  dtm = DocumentTermMatrix(corpus) 
  inspect(dtm) 
  dim(dtm) 
  dtm = removeSparseTerms(dtm, 0.999) 
  inspect(dtm)
  
  convert <- function(x) {
    y <- ifelse(x > 0, 1,0)
    y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
    y
  }  
      
  datanaive = apply(dtm, 2, convert)
  dataset = as.data.frame(as.matrix(datanaive))    
  
  nb_pred_dataset <- predict(model_nb, type = 'class', newdata = dataset)
  print(nb_pred_dataset)
  
  result <- reviews_words_by_company
  result$sentimento <- nb_pred_dataset
  result$usedText <- df$word
  
  resultPositiveAnalise <- result %>% 
    filter(sentimento == 2) %>%
    group_by(score) %>%
    count(score, sort = TRUE)
  
  resultNegativeAnalise <- result %>% 
    filter(sentimento == 0) %>%
    group_by(score) %>%
    count(score, sort = TRUE)
  
  print(paste(output, ' predicted class added'))
  
  predictedCount <- result %>% count(sentimento, sort = TRUE)
  colnames(predictedCount) <- c("sentimento","comentários")
  predictedCount$sentimento <- gsub("0", "Negativo", predictedCount$sentimento)
  predictedCount$sentimento <- gsub("2", "Positivo", predictedCount$sentimento)
  
  write.csv(result, file = fileCsv, row.names = FALSE)
  write.csv(predictedCount, file = fileCount, row.names = FALSE)
  write.csv(resultPositiveAnalise, file = filePositive, row.names = FALSE)
  write.csv(resultNegativeAnalise, file = fileNegative, row.names = FALSE)
  
  print(paste(output, ' csv files genereted'))
  
  bow <- result %>%
    unnest_tokens(word, usedText, token = "ngrams", n = 1) %>%
    count(sentimento, word, sort = TRUE)
  
  bowPositive <- bow %>%
    filter(sentimento == 2)
  
  bowNegative <- bow %>%
    filter(sentimento == 0)
  
  all_words <- rbind(head(bowPositive,25), head(bowNegative,25))
  colors <- ifelse(all_words$sentimento == 2, "blue", "red")
  all_words$colors <- colors
  
  png(fileBowSentiment, width = 1500, height = 1500, res = 200)
  geral_wordcloud <- all_words %>% 
    with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=colors, ordered.colors=TRUE))
  dev.off()
  
  print(paste(output, ' wordcloud files genereted'))
  
  print(paste(output, ' finish'))
  #coresNegative <- gray.colors(length(resultNegativeAnalise$score))
  #png(filePieNegative, width = 600, height = 600, res = 200)
    #pie(resultNegativeAnalise$n, labels = resultNegativeAnalise$score, col = coresNegative)
   # legend("topright", legend = resultNegativeAnalise$n, title = "Valores", cex = 0.8, fill = coresNegative)
  #dev.off()
}