library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
library("widyr")
library("ggplot2")
library("igraph")
library("ggraph")
library("lexiconPT")
library("stringr")
library("tidyr")
library("tm")
library("wordcloud")
library("tidyverse")

# Sentando o diretório padrão, rode PWD no terminal
setwd("/Users/gbernini/Projects/MarcelaTCC/R")
getwd()

# Importe o arquivo .txt como uma tabela
#dados <- read.table("./Datasets/SentiLex-flex-PT02.txt", header = FALSE, sep = ";", quote = "", comment.char = "", col.names = c("term", "grammar_category", "polarity", "polarity_target", "polarity_classification"))

sentilex_flex <- read.table("./Datasets/SentiLex-flex-PT02.txt", sep = ".", stringsAsFactors = FALSE)
sentilex_flex <- sentilex_flex %>%
  mutate(ocorrencias_ponto_virgula = nchar(gsub("[^;]", "", V2))) %>%
  filter(ocorrencias_ponto_virgula == 4)
sentilex_flex <- separate(sentilex_flex, V2, into = c("pos", "flex", "tg", "pol", "anot"), sep = ";")
sentilex_flex$pol <- gsub("[^-0-9]", "", sentilex_flex$pol)
sentilex_flex$pol <- substr(sentilex_flex$pol, 2, nchar(sentilex_flex$pol))
sentilex_flex$flex <- substr(sentilex_flex$flex, 6, nchar(sentilex_flex$flex))
sentilex_flex$pos <- substr(sentilex_flex$pos, 5, nchar(sentilex_flex$pos))
sentilex_flex$anot <- substr(sentilex_flex$anot, 6, nchar(sentilex_flex$anot))
sentilex_flex <- sentilex_flex %>% filter(pos != "IDIOM")
sentilex_flex <- sentilex_flex %>%
  separate_rows(V1, sep = ",")

matriz_cor <- cor(mtcars)
corrplot(matriz_cor, method = "color", type = "upper", order = "hclust", addrect = 2)


#bibliometrix::biblioshiny()






#####################




library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
library("widyr")
library("ggplot2")
library("igraph")
library("ggraph")

# Sentando o diretório padrão, rode PWD no terminal
setwd("/Users/gbernini/Projects/MarcelaTCC/R")
getwd()

reviews_words <- read.csv("./Datasets/total-words-20000.csv")
#reviews_words_sample <- head(reviews_words, 100000)
reviews_words_sample <- reviews_words

word_pairs_sample <- reviews_words_sample %>%
  pairwise_count(word, id, sort = TRUE)

word_cors_sample <- reviews_words_sample %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, id, sort = TRUE)

word_cors_filtered <- word_cors_sample %>% filter(correlation > 0.5)

# Gráfico de dispersão

ggplot(word_cors_filtered, aes(x = item1, y = item2, color = correlation)) +
  geom_point() +
  labs(x = "item1", y = "item2", color = "correlation") +
  theme_minimal()

# Gráfico network

graph <- graph_from_data_frame(word_cors_filtered, directed = FALSE, vertices = unique(c(word_cors_filtered$item1, word_cors_filtered$item2)))

#E(graph)$correlation <- word_cors_filtered$correlation

#ggraph(graph, layout = "fr") +
#  geom_edge_link(aes(color = correlation), arrow = arrow(length = unit(0.2, "cm")), alpha = 0.7) +
#  geom_node_point() +
#  geom_node_text(aes(label = name), repel = TRUE) +  # Adiciona as palavras dos itens
#  labs(x = "item1", y = "item2", color = "correlation") +
#  theme_minimal()

#set.seed(123)

word_cors_filtered %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#word_pairs_company <- reviews_words %>%
# pairwise_count(word, company, sort = TRUE)

#word_cors_company <- reviews_words %>%
#  group_by(word) %>%
#  filter(n() >= 20) %>%
#  pairwise_cor(word, company, sort = TRUE)


###########################################

# Topic Modeling


