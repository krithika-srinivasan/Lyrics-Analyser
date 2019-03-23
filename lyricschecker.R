library(streamR)
library(ROAuth)
library(tm)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(igraph)
library(ggraph)
library(qdap)

# tweets <- tweets_df$text
tweets <- read.csv(file = "C:/Users/GLaDOS/Documents/arianalyrics.txt", sep = "/")

tweets <- tibble(tweets)
#Make a corpus
tweetcorpus <- Corpus(VectorSource(tweets))
tweetcorpus

#Clean the corpus
tweetcorpus <- tm_map(tweetcorpus, stripWhitespace)
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, removeNumbers)
tweetcorpus <- tm_map(tweetcorpus, tolower)
tweetcorpus <- tm_map(tweetcorpus, function(x) iconv(enc2utf8(x), sub = "byte"))

#Make a document term matrix
tweetdtm <- DocumentTermMatrix(tweetcorpus)
tweetdtm


tweets = tidy(tweets)

#Replace 'term with word'
tidytweet <- tweets %>%
  unnest_tokens(word, tweets$Step.up.the.two.of.us $nobody.knows.us , token = "ngrams", n = 1)


#Remove stopwords
tidytweet <- tidytweet %>%
  anti_join(stop_words)
tidytweet


#Word frequency
# tidytweet %>%
#   count(word, sort = TRUE) %>%
#   top_n(20) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()

tidytweet %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))

#Sentiment Analysis

#Bing
tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#NRC
tidytweet %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Sentiment Scores
tweetsent <- tidytweet %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, score, sort = TRUE) %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
ggplot(tweetsent,aes(word, n, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(y = paste0("Average sentiment: ", mean(tweetsent$score)),
       x = NULL) +
  coord_flip()

mean(tweetsent$score)

#Comparison Wordcloud
tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#  #Word correlation
# tweetgraph <- findAssocs(tweetdtm, "hbo", 0.5)[1]
# tweetgraph

#Bigrams
tweetbi <- tidytweet %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2)
tweetbi

##Bigram Frequency
tweetbi %>%
  count(bigram, sort = TRUE) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Seperate bigrams
tweetbisep <- tweetbi %>%
  separate(bigram, c("word1", "word2"), sep = " ")
tweetbisep

#Bigram graph
tweetgraph <- tweetbisep %>%
  count(word1,word2, sort = TRUE) %>%
  filter(n > 5) %>%
  graph_from_data_frame()

tweetgraph

set.seed(2016)
ggraph(tweetgraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Sentence search
tweetsent <- tweets
#Install 64-bit ver of Java first
