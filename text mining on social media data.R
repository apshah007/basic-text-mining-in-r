#************************Text mining with R************************

#install.packages("tidytext")
library(tidyverse)
library(tidytext) # Text mining
library(dplyr) # Data manipulation
library(tidyr) # Spread, separate, unite, text mining
library(ggplot2) #Visualization
library(textdata)


data <- read.csv("messages1.csv")
head(data)

select_column_content<- data %>% select(CONTENT)
select_column_content

tidy_d<- select_column_content %>%
  unnest_tokens(word, CONTENT )
tidy_d
#column name "word" is an unnested column of all words

# Remove stop words
without_stop_words <- tidy_d %>% anti_join(stop_words)

# Use dplyr count() function to find the most common words in the books:

numbered<- count(without_stop_words$word)


# The word are now stored in a tidy data frame, which allows us to pipe this directly to the ggplot2, 
# to create a visualization of the most common words for instance

numbered %>% filter(freq > 100) %>% ggplot(aes(reorder(x, freq), freq)) +
  geom_bar(stat = "identity") + coord_flip()

# Sentiment Analysis
sentiments
table(sentiments$lexicon)
get_sentiments("nrc")


# What are the most common joy words in  dickens books
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
################################################################################
#   I  am now here.  I am not able to understand the sentiment analyis code, but 
#  I successfully created a frequency table of the most common words used.
################################################################################




# bing
bing_data <- numbered %>%
  inner_join(get_sentiments("bing")) %>%
  count(x, sentiments) %>%
  ungroup()
bing_data

# Plot words and sentiments
bing_data %>%
  group_by(sentiment) %>% top_n(10) %>% ungroup() %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = " Words contribution to sentiment", x = NULL) +
  coord_flip()

# Word clouds
#install.packages("wordcloud")
library(wordcloud)

numbered %>%
  count(numbered$x) %>%
  with(wordcloud(x, n, max.words = 100))


tidy_dickens %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Sentiment comparison word cloud
#install.packages("reshape2")
library(reshape2)
tidy_dickens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green4"),
                   max.words = 100)

# Relationships between words - bigrams
bigrams_dickens <- dickens %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_dickens

# Remove stop words
bigrams_dickens_separated <- bigrams_dickens %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_dickens_filtered <- bigrams_dickens_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigrams_dickens_filtered %>% 
  count(word1, word2, sort = TRUE)

# And then unite() the two words
bigrams_dickens_united <- bigrams_dickens_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_dickens_united

# Plot
bigrams_dickens_united %>%  count(bigram, sort = TRUE) %>% top_n(20) %>% ggplot(aes(reorder(bigram, n), n)) +
  geom_bar(stat = "identity") +  coord_flip()

# TF-IDF
GreatExpec <- gutenberg_download(1400)


GreatExpec <- GreatExpec %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, "^Chapter[\\s][IVXL]"))) 

GreatExpec <- GreatExpec %>% unnest_tokens(word, text)

# Remove stop words
GreatExpec <- GreatExpec %>% anti_join(stop_words)

# Show the word frequency of great expectation
GreatExpec <- GreatExpec %>% count(chapter, word, sort = TRUE)

# tf-idf
chapter_words <- GreatExpec %>%
  bind_tf_idf(word, chapter, n)

chapter_words

# Arrange
chapter_words %>%
  arrange((tf_idf))

# Visualize

chapter_words %>%
  filter(chapter %in% c(1,2,3,4,5,6)) %>%
  arrange(-tf_idf) %>%
  group_by(chapter) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = chapter)) +
  geom_bar(stat = "identity") +
  facet_wrap(~chapter, scales = "free") +
  coord_flip()
