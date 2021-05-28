#************************Text mining with R************************

#install.packages("tidytext")
library(tidyverse)
library(tidytext) # Text mining
library(dplyr) # Data manipulation
library(tidyr) # Spread, separate, unite, text mining
library(ggplot2) #Visualization
#install.packages("gutenbergr")
library(gutenbergr)
#library(textdata)

# Downloading Dickens’ five novels by Project Gutenberg ID numbers :

dickens <- gutenberg_download(c(98, 1400, 46, 730, 786))
dickens
# unnest_tokens() function: Create tidy data which breaks out the book into individual words 
# one word per row.
tidy_dickens <- dickens %>% unnest_tokens(word, text)
tidy_dickens

# Remove stop words
tidy_dickens <- tidy_dickens %>% anti_join(stop_words)

# Use dplyr count() function to find the most common words in the books:

tidy_dickens %>% count(word, sort = TRUE) 

#OR

tidy_dickens %>% group_by(word) %>% summarise(n = n()) %>% arrange(-n)

# The word are now stored in a tidy data frame, which allows us to pipe this directly to the ggplot2, 
# to create a visualization of the most common words for instance

tidy_dickens %>%  count(word, sort = TRUE) %>% filter(n > 600) %>% ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") + coord_flip()

# Sentiment Analysis
sentiments
table(sentiments$lexicon)
get_sentiments("nrc")


# What are the most common joy words in  dickens books
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# bing
bing_tidy_dickens <- tidy_dickens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_tidy_dickens

# Plot words and sentiments
bing_tidy_dickens %>%
  group_by(sentiment) %>% top_n(10) %>% ungroup() %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = " Words contribution to sentiment", x = NULL) +
  coord_flip()

# Word clouds
#install.packages("wordcloud")
library(wordcloud)

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

