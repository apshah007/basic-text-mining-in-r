#************************Text mining with R************************

#install.packages("tidytext")
library(tidyverse)
library(tidytext) # Text mining
library(dplyr) # Data manipulation
library(tidyr) # Spread, separate, unite, text mining
library(ggplot2) #Visualization
library(textdata)
library(wordcloud)
library(reshape2)


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

#library(wordcloud)
# this word cloud works 

wordcloud(words = numbered$x, freq = numbered$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,)

  
colnames(numbered)[1] = "word"
  
# Sentiment Analysis
#sentiments
#table(sentiments$lexicon)
#get_sentiments("nrc")


# What are the most common joy words in  dickens books
#nrc_joy <- get_sentiments("nrc") %>% 
#  filter(sentiment == "joy")
################################################################################
#   I  am now here.  I am not able to understand the sentiment analyis code, but 
#  I successfully created a frequency table of the most common words used.
################################################################################


nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")


innerjtables <- merge(numbered, nrc_joy, by = "word")

counted_innerjtable<- count(innerjtables, "word")


#bar chart of joy words 
counted_innerjtable %>% filter(freq > 20) %>% ggplot(aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity") + coord_flip()


#word cloud of joy words
wordcloud(words = counted_innerjtable$word, freq = counted_innerjtable$freq, min.freq = 1,
          max.words=250, random.order=FALSE, rot.per=0.35,)


bingsent<-get_sentiments("bing")
mergedtobing <- merge(bingsent, numbered, by = "word")
sentiment_breakdown<- count(mergedtobing$sentiment)


analyze_sentiment<- mergedtobing %>% 
  group_by(word) %>%
  spread(sentiment, freq, fill = 0) %>%
  mutate(sentimentchange = positive - negative)


#nonworking plot for sentiment analysis 
ggplot(analyze_sentiment, aes(x = analyze_sentiment$positive, 
                              y = analyze_sentiment$sentimentchange))
 



# Sentiment comparison word cloud
#install.packages("reshape2")
#library(reshape2)

#working sentiment analysis word cloud 

mergedtobing %>%
  acast(word ~ sentiment, value.var = "freq", fill = 0) %>%
  comparison.cloud(colors = c("red", "green4"),
                   max.words = 100)

