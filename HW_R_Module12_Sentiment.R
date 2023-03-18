#HW_R Module 12 - Sentiment Analysis:

# dataset of Tweeets.csv is downloaded from the link below:

# https://www.kaggle.com/code/nursah/airline-tweets-sentiment-analysis/data

install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("scatterplot3d")
install.packages("scatterplot3d")
install.packages("scatterplot3d")

library(dplyr)
library(tidytext)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(tm)
options(warn=-1)

#Loading the dataset:

dataset <- read.csv('/Users/lana-n/datacsv/Tweets.csv')
str(dataset)

dataset$text <- as.character(dataset$text)
tidy_dataset <- dataset %>%
  unnest_tokens(word, text)

summary(dataset$airline_sentiment)

#Visualizing whether the sentiment of the tweets was positive, neutral, or negative for each airlines:

ggplot(dataset, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar() +
  facet_grid(. ~ airline) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.margin = unit(c(3,0,3,0), "cm"))

positive <- tidy_dataset %>% 
  filter(airline_sentiment == "positive")

list <- c("to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
          "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", "this",
          "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can")

positive <- positive %>%
  filter(!(word %in% list))

wordcloud(positive[,15],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(10, "Blues"))


positive <- positive %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

head(positive, 21)

positive <- positive %>%
  top_n(21)
colourCount = length(unique(positive$word))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# The Most 21 Frequent Words in Positive Tweets
positive %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col(fill = getPalette(colourCount)) +
  coord_flip()

#The Most Frequent Words in Negative Sentiment:

negative <- tidy_dataset %>% 
  filter(airline_sentiment == "negative") 

negative <- negative %>%
  filter(!(word %in% list))

wordcloud(negative[,15],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(10, "Reds"))

negative <- negative %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

negative <- negative %>%
  top_n(21)
colourCount = length(unique(negative$word))
getPalette = colorRampPalette(brewer.pal(8, "Dark2"))

# The Most 21 Frequent Words in Negative Tweets
negative %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col(fill = getPalette(colourCount)) +
  coord_flip()

# Intersection of positive and negative words:

intersect(negative$word, positive$word)

#Top words which included in only positive sentiment:

setdiff(positive$word, negative$word)

#Top words which included in only negative sentiment:

setdiff(negative$word, positive$word)

# What is the negative reason ?

dataset %>%
  filter(negativereason != "") %>%
  ggplot(aes(x = negativereason)) + 
  geom_bar(fill = "tomato") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# The Most Frequent Words in Neutral Sentiment:

neutral <- tidy_dataset %>% 
  filter(airline_sentiment == "neutral") 

neutral <- neutral %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

neutral <- neutral %>%
  filter(!(word %in% list))

head(neutral, 21)


neutral <- head(neutral, 21)

colourCount = length(unique(neutral$word))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))


# The Most 21 Frequent Words in Neutral Tweets
neutral %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col(fill = getPalette(colourCount)) +
  coord_flip()

#How many words for each sentiment ?:

totals <- tidy_dataset %>%
  # Count by tweet id to find the word totals for tweet
  count(tweet_id) %>%
  # Rename the new column
  rename(total_words = n) 


totals <- dataset %>%
  inner_join(totals, by = "tweet_id") %>%
  select(tweet_id, total_words, airline_sentiment) %>%
  arrange(desc(total_words))

totals <- head(totals, 20)

ggplot(totals, aes(x = airline_sentiment , y = total_words, fill = airline_sentiment)) +
  geom_col() +
  scale_fill_brewer(palette="Paired")

















