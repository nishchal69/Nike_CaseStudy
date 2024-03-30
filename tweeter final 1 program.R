getwd()
setwd("C:/Users/nishc/Desktop/web social media nike case study/finally csv dataset")
library(tidyverse)
library(syuzhet)
library(ggplot2)
library(tm)
library(stringi)
library(tidytext)

df <- read_csv("justdoit_tweets_5000.csv")
head(df$tweet_full_text)

tweets.df2 <- gsub("http.*","",df$tweet_full_text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- stri_replace_all_regex(tweets.df2, "@\\w+", "")
tweets.df2 <- stri_replace_all_regex(tweets.df2, "#\\w+", "")
tweets.df2 <-str_remove_all(tweets.df2,"&amp")
tweets.df2 <- gsub("!.*","",tweets.df2)
tweets.df2 <- gsub("\n.*","",tweets.df2)

print(head(tweets.df2))

word.df <- as.vector(tweets.df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2,emotion.df)
head(emotion.df2)

sentiment_totals <- colSums(emotion.df2[, c("sadness", "surprise", "trust", "negative", "positive",
                                            "anger", "anticipation", "disgust", "fear", "joy")])

sentiment_df <- data.frame(sentiment = names(sentiment_totals), count = sentiment_totals)

ggplot(sentiment_df, aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Sentiment", y = "Count", title = "Sentiment Counts Across All Tweets") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() 

sentiment.value <- get_sentiment(word.df)
most.positive <- word.df[sentiment.value == max(sentiment.value)]
most.positive

sentiment.value <- get_sentiment(word.df)
most.negative <- word.df[sentiment.value == min(sentiment.value)]
most.negative

positive.tweets <- word.df[sentiment.value > 0]
head(positive.tweets)
negative.tweets <- word.df[sentiment.value < 0 ]
head(negative.tweets)
neutral.tweets <-word.df[sentiment.value == 0]
head(neutral.tweets)

category_Sentiment <- ifelse(sentiment.value < 0, "Negative",
                             ifelse(sentiment.value > 0,"Positive","Neutral"))
table(category_Sentiment)

visual_df <- data.frame(
  Sentiment = c("Negative", "Neutral", "Positive"),
  Number_of_Tweets = c(1123, 1640, 2326)
)



ggplot(visual_df, aes(x = Sentiment, y = Number_of_Tweets, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "Sentiment", y = "Number of Tweets", fill = "Sentiment") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Sentiment Analysis of Tweets")

tweets <- data.frame(text = tweets.df2)

tweets_words <- tweets %>%
  unnest_tokens(word, text)

data(stop_words)
tweets_words <- tweets_words %>%
  anti_join(stop_words)

word_counts <- tweets_words %>%
  count(word, sort = TRUE)

top_n <- 10
word_counts_top_n <- head(word_counts, top_n)

ggplot(word_counts_top_n, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +  
  theme_minimal() +
  ggtitle("Most Common Words in Tweets")

