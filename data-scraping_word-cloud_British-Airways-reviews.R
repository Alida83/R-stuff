library(rvest)
library(dplyr)

####### British Airline reviews

link = "https://www.airlinequality.com/airline-reviews/british-airways/?sortby=post_date%3ADesc&pagesize=1000"
page = read_html(link)

title = page %>% html_nodes(".text_header") %>% html_text()
review = page %>% html_nodes(".text_content") %>% html_text()

print(title)
print(review)
BA = data.frame(title, review)
write.csv(BA, "BA.csv")

######### Tidying dataframe

install.packages("tidytext")
install.packages("wordcloud")
install.packages("ggplot2")
library(wordcloud)
library(ggplot2)
library(tidytext)
tidy_BA <- BA %>%
  unnest_tokens(output = word, input = review)
class(review)

tidy_BA

tidy_BA %>%
  count(word, sort = TRUE) ### counting words by frequency

positive <- get_sentiments("bing") %>% ### Counting positive words according to lexicon of Bing Liu
  filter(sentiment == "positive")

tidy_BA %>%
  semi_join(positive) %>%
  count(word, sort = TRUE)

negative <- get_sentiments("bing") %>% ### Counting positive words according to lexicon of Bing Liu
  filter(sentiment == "negative")

tidy_BA %>%
  semi_join(negative) %>%
  count(word, sort = TRUE)

tidy_BA %>%
  semi_join(positive) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20))

tidy_BA %>%
  semi_join(negative) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20))

install.packages("tidyr")
library(tidyr)
bing <- get_sentiments("bing")

bing_word_counts <- tidy_BA %>%
  inner_join(bing, relationship = "many-to-many") %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts

install.packages("reshape2")
library(reshape2)

tidy_BA %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

