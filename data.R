require(academictwitteR)
require(tidyverse)

read_csv("stoxx_names_ticker.csv", skip = 1)


tweets <- get_all_tweets(
            query = "$BAYRY",
            start_tweets = "2015-01-01T00:00:00Z",
            end_tweets = "2020-02-01T00:00:00Z",
            #exact_phrase = T,
            is_retweet = F,
            is_reply = F,
            is_quote = F,
            lang = c("en"),
            has_cashtag = T,
            n = 100
           )

tweets$created_at
tweets$text
tweets$lang
tweets %>% view()
tweets$entities$cashtags
