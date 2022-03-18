require(academictwitteR)
require(tidyverse)

tweets <- get_all_tweets(
             query = "$BUDFF",
             start_tweets = "2015-01-01T00:00:00Z",
             end_tweets = "2020-02-01T00:00:00Z",
             exact_phrase = T,
            is_retweet = F,
            is_reply = F,
            is_quote = F,
             n = 10
           )

tweets$created_at
tweets$text

tweets %>% view()
