library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
library(tidyverse)
library(stringr)
library(wordcloud2)
library(tidyr)
library(academictwitteR)


set_bearer()

api_key <- "rqJBjWXFsNVURdEknMALujxfU"
api_secret <- "OTwPaCWDWkiHuaxHUbN1byjvQpQles3JO7IX6KxFZxONnl39am"
access_key <-"1467560904665579522-YVmG06Rr5SLlLDczxCMTodLw8JQe5g" 
access_secret <- "dciOsAtvXKLbidZXoSKBOBmZhjtOQb0JnKGPZ43Y5zHeI"

## authentication via web browser
token <- create_token(
  app = "ESG Sentiment in Factor", 
  consumer_key = api_key, 
  consumer_secret = api_secret,
  access_token = access_key,
  access_secret = access_secret)

# tweets <-
#   get_all_tweets(
#     query = c("\"adidas ag\"", "addyy", "adidas"),
#     start_tweets = "2020-01-01T00:00:00Z",
#     end_tweets = "2020-02-01T00:00:00Z",
#     file = "adidas_ag",
#     data_path = "data/",
#     n = 10000,
#     lang = "en"
#   )

words <- "Social, end poverty, poverty dimension, poverty, social protection, poor, unemployed 
person, poverty line, protection, cash benet, extreme poverty, poor vulnerable, 
humanitarian, vulnerable, 
Malnutrition, hunger, food producer, underweight, hunger malnutrition, undernutrition, famine,
 food insecurity, agricultural productivity, agricultural, extreme hunger, agriculture, prevalence
 undernourishment, nutritional need, food, 
Life expectancy, mental health, air pollution, medicine vaccine, infectious disease, good health,
 respiratory disease, reproductive health, mortality, healthcare, disease diabetes, disease,
 health coverage, health, maternal mortality, death preventable, cardiovascular disease
Teacher, secondary school, prociency level, primary school, inclusive, literacy, literacy
 numeracy, higher education, quality education, school, effective learn, vocational train, level
 prociency, minimum prociency, technical vocational, 
Domestic work, right, sexual violence, woman, girl, discrimination, reproductive health,
 managerial position, woman girl, marriage, woman representation, gender equality, gender
 parity, child marriage, gender, 
Water sanitation, drink water, sanitation, basic drink, wastewater, water scarcity, hygiene,
 water, sanitation service, sanitation hygiene, supply freshwater, hand wash facility, resource
 management, water stress, 
Energy, technology, renewable energy, infrastructure, electricity, cheap energy, solar, solar
 power, wind power, thermal power, energy productivity, energy efciency, greenhouse gases,
 greenhouse, fossil fuels, pollution, energy standards, energy access, energy consumption,
 access electricity, without electricity, fuel technology, fossil fuel
Labor, employment, gdp, job, unemployed, economic growth, productivity, job creation,
 slavery, forced labor, labor force, women participation, labor organization, human right,
 informal employment, growth rate, labor productivity, decent work, secure work, global
 economic, gender pay, crisis level, rate real, decent work, education employment, slavery
 human, child labor, youth employment, 
Research development, development, industry, infrastructure, transport, technological
 progress, communication technology, sustainable development, sustainable industries,
 innovation, entrepreneurship, access information, access internet, material footprint,
 develop country, least develop, economic infrastructure, infrastructure support, global
 manufacture, scientic research, resilient infrastructure, research innovation
Income, population, income inequality, economic inclusion, safe migration, economic
 inequality, reduce inequality, wealth share, indigenous rights, migrant worker, migrant,
 ofcial development, income inequality, migration mobility, global wealth, least develop,
 development assistance, 
City, urban, urban population, public, disability, disaster, sustainable city, affordable
 housing, housing access, resilient societies, public transport, public spaces, urban
 planning, inclusive, business opportunities, sustainable development, person disability,
 green public, sustainable resilient, sustainable urbanization, population convenient,
 convenient access, 
Responsible consumption, sustainable development, resources, consumption, production,
 development, reduce waste, efcient, efcient economy, energy consumption, energy
 efcient, supporting developing, material footprint, natural resource, recycle, sustainable
 consumption, domestic material, consumption production, food waste, 
Climate, develop, disaster, local, emissions reductions, global warm, climate change, climate
 system, greenhouse gas, emissions, co2 emissions, low carbon, disaster risk, sustainable
 management, natural resource, sea levels, sustainable energy, Paris Agreement, sustainable
 energy, climate relate, green climate, disaster risk, 
Marine, ocean, fish, sea, water, shery, overshing, coastal biodiversity, coastal ecosystems,
 ocean resources, marine biodiversity, fish stocks, marine pollution, ocean acidication,
 depleted sheries, unregulated fish, fish stock, shery management, marine coastal, shery
 subsidy, conservation sustainable, marine technology, 
Land degradation, terrestrial freshwater, ecosystem, deforestation, species animal, forest
 management, biodiversity, conservation, protect area, forest, terrestrial, species, wildlife,
 protect, agriculture, land, area, 
Law, human right, right violation, insecurity, institution, violence, exploitation, global
 governance, transparency, rule, corruption bribery, access justice, corruption, justice, peace,
 conflict violence, international, victim human, conflict, 
Sustainable development, innovation, enhance international, ofcial development, capacity
 build, development, coordinate policy, develop country, assistance, cooperation, international
 support, international cooperation, partnership, international, policy coherence, development
 assistance, country"

tweets <- bind_tweets(data_path = "data/")
words <- gsub("[\r\n]", "", words)
words <- str_split(words, ", ")
tweets <- as.data.frame(tweets)

tweets <- tweets[str_detect(tweets$text, words[[1]]),]

tweets$text[6]


BTC <- search_tweets(
  "$BTC", n = 10000, include_rts = FALSE, token = token
)

min(BTC$created_at)
max(BTC$created_at)

summary(BTC)

colnames(BTC)


BTC %>% group_by(user_id) %>% summarise(n())
BTC %>% select(status_id) %>% head()
BTC %>% select(display_text_width) %>% head()

BTC %>% pull(display_text_width) %>% hist()
BTC %>% select(source) %>%group_by(source) %>% summarize(n = n()) %>% arrange(desc(n))

BTC_symbols <- BTC %>% pull(symbols) 
BTC_symbols <- lapply(BTC_symbols, tolower)




BTC_symbols <-unlist(BTC_symbols)

BTC_symbols %>% as_tibble() %>% group_by(value) %>% summarise(n = n()) %>% %>% head() 

BTC_text <- unlist(BTC$text)



BTC_text <- tibble(line = 1:length(BTC_text), text = BTC_text)

url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

BTC_text <- BTC_text %>% mutate(text = str_remove_all(text, url_regex))

BTC_tokens <- BTC_text %>%unnest_tokens(word, text)


data(stop_words)

BTC_tokens <- BTC_tokens %>%
  anti_join(stop_words)


BTC_tokens %>% count(word, sort = T) 

BTC_tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

BTC_tokens %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE)


BTC_tokens %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

BTC_tokens %>%
  inner_join(afinn) %>%
  count(word, value, sort = TRUE)


BTC_tokens %>%
  inner_join(afinn) %>% group_by(line) %>% summarise(sum = sum(value)) %>%  arrange(sum)

# best
BTC$text[[7074]]

BTC_tokens %>% filter(line == 7074) %>%
  inner_join(afinn)

# worst
BTC$text[[8192]]

BTC_tokens %>% filter(line == 8192) %>%
  inner_join(afinn)


BTC_senti <- BTC_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(line, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(BTC_senti, aes(sentiment, sentiment)) +
  geom_col(show.legend = FALSE) 


BTC_tokens %>% filter(nchar(word) > 3) %>% 
  count(word) %>% arrange(desc(n)) %>%  inner_join(afinn) %>% select(-value) %>%  
  wordcloud2(color = (BTC_tokens %>% filter(nchar(word) > 3) %>% 
               count(word) %>%   inner_join(afinn))$value)


BTC_tokens %>% inner_join(get_sentiments("loughran")) %>% group_by(sentiment) %>% summarise(n = n())

BTC_tokens %>% inner_join(get_sentiments("loughran")) %>% 
  filter(sentiment  == "negative") %>% 
  group_by(line) %>% 
  count(line) %>% arrange(desc(n))


BTC$text[[2096]]


