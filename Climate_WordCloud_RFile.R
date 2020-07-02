## Word Cloud: Climate and COVID-19.

## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools") }

library(rtweet)
create_token(
  app = "TwitterData_Analysis2020",
  consumer_key = '####################',
  consumer_secret =  '###############################################',
  access_token = '#################################################',
  access_secret = '###########################################')

library(wordcloud)
library(tidyverse)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tm)

stream_tweets(
  q = "climate, global warming, environment, carbondioxide, nitrogen, dioxide, greenhouse gases, anomaly, weather, climate change, polar vortex, earth, nature, CO2, ozone, humid, temperature, covid-19",
  timeout = 60,
  parse = FALSE,
  file_name = "tweets_climate.json")


tweets_climate <- parse_stream("tweets_climate.json")

tweets_climate %>%
  ts_plot("1 second") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #Twitter statuses from past 100 seconds",
    subtitle = "Twitter status (tweet) counts aggregated using 1 second intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
climate_vocab <- 
  tweets_climate %>%
  select(user_id,source,created_at,text) %>% 
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


corpus <- VCorpus(VectorSource(climate_vocab$word))
dtm <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm))
freq <- data.frame(names(freq), count = as.numeric(freq))
freq <- freq[order(-freq$count),]
#head(freq)

climatesent <- inner_join(
  get_sentiments("nrc"),
  climate_vocab, 
  by="word") %>% 
  count(sentiment)  

ggplot(climatesent, aes(sentiment, n)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sentiment") +
  ylab("Frequency expressed in tweets")

head(get_sentiments("nrc"))

#pal2 <- brewer.pal(8,"Dark2")
#plot_wc <- wordcloud(climate_vocab$word,scale=c(8,.2),min.freq=2,
#max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
df <- as.data.frame(freq)
wcloud2 <- wordcloud2(data = df, size = 2.6)
wcloud2


