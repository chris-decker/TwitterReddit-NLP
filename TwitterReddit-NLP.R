library(rtweet)
library(tidytext)
library(stringr)
library(dplyr)
library(xml2)
library(wordcloud)
library(tidyr)
library(purrr)
library(RedditExtractoR)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = 'TwAPIAuth')

create_token(
  app = app_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)

#Retrieve sample of tweets mentioning 'canada' and 'mexico'

ca <-
  search_tweets('canada',
                lang = 'en',
                n = 1500,
                include_rts = FALSE) %>%
  filter(str_detect(tolower(text), 'canada')) %>% head(1000)
mx <-
  search_tweets('mexico',
                lang = 'en',
                n = 1500,
                include_rts = FALSE) %>%
  filter(str_detect(tolower(text), 'mexico')) %>% head(1000)

load(file = 'content')

#Process each set of tweets into tidytext objects

unescape_html <- function(str) {
  xml2::xml_text(xml2::read_html(paste0('<x>', str, '</x>')))
}

ca$stripped_text <-
  gsub('http\\S+', '', sapply(ca$text, unescape_html))
mx$stripped_text <-
  gsub('http\\S+', '', sapply(mx$text, unescape_html))

ca.tidy <- ca %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
mx.tidy <- mx %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

#Apply pre-processing transformations

data('stop_words')

ca.pre <-
  ca.tidy %>% filter(sapply(ca.tidy, str_length) > 2) %>%  anti_join(stop_words) %>% mutate(word = str_replace(word, "'|'", ''))
mx.pre <-
  mx.tidy %>% filter(sapply(mx.tidy, str_length) > 2) %>%  anti_join(stop_words) %>% mutate(word = str_replace(word, "'|'", ''))

#Generate frequency for each term

ca.count <-
  ca.pre %>% filter(word != 'canada') %>% count(word, sort = T)
mx.count <-
  mx.pre %>% filter(word != 'mexico') %>% count(word, sort = T)
ca.count
mx.count

#Generate word clouds

wordcloud(
  words = ca.count$word,
  freq = ca.count$n,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, 'Dark2')
)

wordcloud(
  words = mx.count$word,
  freq = mx.count$n,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, 'Dark2')
)

#Generate frequency for each bigram

create.bgrm <- function(twt) {
  bgrm <- twt %>%
    select(stripped_text) %>%
    unnest_tokens(pairs, stripped_text, token = 'ngrams', n = 2)
  sep_pairs <-
    bgrm %>% separate(pairs, c('Word1', 'Word2'), sep = ' ')
  return (
    sep_pairs %>% filter(!Word1 %in% stop_words$word &
                           sapply(Word1, str_length) > 2) %>%
      filter(!Word2 %in% stop_words$word &
               sapply(Word2, str_length) > 2)
  )
}

ca.bgrm <- create.bgrm(ca)
ca.bgrm %>% count(Word1, Word2, sort = T)
mx.bgrm <- create.bgrm(mx)
mx.bgrm %>% count(Word1, Word2, sort = T)

#Generate  sentiment scores for each tweet & compute summary statistics

sentiment_bing = function(twt) {
  twt_tbl = tibble(stripped_text = twt) %>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments('bing')) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
    mutate(score = case_when(sentiment == 'negative' ~ n * (-1),
                             sentiment == 'positive' ~ n * 1))
  sent.score = case_when(nrow(twt_tbl) == 0 ~ 0,
                         nrow(twt_tbl) > 0 ~ sum(twt_tbl$score))
  zero.type = case_when(nrow(twt_tbl) == 0 ~ 'Type 1',
                        nrow(twt_tbl) > 0 ~ 'Type 2')
  list(score = sent.score,
       type = zero.type,
       twt_tbl = twt_tbl)
}

ca.sent <- lapply(ca$stripped_text, sentiment_bing)
mx.sent <- lapply(mx$stripped_text, sentiment_bing)

country.sent <- bind_rows(
  tibble(
    country = 'ca',
    score = unlist(map(ca.sent, 'score')),
    type = unlist(map(ca.sent, 'type'))
  ),
  tibble(
    country = 'mx',
    score = unlist(map(mx.sent, 'score')),
    type = unlist(map(mx.sent, 'type'))
  )
)

country.sent %>% filter(type != 'Type 1') %>% group_by(country) %>%
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

#Display most negative tweets for each country

ca[which.min(country.sent[1:nrow(ca), ]$score), ]$stripped_text
mx[which.min(country.sent[nrow(ca) + 1:1000000L, ]$score), ]$stripped_text

#Further analysis on subset of tweets

ca %>% select(stripped_text) %>% filter(str_detect(tolower(stripped_text), 'people')) %>% top_n(10) %>% View

get_sentiments('bing') %>% filter(word == 'migrants')


#Retrieve sample of Reddit comments mentioning 'canada' and 'mexico'

get.wn.comments <- function(term) {
  links = reddit_urls(
    search_terms = term,
    subreddit = 'worldnews',
    sort_by = 'new',
    page_threshold = 1
  )
  return (links %>% top_n(1, num_comments) %>% pull(URL) %>% reddit_content() %>% as_tibble())
}

ca.r <- get.wn.comments('canada')
mx.r <- get.wn.comments('mexico')

#Process each set of comments into tidytext objects

ca.r$stripped_text <-
  gsub('http\\S+', '', sapply(ca.r$comment, unescape_html))
mx.r$stripped_text <-
  gsub('http\\S+', '', sapply(mx.r$comment, unescape_html))

ca.r.tidy <- ca.r %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
mx.r.tidy <- mx.r %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

#Apply pre-processing transformations

ca.r.pre <-
  ca.r.tidy %>% filter(sapply(ca.r.tidy, str_length) > 2) %>% anti_join(stop_words) %>% mutate(word = str_replace(word, "'|'", ''))
mx.r.pre <-
  mx.r.tidy %>% filter(sapply(mx.r.tidy, str_length) > 2) %>% anti_join(stop_words) %>% mutate(word = str_replace(word, "'|'", ''))

#Generate frequency for each term

ca.r.count <-
  ca.r.pre %>% filter(word != 'canada') %>% count(word, sort = T)
mx.r.count <-
  mx.r.pre %>% filter(word != 'mexico') %>% count(word, sort = T)
ca.r.count
mx.r.count

#Generate word clouds

wordcloud(
  words = ca.r.count$word,
  freq = ca.r.count$n,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, 'Dark2')
)

wordcloud(
  words = mx.r.count$word,
  freq = mx.r.count$n,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, 'Dark2')
)

#Generate  sentiment scores for each post & compute summary statistics

ca.r.sent <- lapply(ca.r$stripped_text, sentiment_bing)
mx.r.sent <- lapply(mx.r$stripped_text, sentiment_bing)

country.r.sent <- bind_rows(
  tibble(
    country = 'ca',
    score = unlist(map(ca.r.sent, 'score')),
    type = unlist(map(ca.r.sent, 'type'))
  ),
  tibble(
    country = 'mx',
    score = unlist(map(mx.r.sent, 'score')),
    type = unlist(map(mx.r.sent, 'type'))
  )
)

country.r.sent %>% filter(type != 'Type 1') %>% group_by(country) %>%
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

#Display most negative posts for each country

ca.r[which.min(country.r.sent[1:nrow(ca.r), ]$score), ]$stripped_text
mx.r[which.min(country.r.sent[nrow(ca.r) + 1:1000000L, ]$score), ]$stripped_text
