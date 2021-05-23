###########################
#       Attila Serfozo    #
#       Data Science 3    #
#        Term Project     #
###########################


# Import libraries --------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(tidytext)
data("stop_words")
library(topicmodels)

library(ggplot2)
library(wordcloud)  # Creating wordclouds
library(igraph)     # Creating network charts
library(ggraph)     # Creating network charts


# Data transformation -----------------------------------------------------

data <- read.csv("Robinhood_GooglePlay_Reviews_enUS.csv")

data <- data %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  arrange(at) %>% 
  mutate(reviewId = 1:nrow(data),
         date = as.Date(at),
         reply_date = as.Date(repliedAt),
         year = year(date),
         month = month(date),
         week = week(date),
         day = day(date),
         month_end = ceiling_date(date, 'month')-1) %>% 
  select(-c('userImage',"userName","at", 'repliedAt')) %>% 
  filter(content != is.na(content))


# Exploratory Data Analysis -----------------------------------------------

skimr::skim(data)

summary_stat <- data %>% group_by(month_end) %>% 
  summarise(avg_score=mean(score),
            nr_rating = n(),
            nr_replies = sum(!is.na(replyContent)))
tail(summary_stat,16)

ggplot(summary_stat, aes(x=month_end, y = avg_score)) +
  geom_line(color='red', size=1) +
  labs(title='Average monthly rating across time', x='Date', y='Rating') +
  theme_bw()


ggplot( summary_stat, aes( x=month_end ) ) +
  geom_line( aes( y = avg_score ) , color = 'red', size=1 ) +
  annotate("text", x = ymd("2018-01-01"), y = 1.5, label = "Before Covid-19") +
  annotate("rect", xmin = ymd("2015-08-01"), xmax = ymd("2020-03-01"), ymin = 0, ymax = 5,
           alpha = .2, fill = "yellow" )+
  annotate("text", x = ymd("2020-03-15"), y =1, label = "Robinhood outage", angle=90) +
  annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2020-04-01"), ymin = 0, ymax = 5,
           alpha = .2, fill = "blue" )+
  annotate("text", x = ymd("2020-08-01"), y = 1.5, label = "Pandemic") +
  annotate("rect", xmin = ymd("2020-04-01"), xmax = ymd("2021-01-01"), ymin = 0, ymax = 5,
           alpha = .2, fill = "green" )+
  annotate("text", x = ymd("2021-02-01"), y = 0.7, label = "Gamestop", angle=90) +
  annotate("rect", xmin = ymd("2021-01-01"), xmax = ymd("2021-03-01"), ymin = 0, ymax = 5,
           alpha = .2, fill = "purple" )+
  annotate("text", x = ymd("2021-04-01"), y = 1.5, label = "After Gamestop", angle=90) +
  annotate("rect", xmin = ymd("2021-03-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 5,
           alpha = .2, fill = "orange" ) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(title='Average monthly app rating', x = 'Date' , y = 'Rating' )+
  theme_bw()


ggplot(summary_stat, aes(x=month_end, y = nr_rating)) +
  geom_line(color='navyblue', size=1) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(title='Number of ratings by month', x='Date', y='Number of Ratings') +
  theme_bw()


ggplot(summary_stat, aes(x=month_end, y = nr_replies)) +
  geom_line(color='navyblue', size=1) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(title='Number of replies by month', x='Date', y='Number of replies') +
  theme_bw()


# Create before and after Gamestop dataset --------------------------------

jan_ratings <- data %>% filter(date>='2021-01-01',
                            date< '2021-02-01')%>% 
  group_by(day) %>% 
  summarise(avg_score=mean(score))

# The Robinhood - Gamestop scandal erupted on 28 of January 2021
ggplot(jan_ratings, aes(x=day, y = avg_score)) +
  geom_line(color='navyblue', size=1) +
  labs(title='Average ratings in January', x='Days', y='Rating') +
  theme_bw()

data <- data %>% mutate(period = ifelse(date <'2021-01-28','before', 'after'))


# Unnest tokens -----------------------------------------------------------

# Use unnest tokens to get words
tidy_df <- data %>% mutate(id = rownames(data)) %>% 
  unnest_tokens(word,content)


# Remove stop words -------------------------------------------------------

tidy_df <- tidy_df %>%
  anti_join(stop_words)


# Word frequency ----------------------------------------------------------

tidy_df %>% 
  count(period, word, sort=TRUE) %>% 
  group_by(period) %>% 
  top_n(20,n) %>%  
  ggplot(aes(reorder_within(word, n, period),n))  + 
  geom_col(show.legend = F, fill = 'navyblue') + 
  facet_wrap(~factor(period,levels=c('before','after')), scales = 'free') +
  scale_x_reordered() +
  coord_flip() + 
  theme_bw() + 
  xlab('') +
  labs(title='20 Most frequent words before and after the GameStop events')

# Difference between before and after Gamestop
frequency <- 
  tidy_df %>% 
  count(period, word) %>% 
  group_by(period) %>% 
  mutate(proportion = n/sum(n)) %>% select(-c(n)) %>% 
  pivot_wider(names_from = period, values_from = proportion) %>% 
  filter(before > 0.0015 | after > 0.0015)

ggplot(frequency,aes(x=before, y = after)) + 
  geom_point( fill = 'lightseagreen', color='lightseagreen', size = 2, alpha=0.5) + 
  geom_text(aes(label = word), hjust = -0.2, size = 3, alpha=0.7) + 
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  labs(title='Distribution of proportion of words before and after the GME events') 

# Looking closer
ggplot(frequency,aes(x=before, y = after)) + 
  geom_point( fill = 'lightseagreen', color='lightseagreen', size = 2, alpha=0.5) + 
  geom_text(aes(label = word), hjust = -0.2, size = 3, alpha=0.7) + 
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,0.01)) +
  scale_y_continuous(limits = c(0,0.01)) +
  theme_bw() +
  labs(title='Distribution of proportion of words before and after the GME events') 


# Sentiment ---------------------------------------------------------------

get_sentiments('afinn')
get_sentiments('nrc')
get_sentiments('bing')
get_sentiments('loughran')


afinn <- tidy_df %>% 
  inner_join(get_sentiments('afinn')) %>% 
  group_by(month_end) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = 'AFINN')

bing_nrc_and_loughran <- bind_rows(tidy_df %>% 
                                     inner_join(get_sentiments("bing")) %>% 
                                     mutate(method = "Bing et al."),
                                   tidy_df %>% 
                                     inner_join(get_sentiments("nrc") %>% 
                                                  filter(sentiment %in% c("positive",
                                                                          "negative"))) %>% 
                                     mutate(method = "NRC"),
                                   tidy_df %>% 
                                     inner_join(get_sentiments("loughran") %>% 
                                                  filter(sentiment %in% c("positive",
                                                                          "negative"))) %>% 
                                     mutate(method = "Loughran and McDonald")) %>% 
  count(method, month_end, sentiment) %>% 
  spread(sentiment,n,fill=0) %>% 
  mutate(sentiment=positive-negative)

# Sentiment before 2021

bind_rows(filter(afinn, month_end<'2021-01-01'),
          filter(bing_nrc_and_loughran, month_end<'2021-01-01')) %>% 
  ggplot(aes(month_end,sentiment,fill=method))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~method, ncol=1, scales="free_y") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(title="Positivity of Robinhood reviews until the end of 2020", x="Date", y="Positivity") +
  theme_bw()

# Sentiment in whole timeline

bind_rows(afinn,
          bing_nrc_and_loughran) %>% 
  ggplot(aes(month_end,sentiment,fill=method))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~method, ncol=1, scales="free_y") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(title="Positivity of Robinhood reviews until end of April 2021", x="Date", y="Positivity") +
  theme_bw()


# Loughran words ----------------------------------------------------------

loughran_words <- tidy_df %>% 
  count(word, sort=TRUE) %>% 
  inner_join(get_sentiments('loughran')) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n))

ggplot(loughran_words, aes(x=word, y=n)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = 'free') +
  labs(title="Loughran and McDonalds sentiment categories top 10 words") +
  theme_bw()
  
# Positivity according to Loughran sentiment during time

loughran_period <- tidy_df %>% 
  inner_join(get_sentiments('loughran')) %>% 
  count(sentiment, period) %>% 
  spread(sentiment, n, fill = 0)

loughran_date <- tidy_df %>% 
  inner_join(get_sentiments('loughran')) %>% 
  count(sentiment, month_end) %>% 
  spread(sentiment, n, fill = 0)

loughran_date %>% 
  mutate(score = (positive-negative) / (positive + negative)) %>% 
  ggplot(aes(x=month_end, y=score)) +
  geom_line(color='navyblue', size=1) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(title='Positivity of Robinhood reviews (monthly)', x='Date', y='Positivity score of reviews') +
  theme_bw()


# tf-idf ------------------------------------------------------------------

tf_idf_words <- tidy_df %>% 
  count(period, word, sort = T) %>% 
  ungroup() %>%
  bind_tf_idf(word,period,n) %>% 
  arrange(desc(tf_idf))

# Remove special characters or broken words
tf_idf_words <- tf_idf_words %>% 
  filter(word != 'ů',word != 'á',word != '9.00',word != 'dob',word != 'đť',
         word != 'ř',word != 'áµ',word != 'đź‘ťđź‘ťđź‘ťđź‘ť ')

tf_idf_words %>% 
  group_by(period) %>% 
  top_n(10,tf_idf) %>%  
  ggplot(aes(reorder_within(word, n, period),n))  + 
  geom_col(show.legend = F, fill = 'navyblue') + 
  facet_wrap(~factor(period,levels=c('before','after')), scales = 'free') +
  scale_x_reordered() +
  coord_flip() + 
  theme_bw() + 
  xlab('') +
  labs(title='10 Most frequent words before and after the GameStop events in the tf-idf method')


## Wordcloud
library(wordcloud)

wordcloud(words=filter(tf_idf_words,period=='before')$word, freq=filter(tf_idf_words,period=='before')$tf_idf,
          scale=c(4,0.7), max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(5,"Dark2"))

wordcloud(words=filter(tf_idf_words,period=='after')$word, freq=filter(tf_idf_words,period=='after')$tf_idf,
          scale=c(5,1), max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(5,"Dark2"))


# Tokenizing by n-grams ---------------------------------------------------

bigram_df <- data %>% 
  unnest_tokens(bigram,content, token = 'ngrams', n = 2)

bigram_td_idf <- bigram_df %>% 
  count(period,bigram, sort =T) %>%
  bind_tf_idf(bigram, period, n) %>%
  arrange(desc(tf_idf)) %>% 
  separate(bigram,c('word1', 'word2'), sep=  ' ') %>% 
  filter(
    !word1 %in% stop_words$word ,
    !word2 %in% stop_words$word) %>% 
  unite(col = 'bigram',word1:word2, sep =' ' ) %>% 
  group_by(period) 

bigram_td_idf %>% 
  top_n(10,tf_idf) %>% 
  ggplot(aes(reorder_within(bigram, n, period),n))  + 
  geom_col(show.legend = F, fill = 'navyblue') + 
  facet_wrap(~factor(period,levels=c('before','after')), scales = 'free') +
  scale_x_reordered() +
  coord_flip() + 
  theme_bw() + 
  xlab('') +
  labs(title='10 Most frequent word pairs before and after the GameStop events')


# Visualizing network of bigrams with ggraph
library(igraph)
library(ggraph)

bigram_counts <- data %>%
  unnest_tokens(bigram,content, token = 'ngrams', n = 2) %>% 
  separate(bigram,c('word1', 'word2'), sep=  ' ') %>% 
  filter(
    !word1 %in% stop_words$word ,
    !word2 %in% stop_words$word,
    word1 != is.na(word1)) %>% 
  count(period, word1, word2, sort = T )

bigram_graph_before <- bigram_counts %>% 
  filter(period=='before') %>% 
  select(from = word1, to = word2, n) %>% 
  filter(n > 200) %>% 
  graph_from_data_frame()

bigram_graph_after <- bigram_counts %>% 
  filter(period=='after') %>% 
  select(from = word1, to = word2, n) %>% 
  filter(n > 299) %>% 
  graph_from_data_frame()


set.seed(20210522)
a<- grid::arrow(type = 'closed', length = unit(.15, 'inches'))

ggraph(bigram_graph_before, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n), show.legend = F, 
                 arrow= a, end_cap = circle(.07,'inches')) +
  geom_node_point(color = 'lightblue', size = 5) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(title="Most frequent consecutive words before GME") +
  theme_void()

ggraph(bigram_graph_after, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n), show.legend = F, 
                 arrow= a, end_cap = circle(.07,'inches')) +
  geom_node_point(color = 'lightblue', size = 5) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(title="Most frequent consecutive words before GME") +
  theme_void()


# Topic Modelling ---------------------------------------------------------

tf_idf_id <- tidy_df %>% 
  count(id, period, word, sort = T) %>% 
  ungroup() %>%
  bind_tf_idf(word,id,n) %>% 
  arrange(desc(tf_idf))

tf_idf_filtered <- tf_idf_id %>% filter(tf_idf > median(tf_idf_id$tf_idf))

tf_idf_dtm <- tf_idf_filtered %>% 
  unite(document, period, id) %>% 
  count(document, word) %>% 
  cast_dtm(document, word, n)

tf_idf_lda <- LDA(tf_idf_dtm, k = 2, control = list(seed = 20210523))

tf_idf_lda %>% tidy(matrix="beta") %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder(term,beta)) %>% 
  ggplot(aes(term,beta,fill = factor(topic)))+geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = 'free') + 
  coord_flip() +
  labs(title="Top words of topics") +
  theme_bw()

tf_idf_lda %>% tidy(matrix="beta") %>% 
  mutate(topic = paste0("topic", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>% 
  mutate(log_ratio = log2(topic2 / topic1)) %>% 
  top_n(20,abs(log_ratio)) %>% 
  mutate(term = reorder(term,log_ratio) ) %>% 
  ggplot(aes(term, log_ratio, fill = log_ratio > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title="Words with largest beta difference") +
  theme_bw()

tf_idf_lda %>% 
  tidy(matrix = 'gamma') %>% 
  separate(document, c('period', 'id'), sep = '_') %>% 
  mutate(top_critic = reorder(period, gamma * topic)) %>% 
  ggplot(aes(factor(topic), gamma)) + 
  geom_boxplot() + 
  facet_wrap(~factor(period,levels=c('before','after')), scales = 'free') +
  labs(title="Distribution of gamma for each topic within the period") +
  theme_bw()
