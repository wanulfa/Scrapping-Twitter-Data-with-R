# Scrapping-Twitter-Data-with-R

There is an R library called rtweet which will be used to download the tweets and create a data frame. Use the code given below to proceed

```{r}
install.packages("httr")
```
```{r}
install.packages("rtweet")
```
```{r}
install.packages("httpuv")
```

```{r}
library("httr")
library("rtweet")
library("httpuv")
```


# the name of the twitter app created by you
```{r}
appname <- "tweet-analytics"
```

# api key (replace the following sample with your key)

```{r}
key <- "IC2MiUjOszYm8zDKl1TChksPp"
secret <- "c5YV7oDhNGSkdEVBmlpz3aRF6V91IxslvPOMZuBSaEor5MXdia"
token <- "61221956-FP0kvLEQNfnIkBljJFaGa7Ilh1WJQrX0M07M8M8x1"
tokensec <- "lp0PJafD1vSt2vQH1uqnm1napGgPOU4Pz2Ppb0lrmfBq4"
```

```{r}
setup_twitter_oauth(key, secret, token, tokensec)
```


# create token named "twitter_token"

```{r}
twitter_token <- create_token(app = appname, consumer_key = key, consumer_secret = secret)
```
```{r}
jkw_tweets <- get_timeline("jokowi", n = 3200)
```

```{r}
install.packages("twitteR")
```
```{r}
library(twitteR)
```
```{r}
account <- "jokowi" 
account.timeline <- userTimeline(account, n=1000, includeRts = TRUE)
trialDF <- twListToDF(account.timeline)
file.timeline <- paste(account, "jokowi.csv", sep = "")
write.csv(TrialDF, file.timeline)
```


```{r}
write.csv(jkw_tweets, file = 'jokowitweets.csv')
```



We’ll be using the amazing ggplot2 and lubridate library to plot charts and work with the dates. Go ahead and follow the code given below to install and load the packages:

```{r}
install.packages("ggplot2")
install.packages("lubridate")
library("ggplot2")
library("lubridate")
```
```{r}
ggplot(data = jkw_tweets,
  aes(month(created_at, label=TRUE, abbr=TRUE),
  group=factor(year(created_at)), color=factor(year(created_at))))+
  geom_line(stat="count") +
  geom_point(stat="count") +
  labs(x="Month", colour="Year") +
  xlab("Month") + ylab("Number of tweets") +
  theme_minimal()
```

We can see the break-up of the month-wise tweets (spikes on March 2014, March 2016 and  October 2015) over the years, but interpretation is difficult. Let’s now simplify the chart by plotting only year-wise tweet counts.

```{r}
ggplot(data = jkw_tweets, aes(x = year(created_at))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Year") + ylab("Number of tweets") +
  scale_x_continuous (breaks = c(2010:2018)) +
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")
```

Let’s now find out whether Jokowi tweets equally over the months of a year or there are any specific months in which she tweets the most. Use the following code to create the chart:

```{r}
ggplot(data = jkw_tweets, aes(x = month(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Month") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")
```

#Comparison of the number of re-tweets and original tweets
Now we’ll compare the number of original tweets and re-tweets. Given below is the code:

```{r}
ggplot(data = jkw_tweets, aes(x = created_at, fill = is_retweet)) +
  geom_histogram(bins=48) +
  xlab("Time") + ylab("Number of tweets") + theme_minimal() +
  scale_fill_manual(values = c("chartreuse4", "chartreuse3"),
                    name = "Retweet")
```

#TEXT MINING

The downloaded dataset already has a column containing hashtags; we’ll be using that to find out the top 10 hashtags used by Jokowi. 

```{r}
install.packages("dplyr")
library("dplyr")
```

# Getting the hashtags from the list 
```{r}
jkw_tags_split <- unlist(strsplit(as.character(unlist(jkw_tweets$hashtags)),'^c\\(|,|"|\\)'))
```

# Formatting by removing the white spacea

```{r}
jkw_tags <- sapply(jkw_tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
jkw_tag_df <- as_data_frame(table(tolower(jkw_tags_split[jkw_tags])))
jkw_tag_df <- jkw_tag_df[with(jkw_tag_df,order(-n)),]
jkw_tag_df <- jkw_tag_df[1:10,]
ggplot(jkw_tag_df, aes(x = reorder(Var1, -n), y=n)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags") + ylab("Count")
```


Now we’ll analyze the tweet text to find out the most frequent words and create a word cloud

#install text mining and word cloud package

```{r}
install.packages(c("tm", "wordcloud"))
library("tm")
library("wordcloud")
tweet_text <- jkw_tweets$text
```

#Removing numbers, punctations, links and alphanumeric content

```{r}
tweet_text<- gsub('[[:digit:]]+', '', tweet_text)
tweet_text<- gsub('[[:punct:]]+', '', tweet_text)
tweet_text<- gsub("http[[:alnum:]]*", "", tweet_text)
tweet_text<- gsub("([[:alpha:]])\1+", "", tweet_text)
```

#creating a text corpus

```{r}
docs <- Corpus(VectorSource(tweet_text))
```

# coverting the encoding to UTF-8 to handle funny characters 

```{r}
docs <- tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))
```
# Converting the text to lower case
```{r}
docs <- tm_map(docs, content_transformer(tolower))
```
# Removing english common stopwords
```{r}
docs <- tm_map(docs, removeWords, stopwords("english"))
```

# Removing stopwords specified by us as a character vector

```{r}
docs <- tm_map(docs, removeWords, c("amp"))
```

# creating term document matrix 

```{r}
tdm <- TermDocumentMatrix(docs)
```

# defining tdm as matrix
```{r}
m <- as.matrix(tdm)
```


# getting word counts in decreasing order
```{r}
word_freqs = sort(rowSums(m), decreasing=TRUE)
```
# creating a data frame with words and their frequencies
```{r}
jkw_wf <- data.frame(word=names(word_freqs), freq=word_freqs)
```
# plotting wordcloud


```{r}
set.seed(1234)
wordcloud(words = jkw_wf$word, freq = jkw_wf$freq, 
          min.freq = 1,scale=c(1.8,.5),
          max.words=200, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
```


```{r}
write.csv(jkw_tweets, file = 'jokowitweet.csv')
```











