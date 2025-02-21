# install.packages("jiebaR")
library("jiebaR") # 中文斷詞套件

# install.packages("wordcloud2") # 文字雲
library(wordcloud2)

# install.packages("tm") # 文字探勘
library(tm)
# install.packages("tmcn") # 文字探勘（中文）
library(tmcn)

## 顯示中文
# install.packages("showtext")
library(showtext)
showtext_auto()
font_families()

library(tidyverse)

# install.packages("proxy")
library(proxy)

# install.packages("SnowballC")
library(SnowballC)
setwd("/Users/sandyyin/Desktop/RRRRRRR/hwwww/hw5")

# 載入資料
data <- read.csv("IMDb_Feature Film_2022_review_data.csv")
data = data[data$Title=="Beast",] # 選擇電影beast
head(data)
comment <- Corpus(VectorSource(data$Review))

# 移除不必要的文字形式
comment<-tm_map(comment,tolower) # 文字轉成小寫
comment<-tm_map(comment,stripWhitespace) # 刪除空白
comment<-tm_map(comment,removeNumbers) # 刪除數字
comment<-tm_map(comment,removePunctuation) # 刪除標點符號
comment<-tm_map(comment,removeWords, c(stopwords("english"), "movie", "film") ) # 刪除特定詞彙＆停詞（常用但沒意義的字）

# 文字雲
com <- tm_map(comment, stemDocument)
str(com[[1]])
tdm <- TermDocumentMatrix(com)
inspect(tdm)
com_m <- as.matrix(tdm)
freq_df <- rowSums(com_m)
freq_df <- sort(freq_df, decreasing = T) # 將單字出現的頻率從高排到低
freq_df <- data.frame(word = names(freq_df),
                      num = freq_df)
head(freq_df)
wordcloud2(slice_max(freq_df, order_by = num, n = 80), 
           size = 0.5, shape = "cardioid")


# 情感分析
# install.packages("tidytext")
library(tidytext)
bing_words_count <- freq_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T)
bing_words_count
table(bing_words_count$sentiment) # negative>positive

bing_words_count %>% 
  filter(sentiment == "positive") %>% 
  select(word, n) %>% 
  wordcloud2(size = 0.08, shape = "star")

bing_words_count %>% 
  filter(sentiment == "negative") %>% 
  select(word, n) %>% 
  wordcloud2(size = 0.08, shape = "circle")