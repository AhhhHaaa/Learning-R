library(tidytext)
library(tidyverse)
install.packages("tm")
yes
install.packages("slam")

install.packages("NLP")
library("slam") #it requires the Gfortran compiler in MacOS
library(NLP)
library(tm)
####
docs <- Corpus(DirSource("./texts", encoding = "UTF-8"))
inspect(docs) # access the specific document
summary(docs) # summary the docs 
docs[[1]]     # access the content in text1
docs[1]       # access the text1
#-------------------------------------#
# transform "content_transformer(tolower)"
docs <- tm_map(docs, content_transformer(tolower)) 
# removal of stopwords "removeWords"
docs_no_stopwords <- tm_map(docs, removeWords, stopwords("english"))
# removal of punctuation "removePunctuation"
docs_no_punctuation <- tm_map(docs, removePunctuation)

#The whole process
docs <- docs %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removePunctuation)
#create a document-term metrix this using the "DocumentTermMatrix()"
trumpDTM <- DocumentTermMatrix(docs)
#Access the content of DocumentTermMatrix
inspect(trumpDTM)
# Use view()to make it easier to view
View(inspect(trumpDTM))

#Access specific row and column
inspect(trumpDTM[1:2, 1:2])

#remove the zero time and any low frequency terms"removeSparseTerms()"

#remove by occur 0 time in a percentage of 5%
trumpDTMS <- removeSparseTerms(trumpDTM, 0.05)
#remove by occur 0 time in a percentage of 10%
trumpDTMS <- removeSparseTerms(trumpDTM, 0.10)

#look the occurrence of terms within the coupus
inspect(trumpDTM[, c("news", "fake", "america", "great")])
inspect(trumpDTM[,c("free", "russia", "news")])

#find the popular words by "findFreqTerms()"
trumpFreqTerms <- findFreqTerms(trumpDTM, lowfreq = 50)

#Sum the frquency value
#Before sum, we need to convert DTM into normal matrix "as.matrix()"
#and then use "colSums() which return the named vector

trumpFreqTerms <- colSums(as.matrix(trumpDTM))

#use "sort()" to order the frequency
sort(trumpFreqTerms, decreasing = TRUE)

##another way is to covert into dataframe
trumpFreqTermsDF <- data.frame(word = names(trumpFreqTerms),freq = trumpFreqTerms)
arrange(trumpFreqTermsDF, desc(freq)) %>% head(10)

#Draw a barchart 
trumpFreqTermsDF100 <- subset(trumpFreqTermsDF, freq >= 100) %>% arrange(desc(freq))
barplot(trumpFreqTermsDF100$freq, names.arg = trumpFreqTermsDF100$word)

## wordcloud data visualization
install.packages("wordcloud")
library(wordcloud)
#set.seed to reduce the number of words on wordcloud
set.seed(142)
##world could
wordcloud(trumpFreqTermsDF$word, trumpFreqTermsDF$freq, min.freq = 25)

# use "findAssocs()" to find the words appear together
findAssocs(trumpDTM, "fake", corlimit = 0.9)
#####tidytexttidytexttidytexttidytexttidytext##
#tidytext and other packages
library(tidytext)
library(janeaustenr)#Jameausten novels
library(dplyr)
library(stringr)
##use pipeline to summarise the lines
View(austen_books())
austen_books() %>% 
  group_by(book) %>% 
  summarise(total_lines = n())

#tokenise by"unnest_tokens"
austenTidyBooks <- austen_books() %>% unnest_tokens(word, text)
#computing word frequencies
austenTidyBooks %>% 
  count(word, sort = TRUE)

#remove the stopwords:"anti_join()"
data("stop_words")
austenTidyBooks <- austenTidyBooks %>% 
  anti_join(stop_words)

#chain commend
austenTidyBooks %>% 
  filter(book == "Emma") %>% 
  count(word, sort = TRUE)

#lexicons
#sentiments dataframe
data(sentiments)
#get_sentiments()
get_sentiments("bing")
# nrc
install.packages("textdata")
#count the sentiment frequency
get_sentiments("nrc") %>% 
  group_by(sentiment) %>% 
  summarise(count = n())

#use "inner_join() to join the words from sentiment dictionary
#to the words of Emma

#creat nrcAnger by filer
nrcAnger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

austenTidyBooks %>% 
  filter(book == "Emma") %>% 
  inner_join(nrcAnger) %>% 
  count(word, sort = TRUE)

#comparison the proportion of words matching sentiment types
#####need to spend more time to figure out
austenTidyBooks %>% 
  filter(book == "Emma") %>% 
  summarise(count = n())

austenTidyBooks %>% 
  filter(book == "Emma") %>% 
  inner_join(get_sentiments("nrc")) %>% 
  summarise(count = n())

austenTidyBooks %>% 
  filter(book == "Emma") %>% 
  inner_join(get_sentiments("nrc")) %>% 
  group_by(sentiment) %>% 
  summarise(percent = (n()/34141) * 100)

#count total lines and find out how many novels
austen_books() %>% group_by(book) %>%
      summarise(total_lines = n())
#count how many words are negative in book: Persuasion = 17976
austenTidyBooks %>% 
  filter(book == "Persuasion") %>% 
  inner_join(get_sentiments("nrc")) %>% 
  summarise(count = n())
#count total words in book: Persuasion = 24588
Persuasion_total_words <- austenTidyBooks %>% 
  filter(book == "Persuasion") %>%
  summarise(words = n()) 

austenTidyBooks %>% 
  filter(book == "Persuasion") %>% 
  inner_join(get_sentiments("nrc")) %>% 
  summarise(percent = n()/Persuasion_total_words * 100)


help("get_sentiments")

# #count words frequency in sentiment
mynrc <- get_sentiments("nrc")
mynrc_Freq<- as.data.frame(mynrc)  %>%   #as.dataframe
    count(sentiment, sort = TRUE) #count words frequency in sentiment

#filer Trust words 
austenTidyBooks %>% 
  filter(book == "Persuasion") %>% 
  inner_join(nrcTrust) %>% 
  count(word, sort = TRUE)
#reshape2 package
install.packages("reshape2")
library(reshape2)
library(tidyverse)
library(wordcloud)  #wordcloud package including"comparison.word"

austenTidyBooks %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colours = c("#F8766D", "#00BFC4"), max.word = 100)#in"wordcloud"

#tidytext provides the tf-idf
#bind_tf_idf()
library(tidytext)
austenTDFIF <- austenTidyBooks %>% 
  count(book, word, sort = TRUE) %>%
  bind_tf_idf(word, book, n) %>% 
  arrange(desc(tf_idf))

austenTDFIF %>% 
  group_by(book) %>% 
  top_n(12, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free") +
  ylab("tf_idf")+
  coord_flip()





  



