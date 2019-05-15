# first I install all packages and load all libraries
install.packages("tm")
install.packages("topicmodeling")

library(tidyverse)
library(tm)
library(tidytext)
library(stringr)
library(topicmodels)
library(data.table)
library(dplyr)

# Then I load the dataset that I am going to use in the topic modeling
nyt <- read.csv("Desktop/640/avocado/nytavocadosorted.csv",encoding="utf-8",
                header = TRUE, stringsAsFactors = FALSE)

# I am going to use the "Structured Text Example Corpus Cleaning" method
#"Str is a compact way to display the structure of an R object."
#This two lines are for overview purpose
str(nyt)
glimpse(nyt)

# use "doc_id" and "text" to replace the original column name
# in template the dataset name is peace_res, here is nyt
names(nyt)[names(nyt)=="web_url"]<-"doc_id"
nyt$doc_id <- as.character(nyt$doc_id)
names(nyt)[names(nyt)=="abstract"] <-"text"
colnames(nyt)

#use "dataframesource" and "vcorpus"method to get the corpus
nyt_source <- DataframeSource(nyt)
nyt_corpus <- VCorpus(nyt_source)

#after four stpes of cleaning, get a cleaned corpus
nyt_corpus
nyt_corpus[[1]]
nyt_corpus[[1]][1]

nyt_cleaned <- tm_map(nyt_corpus,removeNumbers)
nyt_cleaned[[1]][1]

nyt_cleaned1 <- tm_map(nyt_cleaned,stripWhitespace)
nyt_cleaned1[[1]][1]

nyt_cleaned2 <- tm_map(nyt_cleaned1,removePunctuation)
nyt_cleaned2[[1]][1]

nyt_cleaned3 <- tm_map(nyt_cleaned2,content_transformer(tolower))
nyt_cleaned3[[1]][1]

#sequence matters!then add my won stopwords

stopwords("en")
my_stops<- c(stopwords("en"),
             "shall", "will", "the", "and", "page", "agreement", 
             "for", "with", "between", "within","can","just","help",
             "like","also","'ll","one")


my_stops

nyt_cleaned4 <- tm_map(nyt_cleaned3,removeWords, my_stops)
nyt_cleaned4[[1]][1]

#Now I am going to do topic modeling for structured Text nty Corpus
#DTM means documentTermMatrix, 
nyt_dtm <- DocumentTermMatrix(nyt_cleaned4)
nyt_dtm

unique_indexes<- unique(nyt_dtm$i)
nyt_dtm <-nyt_dtm[unique_indexes,]
nyt_dtm

nyt_dtm_tidy <-tidy(nyt_dtm)
nyt_dtm_tidy

#lda means Linear Discriminant Analysis
# set the topics no.=6
k<-10
nyt_lda<-LDA(nyt_dtm, k=k, control = list(seed=1234))
nyt_lda

nyt_lda_words<-terms(nyt_lda,10)
nyt_lda_words

#after I have the ten columns of key words ready, i'm going to 
#write into a csv

nyt_lda_topics<- as.matrix(nyt_lda_words)
write.csv(nyt_lda_topics, file = paste("Desktop/640/avocado/nyt_LDA_",k, ".csv"))

#This is what the heading look like
head(nyt_lda_topics)

nyt_lda_tidy <- tidy(nyt_lda)
head(nyt_lda_tidy)


top_terms <- nyt_lda_tidy %>%
  group_by(topic) %>%
  top_n(10, beta)%>%
  ungroup()%>%
  arrange(topic, -beta)
head(top_terms)

#draw a ggplot for me, with those terms in topic groups
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#This part is define a function so that each time we can call it
#each time change the part outside of { } so that not so much stuff needs to be modified

get_LDA_topics_terms_by_topic <- function(input_corpus, plot = TRUE, number_of_topics = 10,
                                          number_of_words = 10,
                                          path = "Desktop/640/avocado/nyt_LDA_document_topics_")
  
  
{
  my_dtm <- DocumentTermMatrix(input_corpus)
  
  unique_indexes <- unique(my_dtm$i)
  my_dtm <- my_dtm[unique_indexes,]
  
  my_lda <- LDA(my_dtm, k = number_of_topics, control = list(seed=1234))
  my_topics <- tidy(my_lda, matrix = "beta")
  
  my_lda_words <- terms(my_lda, number_of_words)
  my_lda_topics <- as.matrix(my_lda_words)
  write.csv(my_lda_topics, file = paste(path,k, ".csv"))
  
  my_top_terms <- my_topics %>%
    group_by (topic)%>%
    top_n(number_of_words, beta)%>%
    ungroup()%>%
    arrange(topic -beta)
  
  if(plot == TRUE){
    my_top_terms %>%
      mutate(term=reorder(term,beta))%>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~topic, scales = "free")+
      coord_flip()
    
  }else{
    return(my_top_terms)
  }
}

#let me try the function, so clean!
get_LDA_topics_terms_by_topic(nyt_cleaned4, number_of_topics = 10, 
                              number_of_words = 10)

#This part is to find the gamma indicator
nyt_lda_document_topics <- tidy(nyt_lda, matrix="gamma")
head(nyt_lda_document_topics)

#and write in a csv
write.csv(nyt_lda_document_topics, file=paste("Desktop/640/avocado/nytnyt_LDA_document_topics_", k, ".csv"))

#dim means dimension
dim(nyt_lda_document_topics)

# ###???####################################
# #I have got error in this line, since I have duplicate identifiers for rows
# #spread in r means "spread a key-value pair across multiple columns"
# 
# nyt_lda_document <- spread(nyt_lda_document_topics, topic, gamma)
# head(nyt_lda_document)
# 
# nyt_lda_document$max_topic <- colnames(nyt_lda_document[2:7])[apply(nyt_lda_document, 1, which.max)]
# head(nyt_lda_document)
# 
# dt1 <- data.table(nyt_lda_document, key = "document")
# dt2 <- data.table(nyt_res, key = "doc_id")
# 
# nyt_merged <- dt1[dt2]
# dim(nyt_merged)
# colnames(nyt_merged)
# 
# #I am commenting this block out in case I do not have enough time for solving this
# ####???#####################################

