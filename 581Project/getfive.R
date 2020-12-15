library(text2vec)#import text2vec
library(data.table)
library(stringr)

trainq<-read.csv("questions.train.csv",header=FALSE)
all_facts<-read.csv("all_facts.csv",header=FALSE)
names(trainq)<-c("ques")
names(all_facts)<-c("fact")


prep_fun=function(x){
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")  
}

factrim=prep_fun(all_facts$fact)
facttok<-itoken(factrim, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = TRUE)
quetrim=prep_fun(trainq$ques)
quetok<-itoken(quetrim, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = TRUE)
stop_words = c("which", "why","for","its", "what", "where", "of", "as", "how", "is", "a", "that","this", "these","to", "he", "she", "are","in","on","and","an","be","by","it")
vocab = create_vocabulary(quetok, stopwords = stop_words)
pruned_vocab = prune_vocabulary(vocab,term_count_min=1,doc_proportion_max=0.5)
vectorizer = vocab_vectorizer(pruned_vocab)
dtm_qes = create_dtm(quetok, vectorizer)
dtm_fact  = create_dtm(facttok, vectorizer)
simmat = sim2(dtm_qes, dtm_fact, method = "cosine", norm = "l2")
lensim=nrow(simmat)
res<-data.frame()
i=1
while (i<=lensim){
  tcor=order(simmat[i,],decreasing = TRUE)[1:5]
  ans=paste(all_facts$fact[tcor],sep = ",")
  res<-rbind(res,c(trainq$ques[i],ans))
  i=i+1
}
write.table(res, file = "q&a.csv", row.names = FALSE, col.names = FALSE)


data("movie_review")#import data as data frame
setDT(movie_review)
setkey(movie_review,id)#set movie review as database, id is key
prep_fun=function(x){
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")  
}
movie_review$review = prep_fun(movie_review$review)
all_ids=movie_review$id#get all review id
train_ids = sample(all_ids, 1000)#take randomly 1000 id
test_ids = setdiff(all_ids, train_ids)#rest id
training = movie_review[train_ids,]#take part 1 data, 1000 datapoint
testing = movie_review[test_ids,]#part 2 data, 4000 datapoint
traintok<-itoken(training$review, preprocessor = tolower, tokenizer = word_tokenizer, ids = training$id, progressbar = TRUE)#function to lower all character, split as word
stop_words = c("which", "why", "what", "where", "of", "as", "how", "is", "a", "b", "c", "d", "to", "are")#set "useless" word
vocab = create_vocabulary(traintok, stopwords = stop_words)#remove useless word
pruned_vocab = prune_vocabulary(vocab,term_count_min=1,doc_proportion_max=0.5, doc_proportion_min=0.001)#pruned low frequncy words
vectorizer = vocab_vectorizer(pruned_vocab)#word to vector
dtm_train = create_dtm(traintok, vectorizer)#create dtm for part 1 data
testtok<-itoken(testing$review, preprocessor = tolower, tokenizer = word_tokenizer, ids = training$id, progressbar = TRUE)#function to lower all character, split as word
dtm_test  = create_dtm(testtok, vectorizer)#create dtm for part 2 data
d1_d2_cos_sim = sim2(dtm_train, dtm_test, method = "cosine", norm = "l2")#create cosine distance matrix
t1cor=order(d1_d2_cos_sim[1,],decreasing=TRUE) #first five data relevent to train[1] data


