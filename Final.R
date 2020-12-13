library(text2vec)#import text2vec
library(data.table)
data("movie_review")#import data as data frame
setDT(movie_review)
setkey(movie_review,id)#set movie review as database, id is key
all_ids=movie_review$id#get all review id
train_ids = sample(all_ids, 1000)#take randomly 1000 id
test_ids = setdiff(all_ids, train_ids)#rest id
training = movie_review[train_ids,]#take part 1 data, 1000 datapoint
testing = movie_review[test_ids,]#part 2 data, 4000 datapoint
traintok<-itoken(training$review, preprocessor = tolower, tokenizer = word_tokenizer, ids = training$id, progressbar = TRUE)#function to lower all character, split as word
stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")#set "useless" word
vocab = create_vocabulary(traintok, stopwords = stop_words)#remove useless word
pruned_vocab = prune_vocabulary(vocab,term_count_min=10,doc_proportion_max=0.5, doc_proportion_min=0.001)#pruned low frequncy words
vectorizer = vocab_vectorizer(pruned_vocab)#word to vector
dtm_train = create_dtm(traintok, vectorizer)#create dtm for part 1 data
testtok<-itoken(testing$review, preprocessor = tolower, tokenizer = word_tokenizer, ids = training$id, progressbar = TRUE)#function to lower all character, split as word
dtm_test  = create_dtm(testtok, vectorizer)#create dtm for part 2 data
d1_d2_cos_sim = sim2(dtm_train, dtm_test, method = "cosine", norm = "l2")#create cosine distance matrix
t1cor=order(d1_d2_cos_sim[1,],decreasing=TRUE)[1:5] #first five data relevent to train[1] data


