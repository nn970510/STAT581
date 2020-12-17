library(text2vec)#import text2vec
library(data.table)
library(stringr)
library(stringi)

trainq<- readRDS(file = "questions.rds")
all_facts<-read.csv("all_facts.csv",header=TRUE)
names(trainq)<-c("ques","explain")

prep_fun=function(x){
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")  
}
#tokenize question/fact
factrim=prep_fun(all_facts$fact)
facttok<-itoken(factrim, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = TRUE)
quetrim=prep_fun(trainq$ques)
quetok<-itoken(quetrim, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = TRUE)
#make stop word list
stop_words = c("which", "why","for","its", "what", "where", "of", "as", "how", "is", "a", "that","this", "these","to", "he", "she", "are","in","on","and","an","be","by","it")
vocab = create_vocabulary(quetok, stopwords = stop_words)
# Remove words too frequent
pruned_vocab = prune_vocabulary(vocab,term_count_min=1,doc_proportion_max=0.5)
#Create dictionary
vectorizer = vocab_vectorizer(pruned_vocab)
#Create DTM
dtm_qes = create_dtm(quetok, vectorizer)
dtm_fact  = create_dtm(facttok, vectorizer)
#calculate cosine distance
simmat = sim2(dtm_qes, dtm_fact, method = "cosine", norm = "l2")
lensim=nrow(simmat)#get length of questions
res<-data.frame()
lenfact=nrow(all_facts)
i=1
nexist=0
nrexist=0
exist=0
while (i<=lensim){
  ord=order(simmat[i,],decreasing = TRUE)
  tcor=ord[1:5]
  random=sample(ord,size=5)
  ans=paste(all_facts$fact[tcor],sep = ",")
  exid=all_facts$id[tcor]#get top five explains of each question
  ranid=all_facts$id[random]#get random five explains of each question
  #get how many finding facts are true correspond facts
  find=stri_count_regex(trainq$explain[i,], paste(exid, collapse="|"))
  findr=stri_count_regex(trainq$explain[i,], paste(ranid, collapse="|"))
  res<-rbind(res,c(trainq$ques[i],ans))
  exist=exist+find
  #see if this question's fact did not to be found
  if (find==0){
    nexist=nexist+1
  }
  if (findr==0){
    nrexist=nrexist+1
  }
  i=i+1
}
#show result
cat("not find rate:",nexist/lensim)
cat("random not find rate:",nrexist/lensim)
cat("average each question find:",exist/lensim)
#write table
write.table(res, file = "q&a.csv", row.names = FALSE, col.names = FALSE,sep=",")




