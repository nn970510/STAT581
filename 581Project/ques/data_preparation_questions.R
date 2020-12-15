library(tidyr)
library(dplyr)
library(stringr)

# getCorrectAns <- function(quesString, key)
# {
# 	print(key)
# 	print(quesString)
# 	print('--------------------')
# 	str1 = paste("\\(",key,"\\)")
# 	str2 = ""
# 	if(key != 'D'){
# 		str2 = "\\("
# 	} else {
# 		str2 = "."
# 	}
# 	pattern = paste(str1, "\\s*(.*?)\\s*", str2)
# 	res <- str_match(quesString, pattern)
# }

# read questions file
df1 <- read.delim("questions.train.tsv", header=TRUE, sep="\t")

# df_ques_raw <- select(df, 'question', 'AnswerKey')
df_ques_raw1 <- select(df1, 'question')

# getCorrectAns_v <- Vectorize(getCorrectAns)
# (df_ques_raw$queryString <- getCorrectAns_v(df_ques_raw$question, df_ques_raw$AnswerKey))

write.table(df_ques_raw1, file = "train_ques.csv", row.names = FALSE, col.names = FALSE)

df2 <- read.delim("questions.dev.tsv", header=TRUE, sep="\t")
df_ques_raw2 <- select(df2, 'question')
write.table(df_ques_raw2, file = "dev_ques.csv", row.names = FALSE, col.names = FALSE)

df3 <- read.delim("questions.test.tsv", header=TRUE, sep="\t")
df_ques_raw3 <- select(df3, 'question')
write.table(df_ques_raw3, file = "test_ques.csv", row.names = FALSE, col.names = FALSE)