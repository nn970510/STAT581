library(tidyr)
library(dplyr)
library(stringr)


# function to extract correct answer string from the question column using the correct answer key
getCorrectAns <- function(quesString, key)
{
	keyString <- paste("\\(", key ,"\\)", sep="")
	spl1 <- strsplit(sub(keyString, "START", quesString), "START", fixed = TRUE)
	res1 <- sapply(spl1, "[", 2)
	spl2 <- strsplit(sub("\\(", "END", res1), "END", fixed = TRUE)
	res2 <- sapply(spl2, "[", 1)
	res3 <- gsub("[()]", "", res2)
	ans <- str_trim(res3)
	return(ans)
}

# function to extract the question statement without the answer choices from the question column
getQues <- function(quesString)
{
	endchar <- paste("\\(", "A" ,"\\)", sep="")
	spl <- strsplit(sub(endchar, "END", quesString), "END", fixed = TRUE)
	res <- sapply(spl, "[", 1)
	ques <- str_trim(res)
	return(ques)
}

# function to perform data preprocessing on a raw questions file
questionsPreprocessing <- function(split)
{	
	# read questions file
	inputFile <- paste("questions/questions", ".", split, ".tsv", sep="")
	df <- read.delim(inputFile, header=TRUE, sep="\t")
	df_raw <- select(df, 'question', 'AnswerKey')

	# extract correct answer string
	getCorrectAns_v <- Vectorize(getCorrectAns)
	(df_raw$ans <- getCorrectAns_v(df_raw$question, df_raw$AnswerKey))

	# extract question statement
	getQues_v <- Vectorize(getQues)
	(df_raw$ques <- getQues_v(df_raw$question))

	# concat question and correct answer strings
	df_ques_ans <- select(df_raw, 'ques', 'ans')
	df_out <- unite(df_ques_ans, 'merged_ques_ans', colnames(df_ques_ans), sep = " ", remove = TRUE, na.rm = TRUE)

	# trim leading and trailing spaces
	df_out_tidy <- data.frame(lapply(df_out, trimws), stringsAsFactors = FALSE)

	# export output
	outputFile <- paste("questions", ".", split, ".csv", sep="")
	write.table(df_out_tidy, file = outputFile, row.names = FALSE, col.names = FALSE)
}

trainques=questionsPreprocessing("train")
questionsPreprocessing("dev")
questionsPreprocessing("test")
