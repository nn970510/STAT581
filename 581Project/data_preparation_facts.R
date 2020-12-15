library(tidyr)

# get all file names in tables folder
filenames <- list.files("./tables", pattern="*.tsv", full.names=TRUE)
paste(filenames)

# read each file in tables folder as a dataframe
datalist = list()
for (i in 1:length(filenames)) {
	df <- read.delim(filenames[i], header=FALSE, sep="\t")

	# remove first row (header)
	df_without_header <- df[-1,]

	# remove last 3 columns
	df_trimmed <- df_without_header[1:(length(df_without_header)-4)]

	# concat all columns as one
	df_fact <- unite(df_trimmed, 'fact', colnames(df_trimmed), sep = " ", remove = TRUE, na.rm = TRUE)

	# trim leading and trailing spaces
	df_fact_tidy <- data.frame(lapply(df_fact, trimws), stringsAsFactors = FALSE)
	datalist[[i]] <- df_fact_tidy
}

# merge all dataframes into one
all_facts = do.call(rbind, datalist)

# export output
write.table(all_facts, file = "all_facts.csv", row.names = FALSE, col.names = FALSE)

