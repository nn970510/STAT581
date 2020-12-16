
library(tidyr)

# get all file names in tables folder
filenames <- list.files("tables", pattern="*.tsv", full.names=TRUE)
paste(filenames)

# read each file in tables folder as a dataframe
datalist = list()
for (i in 1:length(filenames)) {
	df <- read.delim(filenames[i], header=FALSE, sep="\t")

	# remove first row (header)
	df_without_header <- df[-1,]

	# remove last 3 columns except id
	df_trimmed <- df_without_header[, -c(length(df_without_header)-3, length(df_without_header)-2)]
	
	# concat all columns except last as one
	df_fact <- unite(df_trimmed[, -c(length(df_trimmed)-1)], 'fact', colnames(df_trimmed[, -c(length(df_trimmed)-1)]), sep = " ", remove = TRUE, na.rm = TRUE)

	# trim leading and trailing spaces
	df_fact_tidy <- data.frame(lapply(df_fact, trimws), stringsAsFactors = FALSE)

	# add the id column
	df_fact_tidy$id <- df_trimmed[, c(length(df_trimmed)-1)]

	datalist[[i]] <- df_fact_tidy
}

# merge all dataframes into one
all_facts = do.call(rbind, datalist)

# export output
write.table(all_facts, file = "all_facts.csv", row.names = FALSE, col.names = FALSE)