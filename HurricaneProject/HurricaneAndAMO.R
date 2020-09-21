################################################################################################
## read year-wise index data
df_amo_year = read.csv('data/amo_by_year.csv', header = TRUE, sep = "\t")
df_amo_year$avg = rowMeans(df_amo_year[,c(-1)])
head(df_amo_year)

## read category 4 year-wise data
df_cat4_year_full = read.csv('data/category4_by_year.csv', header = TRUE, sep = "\t")
df_cat4_year = df_cat4_year_full[df_cat4_year_full$Year > 1947,]
head(df_cat4_year)

# category 4 year-wise analysis
par(mfrow=c(2,1))
plot(df_amo_year$Year, df_amo_year$avg, type="o", xlab="year", ylab="mean AMO over year", 
	main="Category 4 By Year", ylim=c(-0.5,0.5))
plot(df_cat4_year$Year, df_cat4_year$Frequency, type="o", xlab="year", ylab="frequency", main="Category 4 By Year")

print('------------------ category 4 year-wise correlation test -----------------------------')
cor.test(df_amo_year$avg, df_cat4_year$Frequency, method = "pearson")
cor.test(df_amo_year$avg, df_cat4_year$Frequency, method = "kendall")
cor.test(df_amo_year$avg, df_cat4_year$Frequency, method = "spearman")

## read category 5 year-wise data
df_cat5_year_full = read.csv('data/category5_by_year.csv', header = TRUE, sep = "\t")
df_cat5_year = df_cat5_year_full[df_cat5_year_full$Year > 1947,]
head(df_cat5_year)

# category 5 year-wise analysis
par(mfrow=c(2,1))
plot(df_amo_year$Year, df_amo_year$avg, type="o", xlab="year", ylab="mean AMO over year", 
	main="Category 5 By Year", ylim=c(-0.5,0.5))
plot(df_cat5_year$Year, df_cat5_year$Frequency, type="o", xlab="year", ylab="frequency", main="Category 5 By Year")

print('------------------ category 5 year-wise correlation test -----------------------------')
cor.test(df_amo_year$avg, df_cat5_year$Frequency, method = "pearson")
cor.test(df_amo_year$avg, df_cat5_year$Frequency, method = "kendall")
cor.test(df_amo_year$avg, df_cat5_year$Frequency, method = "spearman")


#################################################################################################
## read decade-wise index data
df_amo_decade = read.csv('data/amo_by_decade.csv', header = TRUE, sep = "\t")
head(df_amo_decade)

## read category4 decade-wise data
df_cat4_decade_full = read.csv('data/category4_by_decade.csv', header = TRUE, sep = "\t")
df_cat4_decade = df_cat4_decade_full[df_cat4_decade_full$Decade > 1930,]
head(df_cat4_decade)

# category 4 decade-wise analysis
plot(df_amo_decade$Decade, df_amo_decade$AvgIndex, type="o", xlab="decade", ylab="mean AMO over decade", 
	main="Category 4 By Decade", ylim=c(-0.5,0.5))
plot(df_cat4_decade$Decade, df_cat4_decade$Frequency, type="o", xlab="decade", ylab="frequency over decade", 
	main="Category 4 By Decade")

print('------------------ category 4 decade-wise correlation test -----------------------------')
cor.test(df_amo_decade$AvgIndex, df_cat4_decade$Frequency, method = "pearson")
cor.test(df_amo_decade$AvgIndex, df_cat4_decade$Frequency, method = "kendall")
cor.test(df_amo_decade$AvgIndex, df_cat4_decade$Frequency, method = "spearman")

## read category 5 decade-wise data
df_cat5_decade_full = read.csv('data/category5_by_decade.csv', header = TRUE, sep = "\t")
df_cat5_decade = df_cat5_decade_full[df_cat5_decade_full$Decade > 1930,]
head(df_cat5_decade)

# category 5 decade-wise analysis
par(mfrow=c(2,1))
plot(df_amo_decade$Decade, df_amo_decade$AvgIndex, type="o", xlab="decade", 
	ylab="mean AMO over decade", main="Category 5 By decade", ylim=c(-1,1))
plot(df_cat5_decade$Decade, '^'(df_cat5_decade$Frequency, (1/2)), type="o", xlab="decade", 
	ylab="sq. root of frequency over decade", main="Category 5 By decade")

print('------------------ category 5 decade-wise correlation test -----------------------------')
cor.test(df_amo_decade$AvgIndex, '^'(df_cat5_decade$Frequency, (1/2)), method = "pearson")
cor.test(df_amo_decade$AvgIndex, '^'(df_cat5_decade$Frequency, (1/2)), method = "kendall")
cor.test(df_amo_decade$AvgIndex, '^'(df_cat5_decade$Frequency, (1/2)), method = "spearman")