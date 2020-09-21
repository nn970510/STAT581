################################################################################################
## read year-wise index data
df_co2_year = read.csv('data/CO2emissions_by_year.csv', header = TRUE, sep = "\t")
head(df_co2_year)

## read category 4 year-wise data
df_cat4_year_full = read.csv('data/category4_by_year.csv', header = TRUE, sep = "\t")
df_cat4_year = df_cat4_year_full[(df_cat4_year_full$Year > 1959 & df_cat4_year_full$Year < 2017),]
head(df_cat4_year)

# category 4 year-wise analysis
par(mfrow=c(2,1))
plot(df_co2_year$Year, df_co2_year$CO2_emissions_metric_tons_per_capita, type="o", xlab="year", 
	ylab="Avg emission over year (metric tons per capita)", main="Category 4 By Year")
plot(df_cat4_year$Year, df_cat4_year$Frequency, type="o", xlab="year", ylab="frequency", main="Category 4 By Year")

print('------------------ category 4 year-wise correlation test -----------------------------')
cor.test(df_co2_year$CO2_emissions_metric_tons_per_capita, df_cat4_year$Frequency, method = "pearson")
cor.test(df_co2_year$CO2_emissions_metric_tons_per_capita, df_cat4_year$Frequency, method = "kendall")
cor.test(df_co2_year$CO2_emissions_metric_tons_per_capita, df_cat4_year$Frequency, method = "spearman")

## read category 5 year-wise data
df_cat5_year_full = read.csv('data/category5_by_year.csv', header = TRUE, sep = "\t")
df_cat5_year = df_cat5_year_full[(df_cat4_year_full$Year > 1959 & df_cat4_year_full$Year < 2017),]
head(df_cat5_year)

# category 5 year-wise analysis
par(mfrow=c(2,1))
plot(df_co2_year$Year, df_co2_year$CO2_emissions_metric_tons_per_capita, type="o", xlab="year", 
	ylab="Avg emission over year (metric tons per capita)", main="Category 5 By Year")
plot(df_cat5_year$Year, df_cat5_year$Frequency, type="o", xlab="year", ylab="frequency", main="Category 5 By Year")

print('------------------ category 5 year-wise correlation test -----------------------------')
cor.test(df_co2_year$CO2_emissions_metric_tons_per_capita, df_cat5_year$Frequency, method = "pearson")
cor.test(df_co2_year$CO2_emissions_metric_tons_per_capita, df_cat5_year$Frequency, method = "kendall")
cor.test(df_co2_year$CO2_emissions_metric_tons_per_capita, df_cat5_year$Frequency, method = "spearman")


#################################################################################################
## read decade-wise index data
df_co2_decade = read.csv('data/CO2emissions_by_decade.csv', header = TRUE, sep = "\t")
head(df_co2_decade)

## read category4 decade-wise data
df_cat4_decade_full = read.csv('data/category4_by_decade.csv', header = TRUE, sep = "\t")
df_cat4_decade = df_cat4_decade_full[(df_cat4_decade_full$Decade > 1950 & df_cat4_decade_full$Decade < 2020),]
head(df_cat4_decade)

# category 4 decade-wise analysis
plot(df_co2_decade$Decade, df_co2_decade$CO2_emissions_metric_tons_per_capita, type="o", xlab="decade", 
	ylab="Avg emission over decade (metric tons per capita)", main="Category 4 By Decade")
plot(df_cat4_decade$Decade, df_cat4_decade$Frequency, type="o", xlab="decade", ylab="frequency over decade", 
	main="Category 4 By Decade")

print('------------------ category 4 decade-wise correlation test -----------------------------')
cor.test(df_co2_decade$CO2_emissions_metric_tons_per_capita, df_cat4_decade$Frequency, method = "pearson")
cor.test(df_co2_decade$CO2_emissions_metric_tons_per_capita, df_cat4_decade$Frequency, method = "kendall")
cor.test(df_co2_decade$CO2_emissions_metric_tons_per_capita, df_cat4_decade$Frequency, method = "spearman")

## read category 5 decade-wise data
df_cat5_decade_full = read.csv('data/category5_by_decade.csv', header = TRUE, sep = "\t")
df_cat5_decade = df_cat5_decade_full[(df_cat4_decade_full$Decade > 1950 & df_cat4_decade_full$Decade < 2020),]
head(df_cat5_decade)

# category 5 decade-wise analysis
par(mfrow=c(2,1))
plot(df_co2_decade$Decade, df_co2_decade$CO2_emissions_metric_tons_per_capita, type="o", xlab="decade", 
	ylab="Avg emission over decade (metric tons per capita)", main="Category 5 By decade")
plot(df_cat5_decade$Decade, df_cat5_decade$Frequency, type="o", xlab="decade", 
	ylab="frequency over decade", main="Category 5 By decade")

print('------------------ category 5 decade-wise correlation test -----------------------------')
cor.test(df_co2_decade$CO2_emissions_metric_tons_per_capita, df_cat5_decade$Frequency, method = "pearson")
cor.test(df_co2_decade$CO2_emissions_metric_tons_per_capita, df_cat5_decade$Frequency, method = "kendall")
cor.test(df_co2_decade$CO2_emissions_metric_tons_per_capita, df_cat5_decade$Frequency, method = "spearman")