install.packages("rvest")
install.packages("httr")
library(rvest)
library(httr)

#TASK 1
get_wiki_covid19_page<- function(){
  wiki_base_url<- "https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country"
  body<-list(title = "Template:COVID-19_testing_by_country" )
  response<-GET(wiki_base_url,query=body)
  return (response)
}
get_wiki_covid19_page()

#TASK 2
page = read_html(get_wiki_covid19_page())
title<-html_nodes(page,".COVID-19_testing_by_country")
covidTable<-html_table(html_nodes(title,"table"),fill=TRUE,convert = TRUE)
covidTable<-as.data.frame(covidTable)
covidTable

#TASK 3
summary(covidTable)
data_frame<-covidTable[1:172,]

#function for pre processing data
preprocess_covid_data_frame <- function(data_frame) {
  
  data_frame <- data_frame[1:172, ]
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units.b."] <- NULL
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested<- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$confirmed.tested.ratio <- as.numeric(gsub(",","",data_frame$confirmed.tested.ratio))
  data_frame$tested.population.ratio <- as.numeric(gsub(",","",data_frame$tested.population.ratio))
  data_frame$confirmed.population.ratio <- as.numeric(gsub(",","",data_frame$confirmed.population.ratio))
  return (data_frame)
}

covidData<-as.data.frame(preprocess_covid_data_frame(data_frame))
summary(covidData)
write.csv(covidData,"covid.csv")
#check if the csv file exists
wd <- getwd()
file_path <- paste(wd, sep="", "/covid.csv")
print(file_path)
file.exists(file_path)

#TASK 4 
fullData<-read.csv("covid.csv")
subsetDf<-fullData[5:10,c('country','confirmed')]
subsetDf

#TASK 5
sum1<-sum(fullData$confirmed)
sum2<-sum(fullData$tested)
positive_r<-sum1/sum2
positive_r

#TASK 6
#conversion into factor if needed, by default in character format only
fullData$country<-factor(fullData$country)
countryCol<-fullData$country
countryCol
print(is.factor(countryCol))
fullData$country<-as.character(fullData$country)
asc<-sort(fullData$country,decreasing=FALSE)
desc<-sort(fullData$country,decreasing=TRUE)
print(asc)
print(desc)

#TASK 7 

matched<-grep("United.+",fullData$country)
fullData$country[matched]

#TASK 8 
row1<-fullData[10,c('country','confirmed','confirmed.population.ratio')]
row2<-fullData[12,c('country','confirmed','confirmed.population.ratio')]
print(row1)
print(row2)
row1$confirmed.population.ratio
row2$confirmed.population.ratio


#Task 9
string<-row1$country
print("Country at more risk is: ")
if(row1$confirmed.population.ratio > row2$confirmed.population.ratio){
  print(row1$country)
} else{
  print(row2$country)
}

#task 10

data<-fullData[(fullData$confirmed.population.ratio)<1,]
data$country
