library(twitteR)
library(ROAuth)
library(RCurl)
library(RJSONIO)
library(stringr)
library(data.table)
library(sentimentr)
library(tidyr)
library(tidytext)
library(dplyr)
library(tibble)
library(stylo)


#authentication keys
api_key <- "ePn6F9vhSnKq2vlkuVNlFNc2i"
api_secret <- "AVWYs4fFWAJx8vMuMdonrNvOd1ZxkT7v57TVHacA18kkcTUMts"
token <- "1014906652045271041-Y8SQ4bDWhfezqELH0DsoSkyQ24mtuH"
token_secret <- "FOP44ZnfpNETA085D23Jdxmd2OyGWYCqcX8kcm9iWCHfY"


#retrieve tweets
setup_twitter_oauth(api_key, api_secret,token, token_secret)
zenithBank<- searchTwitter("'@ZenithBank' OR to:ZenithBank", n=1000)
firstBank <- searchTwitter("@fbn_help OR to:fbn_help OR @FirstBankngr",n=1000)
gtBank<-searchTwitter(" @gtbank_help OR to:@gtbank_help OR to:@gtbank", n=1000)
accessBank<-searchTwitter("@accessbank_help OR to:@accessbank_help OR to:@myaccessbank",n=1000)
UBA<-searchTwitter("@UBACares OR to:@UBACares OR to:@UBAGroup", n=1000)
diamondBank<- searchTwitter("'#DiamondBankHelp' OR #DiamondBank OR #DiamondBankNG OR to:@DiamondBankHelp",n=1000)
ecoBank<- searchTwitter("to:@ecobank_nigeria OR ecobank customer+care OR #ecobank_nigeria OR @ecobank_nigeria",n=1000)


#save tweets in data frames
zenithBank.df <- twListToDF(zenithBank)
firstBank.df <- twListToDF(firstBank)
gtBank.df <- twListToDF(gtBank)
accessBank.df <- twListToDF(accessBank)
UBA.df <- twListToDF(UBA)
diamondBank.df <- twListToDF(diamondBank)
ecoBank.df <- twListToDF(ecoBank)


#add a bank name column/variable to serve as an identifier for in each dataframe
zenithBank.df$bank_name <- "Zenith Bank"
firstBank.df$bank_name <- "First Bank"
gtBank.df$bank_name <- "GT Bank"
accessBank.df$bank_name <- "Access Bank"
UBA.df$bank_name <- "United Bank of Africa"
diamondBank.df$bank_name <- "Diamond Bank"
ecoBank.df$bank_name<- "Eco Bank"


#bind all the dataframes into a single dataframe
allBankCollection <- rbind(zenithBank.df,firstBank.df,gtBank.df,accessBank.df,UBA.df,diamondBank.df,ecoBank.df)


#cleanData

#remove emojis
allBankCollection$text<-gsub("<.*>", "",allBankCollection$text) 
#remove &
allBankCollection$text = gsub("&amp", "", allBankCollection$text)
#remove retweet entities
allBankCollection$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",allBankCollection$text)
#remove @ people
allBankCollection$text = gsub("@\\w+","",allBankCollection$text)
#remove punctuation
allBankCollection$text = gsub("[[:punct:]]","",allBankCollection$text)
#remove digits
allBankCollection$text = gsub("[[:digit:]]","",allBankCollection$text)
#remove html links
allBankCollection$text = gsub("http\\w+","",allBankCollection$text)
#remove emoji and bizarre signs
allBankCollection$text <- iconv(allBankCollection$text,from ="latin1", to= "ascii", sub="")
#remove unecessary spaces
allBankCollection$text = gsub("[ \t]{2,}","",allBankCollection$text)
#remove unecessary spaces
allBankCollection$text = gsub("^\\s+|\\s+$","",allBankCollection$text)
#get rid of apostrophes
allBankCollection$text = gsub("'","",allBankCollection$text)
#get rid of unnecessary spaces
allBankCollection$text <- str_replace_all(allBankCollection$text," "," ")
# Take out retweet header, there is only one
allBankCollection$text <- str_replace(allBankCollection$text,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
allBankCollection$text <- str_replace_all(allBankCollection$text,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
allBankCollection$text <- str_replace_all(allBankCollection$text,"@[a-z,A-Z]*","")   


#get tweets as sentences
sentences <- get_sentences(allBankCollection$text)


#get the afinn lexicon
afinn <- get_sentiments("afinn")


#make the afinn lexicon suitable for polarity_dt
afinnKey <-as_key(afinn, comparison = lexicon::hash_valence_shifters, sentiment = TRUE) 


#Calculate the sentiment score 
sentimentscore<-sentiment(sentences, polarity_dt = afinnKey,
          valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
          amplifier.weight = 0.8, n.before = Inf, n.after = Inf,
          question.weight = 1, adversative.weight = 0.85,
          neutral.nonverb.like = FALSE, missing_value = NULL)


#Add column element_id to ensure each tweet has a unique identifier
allBankCollection$element_id<-seq.int(nrow(allBankCollection))


#Get only the columns we need from the main table
truncatedTable <- allBankCollection[, c(1,5,17,18)]


#merge new table(the tweets) with their respective sentiment scores 
scoredSentiment<- merge(truncatedTable, sentimentscore, by="element_id")

#remove rows with NA
finalScoredSentiment <- scoredSentiment[rowSums(is.na(scoredSentiment)) == 0,]

#set working directory and write to csv
setwd("C:/Users/njayo/Documents/R")
write.csv(finalScoredSentiment, file = "scoredSentiment.csv")

wordCloudText <- finalScoredSentiment%>%
                  unnest_tokens(word,text)%>%
                    select(word,bank_name)%>%as_tibble

#Remove stop words
wordCloudTextGrouped <- data(stop_words)
wordCloudTextGrouped <- wordCloudText %>%
  anti_join(stop_words, by = c("word" = "word"))%>%
  group_by(word,bank_name)%>% 
  summarise(num_of_words=n())%>% 
  filter(num_of_words >=30)

 uniqueStopwords<-data.frame (word= c("toand","and","didnt","hellothank", "ecobank"))

wordCloudTextGrouped <- wordCloudTextGrouped %>% anti_join(uniqueStopwords,by=c("word"="word"))
write.csv(wordCloudTextGrouped, file = "wordCloudTextGrouped.csv")


