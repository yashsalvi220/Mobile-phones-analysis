#Text Analysis of Google Pixel 5 Phones

#Step 1: Install and load neccessary packages
install.packages('tm')
install.packages('SnowballC')
install.packages('topicmodels')
install.packages('stopwords')
install.packages('wordcloud')
install.packages('Rcpp')
install.packages('syuzhet')
install.packages('readr')
library(tm)
library(SnowballC)
library(topicmodels)
library(stopwords)
library(wordcloud)
library(Rcpp)
library(syuzhet)
library(readr)
#Step2: Load Data
#Load the Google Pixel 5data
OnePlus9<- read_csv("C:/Users/yashs/OneDrive/Documents/Fun/Datasets/archive (6)/OnePlus9Pro_reviews.csv") 
                                                                                                                             #Check dimension of data
dim(OnePlus9)#Shows number of rows and columns
class(OnePlus9)
#Convert data into a corpus
mycorpus<- VCorpus(VectorSource(OnePlus9$userComments)) #Create a volatile corpus
writeLines(as.character(mycorpus[[1]]))

#Step3: Preprocess Data
getTransformations()

#Remove URL from data
removeURL<- function(x) gsub('http[^[:space:]]*','',x) #Function to remove URLS
mycorpus<- tm_map(mycorpus,content_transformer(removeURL))
writeLines(as.character(mycorpus[[1]]))

#Remove Punctuation from data
mycorpus<-tm_map(mycorpus,removePunctuation)
writeLines(as.character(mycorpus[[1]]))

#Remove other characters other than English and black spaces
removeotherchar<- function(x) gsub("[^[:alpha:][:space:]]*","",x)
mycorpus<-tm_map(mycorpus,content_transformer(removeotherchar))
writeLines(as.character(mycorpus[[1]]))

#Transform to lower case
mycorpus<-tm_map(mycorpus,content_transformer(tolower))
writeLines(as.character(mycorpus[[1]]))

#Remove all English stopwords
mycorpus<-tm_map(mycorpus,removeWords,c(stopwords('english')))
writeLines(as.character(mycorpus[[1]]))

#Stemming words to an original form
mycorpus<-tm_map(mycorpus,stemDocument)
writeLines(as.character(mycorpus[[1]]))

#Striping whitespace
mycorpus<-tm_map(mycorpus,stripWhitespace)
writeLines(as.character(mycorpus[[1]]))

#Topic modelling
#Step4: Create document-term matrix. This helps in classifying rows for analysis
#Weighting system1: term frequency with focus on word letter
mat<-DocumentTermMatrix(mycorpus,control = list(wordLengths=c(3,20)))
mat

# Step5: Build Topic Model
model1<- LDA(mat,k=5) #select five topics for our analysis
ldout.topics<-as.matrix(topics(model1))
write.csv(ldout.topics,"topic_label_Oneplus.csv",row.names= TRUE)

#Build key words related to each topic
ldaout.terms<-(terms<-terms(model1,10))
write.csv(ldaout.terms,"terms_Oneplus.csv",row.names = FALSE)

#Converting data into term document matrix
TextDoc<-TermDocumentMatrix(mycorpus)
tdm<-as.matrix(TextDoc)
tdm[1:10,1:20]
#Building bar plot
w<-rowSums(tdm)
w<-subset(w,w>25)
barplot(w,las=2,col = rainbow(50))

#Build plot of most frequently used items
w<-sort(rowSums(tdm), decreasing = TRUE)
wordfrequency<- data.frame(word=names(w),freq= w)
head(wordfrequency,5)
# Plot the most frequent words
barplot(wordfrequency[1:5,]$freq, las = 2, names.arg = wordfrequency[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#Build wordcloud for analysis
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

#Find association among words
findAssocs(TextDoc, terms= c("like","batteri","camera"), corlimit= 0.25)
#Find association among words occuring 50 times a week
findAssocs(TextDoc, terms = findFreqTerms(TextDoc,lowfreq = 50), corlimit= 0.25) 

#Sentiment analysis of words
OnePlus9<- read_csv("C:/Users/13345/Dropbox/My PC (DESKTOP-B1D9GNS)/Downloads/archive (6)/OnePlus9Pro_reviews.csv")
sentiment<- iconv(OnePlus9$userComments)
s <- get_nrc_sentiment(sentiment)
head(s)

#Build barplot to understand relationship between posts
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')

#Comparison of sentiment with other measures
#syuzhet method
syuzhet_vector <- get_sentiment(sentiment, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)
# bing method
bing_vector <- get_sentiment(sentiment, method="bing")
head(bing_vector)
summary(bing_vector)
#affin method
afinn_vector <- get_sentiment(sentiment, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)))

#Emotions percentage
barplot(
  sort(colSums(prop.table(s[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage")