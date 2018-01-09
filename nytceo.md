# Lessons from CEOs: Analyzing the Text from the New York Times Corner Office Column in R

From 2009 to 2017, reporter Adam Bryant of the New York Times completed a series of interviews with CEOs for the NYT Corner Office Column. I enjoyed reading a number of these articles and thought they might be a good fit for text mining. Thus, the inspiration for this fun side project: what do CEOs talk about?

Steps included:

1. Using the NYT API to get the URLs for Corner Office articles (451 out of the 525 published articles)
2. Using these URLs to obtain the body of the articles (text body is not available directly from the API)
3. Cleaning the transcripts such that the interview questions were removed and each response was coded as an individual document
4. Cleaning the text data
5. Producing a word cloud of the most frequent words
6. Using LDA to identify common topics and visualizing the topics in an interactive tree
7. An R-Shiny app to display CEO quotes that include the most common word ('people')

The Corner Office articles are edited interview transcripts with questions by the reporter highlighted in bold. An example article is https://www.nytimes.com/2017/09/29/business/corner-office-jody-gerson-universal-music.html

## Project Steps

The following steps will allow you to replicate the analysis on your computer. Alternatively, you could use a similar analysis for different keywords. An example of this is https://github.com/peridoteagle/fun-with-nyt-search-api

### Prerequisites

Before initiating this project, you will need to aquire a NYT API key https://developer.nytimes.com/signup and install the following R libraries:

```
#Please load the following libraries
library(XML)
library(rtimes)
library(stringr)
library(tm)
library(httr)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tibble)
library(quanteda)
library(wordcloud2)
library(ldatuning)
library(collapsibleTree)
library(shiny)
library(htmlwidgets)

#Set the following NYT Key:
Sys.setenv(NYTIMES_AS_KEY = "Your NYT API Key")
```

## Obtaining Article Data

Data for all articles were obtained from the NYT Search API. Note key limitations of the NYT Search API: 
* The API returns information (including URLs) in groups of 10
* Maximum of 1000 requests per day (10,000 articles total)
* Must be 1 second between requests. 

The key variables to obtain the articles:
1. The key terms "corner office" and "adam bryant" were used to search the title, author, and text body. These terms can be modified, as shown in https://github.com/peridoteagle/fun-with-nyt-search-api
2. The '20090701' and '20171025' are the start and end dates respectively for articles collected from the Corner Office column
3. To get n, I used trial and error to determine that approximately 730 articles were returned with the search terms in from the 'keywords' variable

```
#Obtaining articles from the NYT Search API
#Search terms, start and end dates, and number of articles can be adjusted
keywords <- "corner office adam bryant"
startdate <- '20090701'
enddate <- '20171025'
#n is number of desired articles
n <- 730
#nover10 divides n by 10 and rounds up; for use in later for loop
nover10 <- ceiling(n/10)
```

The loop below gets the following information for the articles: url, headline, kicker, section, publication date, news desk, the type of material, and the byline. Note that more information is available from the API. When information is missing, it is reported as NA.

```
urllist<-c()
urls <- c()
headlabel <-c()
headline <- c()
kicklabel <- c()
kicker <- c()
seclabel <- c()
section <- c()
dpublabel <- c()
datepub <- c()
newslabel <- c()
newsdesk <- c()
typelabel <-c()
typematerial <- c()
bylinelabel <- c()
byline <- c()
i=0
for (i in 0:nover10){
  Sys.sleep(1)
  tenarticles <- as_search(q=keywords,start_date =startdate, end_date = enddate,page=i)
  urllist <- tenarticles$data$web_url
  print(length(urllist))
  urls <- c(urls,urllist)
  headlabel <-tenarticles$data$headline.main
  headline <- c(headline,headlabel)
  kicklabel <- tenarticles$data$headline.kicker
  kicker <- c(kicker,kicklabel)
  seclabel <- tenarticles$data$section_name
  section <- c(section,seclabel)
  bylinelabel <- tenarticles$data$byline.original
  byline <- c(byline,bylinelabel)
  dpublabel <- tenarticles$data$pub_date
  datepub <- c(datepub,dpublabel)
  newslabel <- tenarticles$data$new_desk
  newsdesk <- c(newsdesk,newslabel)
  typelabel <- tenarticles$data$type_of_material
  typematerial <- c(typematerial,typelabel)
}
```

This search selects any article that includes these key words, including articles that are not actually in the Corner Office column. The following code limits the articles to only those published by Adam Bryant in the Corner Office column. Note that Adam Bryant wrote that he had completed 525 Corner Office Interviews, but only 451 came up through the search. 

```
#Selecting only those articles from the Corner Office blog
alldata <-  data.frame(urls,headline,kicker,datepub)
relevantarticles <- subset(alldata,kicker=="Corner Office")
```

The urls, headlines, and publication dates are used to label each document. To separate the urls, headlines, and publication dates, run the following:

```
#Separating relevant information for this analysis (namely URL, headline, and publication date)
relevanturls <- as.character(relevantarticles$urls)
relevantheads <- as.character(relevantarticles$headline)
relevantdates <- as.character(relevantarticles$datepub)

narticles <- length(relevanturls)
```

### Parsing the Articles for Term Frequency and Topic Analysis

First, define the function to parse the body of the article from the html files.

```
#Defining function to parse URL for article body
parseArticleBody <- function(artHTML) {
  xpath2try <- c('//div[@class="articleBody"]//p',
                 '//p[@class="story-body-text story-content"]',
                 '//p[@class="story-body-text"]'
  )
  for(xp in xpath2try) {
    bodyi <- paste(xpathSApply(htmlParse(artHTML), xp, xmlValue), collapse = "")
    if(nchar(bodyi)>0) break
  }
  return(bodyi)
}
```
The following loop accomplishes these tasks:
1. Extracts HTML file from every relevant URL
2. Separates out bold text as a bold line represents a question while a non-bold line represents an answer
3. Parses the HTML file
4. Replaces specific symbols in the text from reading the html file
5. Breaks into individual documents where each document is a question or answer by splitting on the bold lines; all question lines should start with BOLD ADAMBRYANT and end with BOLD

After the loop, the code removes all statements by Adam Bryant (the reporter).

```
articletext1 <- c()
j=1
for (j in 1:narticles){
  p <- GET(relevanturls[j])
  html <- content(p, 'text')
  newtry <- str_replace_all(html,"[<]strong[>]"," BOLD ADAMBRYANT ")
  newtry <- str_replace_all(newtry,"[<][/]strong[>]","BOLD ")
  artBody <- parseArticleBody(newtry)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  test <- strsplit(artBody,split="BOLD")
  articletext1 <- c(articletext1,test)
}

attempt1 <- unlist(articletext1)

#The following code separates the interviewer questions from the response questions for all respondents
corp1 <- VCorpus(VectorSource(attempt1))
textVector1 <- sapply(corp1, as.character)
newCorp1 <- Corpus(VectorSource(textVector1[-grep("ADAMBRYANT", textVector1, 
                                                  ignore.case = TRUE)]))
docs <- newCorp1
```

### Parsing the Articles for Shiny App

The Shiny App needs each individual answer to include the date, headline, and url so that the reader knows which article the snippet came from. This information cannot be included in the previous parsing as it skews the topic analysis.

This loop does the following tasks:
1. Extracts HTML file from every relevant URL
2. Separates out bold text as a bold line represents a question while a non-bold line represents an answer
3. Parses the HTML file
4. Replaces specific symbols in the text from reading the html file
5. Substitute date, headline, and url information at the end of every bold line
6. Breaks into individual documents where each document is a question or answer; all question lines should start with BOLD ADAMBRYANT and end with BOLD followed by the date/headline/url statement

After the loop, the code separates the output into documents and removes all questions asked by Adam Bryant (the reporter).

```
articletext2 <- c()
j=1
for (j in 1:narticles){
  p <- GET(relevanturls[j])
  html <- content(p, 'text')
  newtry <- str_replace_all(html,"[<]strong[>]"," BOLD ADAMBRYANT ")
  newtry <- str_replace_all(newtry,"[<][/]strong[>]","BOLD HEZDLINE ")
  artBody <- parseArticleBody(newtry)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  articleinfo<- paste("(",substring(relevantdates[j],1,10),": ",relevantheads[j]," [",relevanturls[j],"]",")",sep="")
  artBody <- str_replace_all(artBody,"HEZDLINE",articleinfo)
  test <- strsplit(artBody,split="BOLD")
  articletext2 <- c(articletext2,test)
}

attempt2 <- unlist(articletext2)

#The following code separates the interviewer questions (ADAMBRYANT) from the response questions for all respondents
corp2 <- VCorpus(VectorSource(attempt2))
textVector2 <- sapply(corp2, as.character)
newCorp2 <- Corpus(VectorSource(textVector2[-grep("ADAMBRYANT", textVector2, 
                                                  ignore.case = TRUE)]))
docswithheadlines <- newCorp2
```

## Data Cleaning

The following code cleans the text data:

```
#Convert symbols to spaces
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "—")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove common English stopwords
docs <- tm_map(docs, removeWords,c("a",	"about",	"above",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"below",	"between",	"both",	"but",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"few",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"is",	"isn't",	"it",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"no",	"nor",	"not",	"of",	"off",	"on",	"once",	"only",	"or",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"than",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"to",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves"))

#Removing contractions since they are generally made of stop words
docs <- tm_map(docs, removeWords,c("ain't",	"aren't",	"can't",	"can't've",	"'cause",	"could've",	"couldn't",	"couldn't've",	"didn't",	"doesn't",	"don't",	"hadn't",	"hadn't've",	"hasn't",	"haven't",	"he'd",	"he'd've",	"he'll",	"he'll've",	"he's",	"how'd",	"how'd'y",	"how'll",	"how's",	"I'd",	"I'd've",	"I'll",	"I'll've",	"I'm",	"I've",	"i'd",	"i'd've",	"i'll",	"i'll've",	"i'm",	"i've",	"isn't",	"it'd",	"it'd've",	"it'll",	"it'll've",	"it's",	"let's",	"ma'am",	"mayn't",	"might've",	"mightn't",	"mightn't've",	"must've",	"mustn't",	"mustn't've",	"needn't",	"needn't've",	"o'clock",	"oughtn't",	"oughtn't've",	"shan't",	"sha'n't",	"shan't've",	"she'd",	"she'd've",	"she'll",	"she'll've",	"she's",	"should've",	"shouldn't",	"shouldn't've",	"so've",	"so's",	"that'd",	"that'd've",	"that's",	"there'd",	"there'd've",	"there's",	"they'd",	"they'd've",	"they'll",	"they'll've",	"they're",	"they've",	"to've",	"wasn't",	"we'd",	"we'd've",	"we'll",	"we'll've",	"we're",	"we've",	"weren't",	"what'll",	"what'll've",	"what're",	"what's",	"what've",	"when's",	"when've",	"where'd",	"where's",	"where've",	"who'll",	"who'll've",	"who's",	"who've",	"why's",	"why've",	"will've",	"won't",	"won't've",	"would've",	"wouldn't",	"wouldn't've",	"y'all",	"y'all'd",	"y'all'd've",	"y'all're",	"y'all've",	"you'd",	"you'd've",	"you'll",	"you'll've",	"you're",	"you've"))

# Remove punctuation marks
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#The following code was written to remove the most common words after looking at the most common words after stemming that did not provide significant interpretation

docs <- tm_map(docs, removeWords,c(" ’s","“","“", "’ve"," ’ll"," ’d"," ’m","didn’t","don’t"," ’re","just","mean","like","yes","thanks","know","get","say","okay","first","mmhmm","well","’ve ","“","—","”","can’t ","â\u0080\u009c"))

#Creating a set of non-stemmed words for a word cloud
docsnostem <-docs

# Text stemming using Porter stemming
docs <- tm_map(docs, stemDocument)
```

## Term Frequency and Word Cloud

This produces an interactive wordcloud of the most frequent words (not stemmed).

```
#Term-document matrix for most frequent words NOT STEMMED:
tdm2 <- TermDocumentMatrix(docsnostem)
#Counting the Top 10 most frequent words
m2 <- as.matrix(tdm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)
#Wordcloud of these words
wordcloud <- wordcloud2(d2)
wordcloud
```
![Alt text](https://user-images.githubusercontent.com/34693652/34654213-b0bf8728-f3c6-11e7-97fb-d549db1762a0.png)

## LDA and Visualization of Topics

The following code selects the number of topics, performs Latent Dirichlet Allocation, and visualizes these topics in an interactive hierarchical tree.

```
#Document Term Matrix for LDA
docterm <- DocumentTermMatrix(docs)

#Selecting only the rows greated than 0
rowTotals <- apply(docterm , 1, sum) #Find the sum of words in each Document
ttdm.new   <- docterm[rowTotals> 0, ] 

#The following code helps select the number of topics for LDA
#Note that it can take awhile to run
result <- FindTopicsNumber(
  ttdm.new,
  topics = seq(from = 2, to = 62, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
#Choosing 40 topics

#Applying LDA to the documents
ap_lda2 <- LDA(ttdm.new, k = 40, control = list(seed = 1234))
chapter_topics <- tidy(ap_lda2, matrix = "beta")

#Applying LDA to the documents
ap_lda2 <- LDA(ttdm.new, k = 40, control = list(seed = 1234))
chapter_topics <- tidy(ap_lda2, matrix = "beta")

#Finding the top terms per topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(3, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

options(tibble.print_max=Inf)
top_terms

toptermsbytopic <- as.data.frame(top_terms)

cTree<-collapsibleTree(
  toptermsbytopic,
  hierarchy = c("topic", "term"),
  width = 500, height = 500, zoomable = FALSE, tooltip = TRUE
)
cTree
```

40 topics were selected. There is some overlap between the topics. Three example topics include:
* Topic 7 (school, college, money) may be about the importance of investing in education
* Topic 27 (famili, work, front) may be about the relationship of family and work
* Topic 33 (idea, meet, creativ) may be about coming up with creative ideas at meetings

A screenshot is below. See all the topics here: [linked phrase](file:///Users/emilyhadley/ctreecorneroffice.html)

![Alt text](https://user-images.githubusercontent.com/34693652/34654324-3a9b508e-f3c8-11e7-8401-36dd2e418312.png)

### Documents Related to a Specific Topic

To see the Top 2 documents related to each topic: 

```
#Seeing which documents are most closely related to a specific topic
ap_documents <- tidy(ap_lda2, matrix = "gamma")

top_docs <- ap_documents %>%
  group_by(topic) %>%
  top_n(3, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

top_docs
#Example interpretation: Doc 1349 is most closely related to topic 27
#What is Doc 1349?
#Doc 1034 in the cleaned data:
docs[[1349]]$content
#Doc 1034 in the original data:
newCorp1[[1349]]$content
#Doc 1034 with Date/Headline/URL:
newCorp2[[1349]]$content
```
For example, the document most closely associated with Topic 27 reads:
*(2015-09-20: Jonathan M. Tisch: Beware of the Thin Air at the Top [https://www.nytimes.com/2015/09/20/business/jonathan-m-tisch-beware-of-the-thin-air-at-the-top.html])  I was fortunate to have grown up as a young kid in the hotel business watching my father, Bob Tisch, and my uncle, Larry Tisch, create what is today the Loews Corporation. I have a recollection of being 5 years old, standing at the front door of the Traymore Hotel, greeting President Eisenhower. I was given instructions that I first salute him because he was a general, and then I shake his hand. I was so nervous I did them both at the same time.My cousins, my siblings and I were fully indoctrinated into the hotel business as children. In 1957, my father and uncle opened the Americana of Bal Harbour, and we would often go there on vacation. If I was bored, I would go behind the front desk at 7 years old. I would go into the kitchen. I would go into the laundry. I had free rein to walk around the property, and I took full advantage of that.My first paying job in the industry started when I was 15 years old, and I worked at the former Americana on Seventh Avenue in New York, a big hotel that my family built. I worked behind the front desk, and I used my middle name as my last name so that nobody knew that I was a member of the family that controlled Loews Hotels and the Loews Corporation.It said 'Jonathan Mark' on my name badge. On nights when the hotel was sold out, somebody might walk up and say, 'I have a reservation,' and I'd check. If they didn't have one, I'd say, 'No, sorry, there's no reservation.' Then they'd say, 'Well, I'm going to call the Tisch brothers. I know them very well. I know all three Tisch brothers.'Now, my father's name was Preston Robert and my uncle is Larry. So in one particular case, an individual thought that there were three Tisch brothers. He was going to call Preston, Robert or Larry. So I knew right away that he didn't know our family. "*

While some topics are more clear, others are not. Even in the clear topics, not all of the articles are accurately categorized. Further work is needed in this area.

## RShiny Application
The most common word in this analysis is 'people'. I was interested in providing a way for readers to learn what CEOs say about people. In the following Shiny application, clicking the button will provide the reader with a random answer from a CEO that includes the word 'people'. These answers are taken from the corpus that attaches the date, headline, and URL to each response.

```
shinyApp(
  ui = fluidPage(
    titlePanel("It's All About People"),
    verbatimTextOutput("text1"),
    actionButton("button1", "Show Me a Quote!"),
    verbatimTextOutput("text2")),
  
  server = function(input, output, session) {
    
    output$text1 <- renderText({
      session$userData$text1 <- "CEO Quotes that Include the Word 'People'"
      session$userData$text1})
    output$text2 <- renderText("Press the button to begin...")
    
    observeEvent(input$button1, {
      llll<-as.character(sample(people,1))
      llll <- str_replace_all(llll,"people","PEOPLE")
      output$text2 <- renderText(llll[1])
    })
  }
)
```
A screenshot of the application is below. The full interactive application can be accessed here 

![Alt text](https://user-images.githubusercontent.com/34693652/34654396-0677f09a-f3c9-11e7-87df-bb31004705a8.png)

## Acknowledgments

* The New York Times for access to the API and all of the wonderful source material
* http://brooksandrew.github.io/simpleblog/articles/new-york-times-api-to-mongodb/ for accessing NYT API
* https://www.ranks.nl/stopwords for the stop word list
* http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know for cleaning text in R and wordcloud fundamentals
* https://www.tidytextmining.com/topicmodeling.html for most common terms, LDA, and most common documents by topic
* https://adeelk93.github.io/collapsibleTree/ for a collapsible tree example
* https://shiny.rstudio.com/articles/basics.html for RShiny basics
* https://github.com/peridoteagle/markdown-cheatsheet for a template for this Markdown
