# Init
libs <- c("googlesheets", "dplyr", "tm", "class", "wordcloud")
lapply(libs, require, character.only = TRUE)

# Set parameters
scores <- c(0,1)

# Read in data
mysheet <- gs_key('1_7AHbbB18loYZ239AQWS4vy-dfHDVmOl3KDPs4m3lQk')
mydata <- mysheet %>% gs_read(ws = "Data")
mydata <- mydata[c(12, 13)]
cols <- c("Reacties","Score")
colnames(mydata) <- cols


# Clean text
newdata <- na.omit(mydata)
newdata$Score <- as.factor(newdata$Score)
summary(newdata)

cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus, stripWhitespace)
  corpus.tmp <- tm_map(corpus, tolower)
  corpus.tmp <- tm_map(corpus, removeWords, stopwords("dutch"))
  return(corpus.tmp)
  
}

# Build TDM
generateTDM <- function(score, df) {
  df <- filter(df, Score == score)
  s.cor <- Corpus(VectorSource(df[[1]]))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  
  s.tdm <- removeSparseTerms(s.tdm, 0.8)
  result <- list(score = score, tdm = s.tdm)
  
}
  
tdm <- lapply(factor(c(0,1)), generateTDM, df = newdata)

# Attach score
bindScoreToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  
  s.df <- cbind(s.df, rep(tdm[["score"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "Score"
  return(s.df)
  
}

scoreTDM <- lapply(tdm, bindScoreToTDM)

#Stack
tdm.stack <- do.call(rbind_list, scoreTDM)
tdm.stack[is.na(tdm.stack)] <- 0

# Hold-out
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack)*0.7))
test.idx <- (1:nrow(tdm.stack))[- train.idx]

# Model - KNN
tdm.score <- tdm.stack[, "Score"][[1]]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "Score"]

knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.score[train.idx])

# Accuracy
conf.mat <- table(Predictons = knn.pred, Actual = tdm.score[test.idx])
accuracy <- 100*sum(diag(conf.mat))/length(test.idx)

# Wordcloud
# create a corpus
mach_corpus = Corpus(VectorSource(newdata[newdata$Score == 1,][[1]]))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("casus", "model", "week", "stof", "goed",stopwords("dutch")),
                                        removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, scale = c(2, 0.2), colors=brewer.pal(8, "Dark2"))
