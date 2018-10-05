library(XML)
library(tm)

##### Constructing TF-IDF Matrices #####

# read some news data from an XML file and transform it into a corpus. the following
# data frame will have three columns:  id (document identifier), t (title), date (rough date)
# and c (content).
document.data.frame = xmlToDataFrame("Data/news_documents.xml", stringsAsFactors = FALSE)
document.data.frame[1:2,]

# for the purpose of this example, we only care about content...keep the id and c columns
document.data.frame = as.data.frame(document.data.frame[,c("id", "c")], stringsAsFactors = FALSE)

# the tm package expects the columns to have specific names
names(document.data.frame) = c("doc_id", "text")

# convert the data frame to a volatile (in-memory) corpus object.
news = VCorpus(DataframeSource(document.data.frame))

# regular indexing returns a sub-corpus
inspect(news[1:2])

# double-indexing accesses actual documents
news[[1]]
news[[1]]$content

# compute TF-IDF matrix and inspect sparsity
news.tfidf = DocumentTermMatrix(news, control = list(weighting = weightTfIdf))
news.tfidf  # non-/sparse entries indicates how many of the DTM cells are non-zero and zero, respectively.
            # sparsity is number of non-zero cells divided by number of zero cells.

# inspect sub-matrix:  first 5 documents and first 5 terms
news.tfidf[1:5,1:5]
as.matrix(news.tfidf[1:5,1:5])

##### Reducing Term Sparsity #####

# there's a lot in the documents that we don't care about. clean up the corpus.
news.clean = tm_map(news, stripWhitespace)                          # remove extra whitespace
news.clean = tm_map(news.clean, removeNumbers)                      # remove numbers
news.clean = tm_map(news.clean, removePunctuation)                  # remove punctuation
news.clean = tm_map(news.clean, content_transformer(tolower))       # ignore case
news.clean = tm_map(news.clean, removeWords, stopwords("english"))  # remove stop words
news.clean = tm_map(news.clean, stemDocument)                       # stem all words

# compare original content of document 1 with cleaned content
news[[1]]$content
news.clean[[1]]$content  # do we care about misspellings resulting from stemming?

# recompute TF-IDF matrix using the cleaned corpus
news.clean.tfidf = DocumentTermMatrix(news.clean, control = list(weighting = weightTfIdf))

# reinspect the first 5 documents and first 5 terms
news.clean.tfidf[1:5,1:5]
as.matrix(news.clean.tfidf[1:5,1:5])

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(news.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
tfidf.99
as.matrix(tfidf.99[1:5,1:5])

tfidf.70 = removeSparseTerms(news.clean.tfidf, 0.70)  # remove terms that are absent from at least 70% of documents
tfidf.70
as.matrix(tfidf.70[1:5, 1:5])
news.clean[[1]]$content

# calculate inter-document similarity (euclidean)
dtm.tfidf.99 = as.matrix(tfidf.99)
dtm.dist.matrix = as.matrix(dist(dtm.tfidf.99))

# inspect documents most similar to (i.e., smallest distance from) the first document
most.similar.documents = order(dtm.dist.matrix[1,], decreasing = FALSE)
news[[most.similar.documents[1]]]$content
news[[most.similar.documents[2]]]$content
news[[most.similar.documents[3]]]$content
news[[most.similar.documents[4]]]$content
news[[most.similar.documents[5]]]$content
