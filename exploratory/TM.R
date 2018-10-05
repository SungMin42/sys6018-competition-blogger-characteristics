# The following code is taken from Matthew Gerber's TM code for the lectures

library(XML)
library(tm)
library(SnowballC)

##### Constructing TF-IDF Matrices #####

df.train.blogs = df.train
df.train.blogs = df.train.blogs[,c("post.id", "text")]
names(df.train.blogs) = c("doc_id", "text")
train.blogs = VCorpus(DataframeSource(df.train.blogs))

# regular indexing returns a sub-corpus
inspect(train.blogs[1:2])

# double-indexing accesses actual documents
train.blogs[[1]]
train.blogs[[100]]$content

# compute TF-IDF matrix and inspect sparsity
blogs.tfidf = DocumentTermMatrix(train.blogs, control = list(weighting = weightTfIdf))
blogs.tfidf  # non-/sparse entries indicates how many of the DTM cells are non-zero and zero, respectively.
            # sparsity is number of non-zero cells divided by number of zero cells.

# there's a lot in the documents that we don't care about. clean up the corpus.
train.blogs.clean = tm_map(train.blogs, stripWhitespace)                          # remove extra whitespace
train.blogs.clean = tm_map(train.blogs.clean, removeNumbers)                      # remove numbers
train.blogs.clean = tm_map(train.blogs.clean, removePunctuation)                  # remove punctuation
train.blogs.clean = tm_map(train.blogs.clean, content_transformer(tolower))       # ignore case
train.blogs.clean = tm_map(train.blogs.clean, removeWords, stopwords("english"))  # remove stop words
train.blogs.clean = tm_map(train.blogs.clean, stemDocument)                       # stem all words

# recompute TF-IDF matrix using the cleaned corpus
blogs.clean.tfidf = DocumentTermMatrix(train.blogs.clean, control = list(weighting = weightTfIdf))
blogs.clean.tfidf

# reinspect the first 5 documents and first 5 terms
blogs.clean.tfidf[1:5,1:5]
as.matrix(blogs.clean.tfidf[1:5,1:5])

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(blogs.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
tfidf.99
as.matrix(tfidf.99[1:5,1:5])

tfidf.70 = removeSparseTerms(blogs.clean.tfidf, 0.70)  # remove terms that are absent from at least 70% of documents
tfidf.70
as.matrix(tfidf.70[1:5, 1:5])

# # calculate inter-document similarity (euclidean)
# dtm.tfidf.99 = as.matrix(tfidf.99)
# dtm.dist.matrix = as.matrix(dist(dtm.tfidf.99))

# # inspect documents most similar to (i.e., smallest distance from) the first document
# most.similar.documents = order(dtm.dist.matrix[1,], decreasing = FALSE)
# blogs[[most.similar.documents[1]]]$content
# blogs[[most.similar.documents[2]]]$content
# blogs[[most.similar.documents[3]]]$content
# blogs[[most.similar.documents[4]]]$content
# blogs[[most.similar.documents[5]]]$content
