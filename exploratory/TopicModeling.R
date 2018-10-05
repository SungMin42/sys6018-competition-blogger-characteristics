library(XML)
library(tm)
library(SnowballC)
library(topicmodels)

# Subsetting because data is too large
train.blogs.clean1 = train.blogs.clean[sample(1:length(train.blogs.clean), 5000)]

# compute TF
blogs.clean.tf = DocumentTermMatrix(train.blogs.clean1, control = list(weighting = weightTf))

# Getting rid of empty rows
row.sums = apply(blogs.clean.tf, 1, sum)
train.blogs.clean1 = train.blogs.clean1[row.sums > 0]
blogs.clean.tf = blogs.clean.tf[row.sums > 0,]

# train topic model with 10 topics
topic.model = LDA(blogs.clean.tf, 10)

# look at the top (highest probability) 10 words within the first 5 topics
terms(topic.model, 10)[,1:5]

# look at the top (highest probability) 5 topics within the first 10 documents
topics(topic.model, 5)[,1:10]

# group documents by most likely topic and look at one of the document groups
document.most.likely.topic = topics(topic.model, 1)
document.topic.clusters = split(blogs.clean.tf, document.most.likely.topic)

# each element in the list returned by split is a subcorpus of documents that have the same most likely topic
document.topic.clusters[[1]]  # subcorpus of documents whose most likely topic is "1"

# print documents within the first few topic clusters
for(cluster.num in 1:3)
{
  # print the first 2 documents in the current cluster
  num.docs.in.cluster = length(document.topic.clusters[[cluster.num]])
  for(doc.num in 1:min(2,num.docs.in.cluster))
  {
    print(paste("Cluster", cluster.num, ", Document", doc.num, ": ", document.topic.clusters[[cluster.num]][[doc.num]]$content))
  }
}

# gamma contains the document-topic matrix...what might we do with it?
topic.model@gamma[1:5,]

# infer the topic probabilities of new text using the topic model we estimated above -- why is this important when using topic probabilities as predictors?
testing.documents = df.test[, c("post.id", "text")]
names(testing.documents) = c("doc_id", "text")
testing.corpus = VCorpus(DataframeSource(testing.documents))
testing.corpus = tm_map(testing.corpus, stripWhitespace)                    # remove extra whitespace
testing.corpus = tm_map(testing.corpus, removeNumbers)                      # remove numbers
testing.corpus = tm_map(testing.corpus, removePunctuation)                  # remove punctuation
testing.corpus = tm_map(testing.corpus, content_transformer(tolower))       # ignore case
testing.corpus = tm_map(testing.corpus, removeWords, stopwords("english"))  # remove stop words
testing.corpus = tm_map(testing.corpus, stemDocument)                       # stem all words
testing.corpus.tf = DocumentTermMatrix(testing.corpus, control = list(weighting = weightTf))
inferred_probabilities = posterior(topic.model, testing.corpus.tf)
inferred_probabilities$topics

# cluster documents in topic space
document.topic.probabilities = topic.model@gamma  # topic distribution for each document
topic.space.kmeans.clusters = kmeans(document.topic.probabilities, 10)
topic.space.clustered.blogs = split(blogs, topic.space.kmeans.clusters$cluster)
topic.space.clustered.blogs[[1]][[1]]$content
topic.space.clustered.blogs[[1]][[2]]$content
topic.space.clustered.blogs[[1]][[3]]$content
