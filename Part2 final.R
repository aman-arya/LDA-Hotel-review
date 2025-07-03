# Install important packages
required_packages <- c("dplyr", "tm", "SnowballC", "topicmodels", "stringr", "cld3", "ldatuning", "ggplot2", "textstem", "hunspell")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
install.packages("cld3")
# Load necessary libraries
library(dplyr) # basic data manipulation
library(tm) # package for text mining package




library(stringr) # package for dealing with strings
library(RColorBrewer)# package to get special theme color
library(wordcloud) # package to create wordcloud
library(topicmodels) # package for topic modelling
library(ggplot2) # basic data visualization
library(LDAvis) # LDA specific visualization 
library(servr) # interactive support for LDA visualization
library(tokenizers)
library(cld3)
library(textstem)
library(wordcloud)
library(ldatuning)
library(ggplot2)

# Set working directory
setwd("/Users/amanarya/Documents/LUBS/BADS/Sem 2/Forecasting and Advanced Business Analytics/Assignment/Assignment Forecast")

# Load the dataset
reviews <- read.csv("HotelsData.csv", stringsAsFactors = FALSE)

summary(reviews)
# Rename the columns
colnames(reviews) <- c("Rating", "Review")



# Set seed for reproducibility based on student ID example
set.seed(345)
# Detect languages for all reviews
reviews$lang <- sapply(reviews$Review, function(x) {
  if (!is.na(x) && nzchar(x)) {
    detect_language(x)
  } else {
    NA
  }
})

# View summary of languages detected
language_counts <- table(reviews$lang)


# Convert to data frame for plotting
language_data <- as.data.frame(language_counts)
colnames(language_data) <- c("Language", "Count")

# Order and get top 5 languages
top_languages <- language_data %>% 
  arrange(desc(Count)) %>% 
  top_n(5, Count)

# Plotting the top 5 languages
ggplot(top_languages, aes(x = Language, y = Count, fill = Language)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 5 Languages in Hotel Reviews", x = "Language", y = "Number of Reviews") +
  scale_fill_brewer(palette = "Pastel1")  # Adds color to the bars

# Filter English reviews and sample 2000 reviews
eng_reviews <- subset(reviews, detect_language(reviews$Review) == "en")
sample_reviews <- sample_n(eng_reviews, size = 2000)

# Save sample reviews
write.csv(sample_reviews, file = "sample_reviews.csv", row.names = TRUE)

# Load the sample reviews dataset
sample_reviews <- read.csv("sample_reviews.csv", stringsAsFactors = FALSE)

# Preprocess the text data
corpus <- Corpus(VectorSource(sample_reviews$Review))

# Define preprocessing functions
#removing special char
remove_special_chars <- function(text) {
  gsub("[^[:alnum:] ]", "", text)
}

replace_accented_chars <- function(text) {
  iconv(text, to = "ASCII//TRANSLIT")
}

# Apply preprocessing steps
corpus <- tm_map(corpus, content_transformer(function(x) gsub("(f|ht)tps?://\\S+|www\\.\\S+", "", x))) #url
corpus <- tm_map(corpus, content_transformer(function(x) gsub("([a-z])([A-Z])", "\\1 \\2", x))) #joined words
corpus <- tm_map(corpus, content_transformer(remove_special_chars))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, content_transformer(replace_accented_chars))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, lemmatize_strings)

# Extract preprocessed text and combine with Rating column
cleaned_text <- sapply(corpus, as.character)
cleaned_data <- data.frame(Rating = sample_reviews$Rating, Review = cleaned_text)

# Save the cleaned data to CSV
write.csv(cleaned_data, file = "cleaned_data.csv", row.names = FALSE)

# Subset data into positive and negative reviews
postive_data <- subset(cleaned_data, cleaned_data$Rating >= 4)
negative_data <- subset(cleaned_data, cleaned_data$Rating <= 3)
# Create separate corpora for positive and negative reviews
postive_review <- Corpus(VectorSource(postive_data$Review))
negative_review <- Corpus(VectorSource(negative_data$Review))

# Summarize data to count the number of reviews per rating
rating_counts <- table(reviews$Rating)

# Convert the table to a data frame for plotting
rating_df <- as.data.frame(rating_counts)
names(rating_df) <- c("Rating", "Count")

# Plot the distribution of ratings
ggplot(rating_df, aes(x = Rating, y = Count, fill = Rating)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Blues") +  # Using a blue color palette
  labs(title = "Distribution of Ratings", x = "Rating", y = "Number of Reviews") +
  theme_minimal()

# Create document-term matrices (DTMs) for positive and negative reviews
postive_dtm <- DocumentTermMatrix(postive_review)
postive_dtm
postive_dtm <- removeSparseTerms(postive_dtm, 0.99)
postive_dtm 
negative_dtm <- DocumentTermMatrix(negative_review)
negative_dtm  <- removeSparseTerms(negative_dtm, 0.99)
negative_dtm 

# Find most frequent words in positive and negative reviews
postive_freq <- findFreqTerms(postive_dtm, 200)
negative_freq <- findFreqTerms(negative_dtm, 200)
postive_freq 
negative_freq

# Create word clouds for positive and negative reviews
postive_freq_df <- data.frame(sort(colSums(as.matrix(postive_dtm)), decreasing = TRUE))
negative_freq_df <- data.frame(sort(colSums(as.matrix(negative_dtm)), decreasing = TRUE))

wordcloud(rownames(postive_freq_df), postive_freq_df[,1], max.words = 50, colors = brewer.pal(1, "Dark2"))
wordcloud(rownames(negative_freq_df), negative_freq_df[,1], max.words = 50, colors = brewer.pal(1, "Dark2"))

# Apply TF-IDF weighting to positive and negative reviews
postive_dtm_tfidf <- DocumentTermMatrix(postive_review, control = list(weighting = weightTfIdf))
negative_dtm_tfidf <- DocumentTermMatrix(negative_review, control = list(weighting = weightTfIdf))

postive_freq_tfidf <- data.frame(sort(colSums(as.matrix(postive_dtm_tfidf)), decreasing = TRUE))
negative_freq_tfidf <- data.frame(sort(colSums(as.matrix(negative_dtm_tfidf)), decreasing = TRUE))

wordcloud(rownames(postive_freq_tfidf), postive_freq_tfidf[,1], max.words = 50, colors = brewer.pal(1, "Dark2"))
wordcloud(rownames(negative_freq_tfidf), negative_freq_tfidf[,1], max.words = 50, colors = brewer.pal(1, "Dark2"))

# Topic modeling-----


# Postive-----
postive_dtm_matrix <- as.matrix(postive_dtm)
frequency <- colSums(postive_dtm_matrix)
frequency <- sort(frequency, decreasing = TRUE)
doc_length <- rowSums(postive_dtm_matrix)

# Determine optimal number of topics
result <- FindTopicsNumber(
  postive_dtm_matrix,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 345),
  mc.cores = 1L,
  verbose = TRUE
)



FindTopicsNumber_plot(result)



# Perform LDA with the optimal number of topics
ldaOut <- LDA(postive_dtm_matrix, 7, method = "Gibbs", control = list(iter = 1000, seed = 1000))
ldaOut
phi <- posterior(ldaOut)$terms %>% as.matrix 
#matrix, with each row containing the distribution over terms for a topic,
theta <- posterior(ldaOut)$topics %>% as.matrix 

#matrix, with each row containing the probability distribution over topics for a document,


# Extract top terms for each topic
ldaOut.terms <- as.matrix(terms(ldaOut, 10)) 
ldaOut.terms

#Namiing of topics
topic_postive_label <- c("Service Quality", "Room Comfort", "Location and Accessibility", "Guest Experience", "Facilities and Services", "Booking and Management", "Value and Dining")
colnames(ldaOut.terms) <- topic_postive_label
ldaOut.terms

# Extract topics from LDA output
ldaOut.topics <- data.frame(Topic = topics(ldaOut))

# Add an index to link back to the original reviews
ldaOut.topics$index <- as.numeric(row.names(ldaOut.topics))

# Rename topics based on predefined labels
ldaOut.topics$Topic <- factor(ldaOut.topics$Topic, levels = 1:7, labels = topic_postive_label)

# Ensure the reviews data also has an index for merging
postive_data$index <- as.numeric(row.names(postive_data))
# Merge the topics with the reviews data
datawithtopic <- merge(postive_data, ldaOut.topics, by = 'index', all.x = TRUE)

# Order the merged data by index to maintain original order
datawithtopic <- datawithtopic[order(datawithtopic$index), ]

# Display or inspect the first few rows to verify correct merging and labeling
head(datawithtopic)



# Extract topic probabilities for each review
topicProbabilities <- as.data.frame(ldaOut@gamma)
# Set the column names of the topic probabilities data frame to the topic labels
colnames(topicProbabilities) <- topic_postive_label

topicProbabilities[0:10,]


# Calculate the mean topic probabilities across all documents
topic_mean_probabilities <- colMeans(theta)
# Convert the topic_mean_probabilities vector to a named vector
names(topic_mean_probabilities) <- topic_postive_label

# Order topics by their mean probabilities
ordered_topics <- sort(topic_mean_probabilities, decreasing = TRUE)

# Get names of the top 3 topics
top_3_topics <- names(ordered_topics)[1:3]

# Print the names and their probabilities
print(top_3_topics)
print(ordered_topics[1:3])

# Sort topics by their mean probabilities
ordered_topics <- sort(topic_mean_probabilities, decreasing = TRUE)

# Perform min-max scaling
scaled_topics <- (ordered_topics - min(ordered_topics)) / (max(ordered_topics) - min(ordered_topics))

# Create a data frame for plotting
topics_df <- data.frame(Topic = names(scaled_topics), Probability = scaled_topics)

# Plot using ggplot
ggplot(topics_df, aes(x = reorder(Topic, Probability), y = Probability, fill = Topic)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes for better visualization of text
  labs(title = "Normalized Topic Probabilities", x = "Topic", y = "Normalized Probability") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +  # Color palette for differentiation
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text for clarity



# Create JSON object for LDAvis visualization
vocab <- colnames(phi)
json_lda <- createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc_length, term.frequency = frequency)

# Visualize the topics using LDAvis
serVis(json_lda, out.dir = 'vis', open.browser = TRUE)


# Negative-----

negative_dtm_matrix <- as.matrix(negative_dtm)
frequency <- colSums(negative_dtm_matrix)
frequency <- sort(frequency, decreasing = TRUE)
doc_length <- rowSums(negative_dtm_matrix)

# Determine optimal number of topics
result <- FindTopicsNumber(
  negative_dtm_matrix,
  topics = seq(from = 2, to = 7, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 345),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Perform LDA with the optimal number of topics
ldaOut <- LDA(negative_dtm, 7, method = "Gibbs", control = list(iter = 1000, seed = 1000))

phi <- posterior(ldaOut)$terms %>% as.matrix
theta <- posterior(ldaOut)$topics %>% as.matrix

# Extract top terms for each topic
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
ldaOut.terms


#Namiing of topics
topic_negative_label <- c("Transportation and Location","Customer Service and Staff",  "Sleep Quality and Noise", "Room Quality and Amenities", "Booking and Administration",  "Food and Dining",  "Cost and Value")
#Namiing of topics
colnames(ldaOut.terms) <- topic_negative_label
ldaOut.terms

# Extract topics from LDA output
ldaOut.topics <- data.frame(Topic = topics(ldaOut))

# Add an index to link back to the original reviews
ldaOut.topics$index <- as.numeric(row.names(ldaOut.topics))

# Rename topics based on predefined labels
ldaOut.topics$Topic <- factor(ldaOut.topics$Topic, levels = 1:7, labels = topic_negative_label)

# Ensure the reviews data also has an index for merging
negative_data$index <- as.numeric(row.names(negative_data))
# Merge the topics with the reviews data
datawithtopic <- merge(negative_data, ldaOut.topics, by = 'index', all.x = TRUE)

# Order the merged data by index to maintain original order
datawithtopic <- datawithtopic[order(datawithtopic$index), ]

# Display or inspect the first few rows to verify correct merging and labeling
head(datawithtopic)



# Extract topic probabilities for each review
topicProbabilities <- as.data.frame(ldaOut@gamma)
# Set the column names of the topic probabilities data frame to the topic labels
colnames(topicProbabilities) <- topic_negative_label

topicProbabilities[0:10,]


# Calculate the mean topic probabilities across all documents
topic_mean_probabilities <- colMeans(theta)
# Convert the topic_mean_probabilities vector to a named vector
names(topic_mean_probabilities) <- topic_negative_label

# Order topics by their mean probabilities
ordered_topics <- sort(topic_mean_probabilities, decreasing = TRUE)

# Get names of the top 3 topics
top_3_topics <- names(ordered_topics)[1:3]

# Print the names and their probabilities
print(top_3_topics)
print(ordered_topics[1:3])

# Sort topics by their mean probabilities
ordered_topics <- sort(topic_mean_probabilities, decreasing = TRUE)

# Perform min-max scaling
scaled_topics <- (ordered_topics - min(ordered_topics)) / (max(ordered_topics) - min(ordered_topics))

# Create a data frame for plotting
topics_df <- data.frame(Topic = names(scaled_topics), Probability = scaled_topics)

# Plot using ggplot
ggplot(topics_df, aes(x = reorder(Topic, Probability), y = Probability, fill = Topic)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes for better visualization of text
  labs(title = "Normalized Topic Probabilities", x = "Topic", y = "Normalized Probability") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +  # Color palette for differentiation
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text for clarity



# Create JSON object for LDAvis visualization
vocab <- colnames(phi)
json_lda <- createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc_length, term.frequency = frequency)

# Visualize the topics using LDAvis
serVis(json_lda, out.dir = 'vis', open.browser = TRUE)
