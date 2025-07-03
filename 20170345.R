####################################### PART 1 ####################################

Install necessary packages
install.packages("forecast")
install.packages("gridExtra")
install.packages("VIM")
install.packages("tseries")
install.packages("Metrics")
install.packages("zoo")
install.packages("fpp")
install.packages("imputeTS")
install.packages("ggplot2")

# Load necessary libraries----
library(forecast)
library(imputeTS)
library(ggplot2)
library(gridExtra) 
library(imputeTS)
library(VIM)
library(stats)
library(tseries)
library(Metrics)
library(zoo)
library(fpp)
library(imputeTS)
library(ggplot2)


# Load the data----
pce_data <- read.csv("PCE.csv", stringsAsFactors = FALSE)

# set seed as last 3 digit of roll number for reproducibility
set.seed(345)

head(pce_data)
tail(pce_data)
summary(pce_data)

#Check structure of data
str(pce_data)

# Convert the DATE column to Date type
pce_data$DATE <- as.Date(pce_data$DATE, format="%d/%m/%Y")


# Created time series object----
# Since the data starts from January 1959 and is monthly, we set start=c(1959, 1)
# The frequency for monthly data is 12
pce_ts <- ts(pce_data$PCE, start=c(1959, 1), end =c(2023,11), frequency=12)
pce_ts

head(pce_ts)

plot(pce_ts, xlab="Time", ylab="PCE", main="Time Series of PCE")

# Checking for missing data----
sum(is.na(pce_ts))

# Impute missing values using the na.kalman function
pce_ts_imputed <- na_kalman(pce_ts)
pce_ts_imputed

# Checking for missing data
sum(is.na(pce_ts_imputed))

# Plot original time series
plot(pce_ts, type = "l", col = "red", ylim = range(pce_ts, pce_ts_imputed, na.rm = TRUE), ylab = "PCE", xlab = "Time", main = "Original vs. Imputed PCE Time Series")
# Add imputed time series to the plot
lines(pce_ts_imputed, type = "l", col = "blue")
legend("topright", legend = c("Original", "Imputed"), col = c("red", "blue"), lty = 1)

summary(pce_ts)
summary(pce_ts_imputed)

# Assuming the data is normally distributed, adjust the tests if not appropriate for your data
# Test for a difference in means
t_test_result <- t.test(pce_ts, pce_ts_imputed, na.action=na.exclude)

# Test for a difference in variance
var_test_result <- var.test(pce_ts, pce_ts_imputed, na.action=na.exclude)

print(t_test_result)
print(var_test_result)


hist(pce_ts, breaks=50, col=rgb(1,0,0,0.5), xlim=range(pce_ts, pce_ts_imputed, na.rm=TRUE), xlab="PCE", main="Histogram of Original vs. Imputed")
hist(pce_ts_imputed, breaks=50, col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("Original", "Imputed"), col=c("red", "blue"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# Additive Decompose the time series to observe the trend, seasonal, and irregular components----
pce_additive <- decompose(pce_ts_imputed, type = "additive")
plot(pce_additive)




### Trend----
plot(pce_ts_imputed)
ggplot(data = as.data.frame(pce_ts_imputed), aes(x = index(pce_ts_imputed), y = coredata(pce_ts_imputed))) +
  geom_line() +
  labs(title = "PCE Time Series", x = "Time", y = "PCE") +
  theme_minimal()

# Create a time index
time_index <- seq_along(pce_ts_imputed)

# Fit a linear model
model <- lm(pce_ts_imputed ~ time_index)

# Summary of the model to check for trend significance
summary(model)


### Seasonality----

ggseasonplot(pce_ts_imputed, main="Seasonal Plot of PCE", xlab="Month", ylab="PCE", year.labels=TRUE, year.labels.left=TRUE)

boxplot(split(pce_ts_imputed, cycle(pce_ts_imputed)), 
        names = month.abb, col = "lightgreen",
        main = "Seasonal Distribution of the Time Series")



### volatility----

# Analyzing volatility using standard deviation over a rolling window
pce_volatility <- rollapply(pce_ts_imputed, width=12, FUN=sd, by.column = TRUE, align='center', fill=NA)
# Plot volatility
plot(pce_volatility, type = "l", col = "blue", xlab = "Time", ylab = "Volatility", main = "Rolling Standard Deviation of PCE")

# BOX COX----
# First, find an optimal lambda for the Box-Cox Transformation
lambda <- BoxCox.lambda(pce_ts_imputed)
# Print the optimal lambda value
print(paste("Optimal lambda:", lambda))

# Apply the Box-Cox Transformation
pce_ts_transformed <- BoxCox(pce_ts_imputed, lambda)

# Plot the original and transformed data to compare
par(mfrow=c(2, 1))
plot(pce_ts_imputed, main="Original PCE Time Series")
plot(pce_ts_transformed, main=paste("Box-Cox Transformed PCE Time Series, Lambda =", round(lambda, 5)))

summary(pce_ts_imputed)
summary(pce_ts_transformed)

# Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF)----

tsdisplay(pce_ts_transformed)
plot(pce_ts_transformed)

pce_ts_diff_2 <- diff(pce_ts_transformed, differences = 2)
kpss_test_result <- kpss.test(pce_ts_diff_2)
kpss_test_result
tsdisplay(pce_ts_diff_2)

# Analyzing volatility using standard deviation over a rolling window
pce_volatility <- rollapply(pce_ts_diff_2, width=12, FUN=sd, by.column = TRUE, align='center', fill=NA)
# Plot volatility
plot(pce_volatility, type = "l", col = "blue", xlab = "Time", ylab = "Volatility", main = "Rolling Standard Deviation of PCE")

# Stationarity checks using ADF and KPSS tests

# adf
adf_test_result <- adf.test(pce_ts_imputed)
adf_test_result
# kpss
kpss_test_result <- kpss.test(pce_ts_imputed)
kpss_test_result

# 1st difference
pce_ts_diff_1 <- diff(pce_ts_imputed, differences = 1)

# adf
adf_test_result <- adf.test(pce_ts_diff_1)
adf_test_result
# kpss
kpss_test_result <- kpss.test(pce_ts_diff_1)
kpss_test_result

# 2nd difference
pce_ts_diff_2 <- diff(pce_ts_imputed, differences = 2)

# adf
adf_test_result <- adf.test(pce_ts_diff_2)
adf_test_result
# kpss
kpss_test_result <- kpss.test(pce_ts_diff_2)
kpss_test_result

# Plot the differences data
plot(pce_ts_diff_1, main="1st Differenced PCE Time Series", xlab="Time", ylab="Differenced PCE")

tsdisplay(pce_ts_diff_1)

# Plot the differences data
plot(pce_ts_diff_2, main="2nd Differenced PCE Time Series", xlab="Time", ylab="Differenced PCE")

tsdisplay(pce_ts_diff_2)

### Moving average with window size 2----
ma_pce <- ma(pce_ts_diff_2, order=2)

# Plotting the original differenced series with the moving average overlay
plot(pce_ts_diff_2, main="Original Differenced Series vs. Moving Average", xlab="Time", ylab="Differenced PCE", type="l")
lines(ma_pce, col="blue", lwd=2)  # thicker line for better visibility
legend("topleft", legend=c("Differenced Series", "Moving Average"), col=c("black", "blue"), lty=1, lwd=2)

# Display the time series properties using tsdisplay
# For the original differenced series
tsdisplay(pce_ts_diff_2, main="Time Series Display of 2nd Differenced PCE")

# For the moving average smoothed series
tsdisplay(ma_pce, main="Time Series Display of Moving Average Smoothed Series")

# Comparing the effect visually and statistically
print("Summary of Original Differenced Series:")
print(summary(pce_ts_diff_2))

print("Summary of Moving Average Applied Series:")
print(summary(ma_pce))



# Modelling----


# Split the data into training and test sets
train_data <- window(pce_ts_imputed, end = c(2010, 12))
test_data <- window(pce_ts_imputed, start = c(2011, 1))
percentage_test = length(test_data)/779*100
percentage_test

#Drift----
# Fit a drift model to the training data
drift_model <- rwf(train_data, drift=TRUE, lambda = 0.0277327677958033, h=length(test_data))
# Summary of the model
summary(drift_model)


# Plot the fitted model along with the actual data
plot(drift_model,ylim=c(0, 19000), main="Drift Model Fitting on PCE Data")
lines(test_data, col='red')
legend("topleft", legend=c("DRIFT", "Actual"), col=c("blue", "red"), lty=1)



# Check residuals
checkresiduals(drift_model)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(drift_model, test_data)
print(accuracy_metrics)


#Holt----
# Applying Holt's linear method to the train data
holt_model <- holt(train_data, lambda = 0.0277327677958033, h=length(test_data))

# Check the model summary
summary(holt_model)

# Forecasting using the Holt model
holt_forecast <- forecast(holt_model, h=length(test_data))

# Plot the training data, forecasts, and the actual test data
plot(holt_forecast, main="Holt's Linear Method Forecast vs Actual")
lines(test_data, col = "red", lwd = 2)
legend("topleft", legend=c("Holt Linear (A,N)", "Actual"), col=c("blue", "red"), lty=1, lwd=2)

# Check residuals
checkresiduals(holt_model)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(holt_forecast, test_data)
print(accuracy_metrics)


#Arima----

# Fit a arima model to the training data

# 2nd difference on train data
pce_ts_diff_2 <- diff(train_data, differences = 2)


tsdisplay(pce_ts_diff_2)


arima_model <- Arima(train_data, lambda = 0.0277327677958033,order = c(3,2,2))
arima_fc <- forecast(arima_model, h=length(test_data))

# Summary of the model
summary(arima_model)
plot(arima_fc)

# Plot the fitted model along with the actual data
plot(arima_fc, ylim =c(0,19000), main="Arima Model Fitting on PCE Data")
lines(test_data, col='red')
legend("topleft", legend=c("ARIMA", "Actual"), col=c("blue", "red"), lty=1)

# Check residuals
checkresiduals(arima_fc)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(arima_fc, test_data)
print(accuracy_metrics)

# Auto-Arima----

# Fit a arima model to the training data
auto_arima_model <- auto.arima(train_data , lambda = 0.0277327677958033)
auto_arima_fc <- forecast(auto_arima_model, h=length(test_data))

# Summary of the model
summary(auto_arima_model)

plot(auto_arima_fc)
# Plot the fitted model along with the actual data
plot(auto_arima_fc, main="Auto-Arima Model Fitting on PCE Data")
lines(test_data, col='red')
legend("topleft", legend=c("Fitted", "Actual"), col=c("blue", "red"), lty=1)

# Check residuals
checkresiduals(auto_arima_fc)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(auto_arima_fc, test_data)
print(accuracy_metrics)


# OCT2024 USING ARIMA
best_arima_model <- Arima(train_data, lambda = 0.0277327677958033,order = c(3,2,2))
arima_fc <- forecast(arima_model, h=11)
summary(best_arima_model)
summary(arima_fc)
# Check the model summary

#rwf# Display the forecast for October 2024 specifically
oct_2024_forecast <- arima_fc$mean[11]  # Assuming the forecast starts from January 2024
print(paste("Forecasted PCE for October 2024: ", oct_2024_forecast))

# Final fit model using Holt to predict oct 2024 ----

best_holt_model <- holt(pce_ts_imputed, h=11, lambda = 0.0277327677958033)
summary(best_holt_model)
# Check the model summary

#rwf# Display the forecast for October 2024 specifically
oct_2024_forecast <- best_holt_model$mean[11]  # Assuming the forecast starts from January 2024
print(paste("Forecasted PCE for October 2024: ", oct_2024_forecast))


# Using auto.arima to find the best fit-------
arima_model <- auto.arima(pce_ts_imputed)

# Summary of the ARIMA model
summary(arima_model)

# Plot the fitted model along with the actual data
plot(forecast(arima_model, h=length(test_data)), main="ARIMA Model Forecast vs Actual Data")
lines(test_data, col='red')
legend("topleft", legend=c("Fitted", "Actual"), col=c("blue", "red"), lty=1)

# Check residuals
checkresiduals(arima_model)

# Predicting October 2024 Using Auto Arima-----

# Fit the ARIMA model to the entire dataset (pce_ts_imputed)
best_arima_model <- auto.arima(pce_ts_imputed)

# Summary of the best ARIMA model
summary(best_arima_model)

# Forecast future values
future_forecast <- forecast(best_arima_model, h=12)  # Forecasting for 12 months ahead
future_forecast



# Display the forecast for October 2024 specifically
oct_2024_forecast <- future_forecast$mean[11]  # Assuming the forecast starts from January 2024
print(paste("Forecasted PCE for October 2024: ", oct_2024_forecast))


# Plot compare models-------

autoplot(pce_ts_imputed) +
  autolayer(drift_model$mean) +
  autolayer(holt_model$mean) +
  autolayer(arima_fc$mean) +
  labs(title="Forecast Comparison of all models", x="Time", y="PCE") 


# One-step ahead rolling forecasting without re-estimation-----

# Drift Model - Rolling Forecast
fit_drift <- rwf(train_data, drift=TRUE, lambda = 0.0277327677958033)
refit_drift <- rwf(pce_ts_imputed, model=fit_drift$model)
fc_drift <- window(fitted(refit_drift), start=c(2011,1))

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(fc_drift, test_data)
print(accuracy_metrics)

# Holt linear - Rolling Forecast
fit_holt <- holt(pce_ts_imputed, lambda = 0.0277327677958033)
refit_holt <- holt(pce_ts_imputed, model=fit_holt)
fc_holt <- window(fitted(refit_holt), start=c(2011,1))

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(fc_holt, test_data)
print(accuracy_metrics)

# ARIMA - Rolling Forecast
fit_arima <- auto.arima(train_data, lambda = 0.0277327677958033)
refit_arima <- Arima(pce_ts_imputed, model=fit_arima)
fc_arima <- window(fitted(refit_arima), start=c(2011,1))

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(fc_arima, test_data)
print(accuracy_metrics)

# Plotting the forecasts against the actual data
plot(pce_ts_imputed, main="One-step Rolling Forecasts vs Actual Data", col="grey", lwd=1, ylim=c(min(pce_ts_imputed, fc_drift, fc_holt, fc_arima), max(pce_ts_imputed, fc_drift, fc_holt, fc_arima)))
lines(fc_drift, col="red", lwd=2)
lines(fc_holt, col="blue", lwd=2)
lines(fc_arima, col="green", lwd=2)
legend("topleft", legend=c("Actual", "Drift", "Holt Linear", "ARIMA"), col=c("grey", "red", "blue", "green"), lty=1, lwd=2)

# Plotting only the test period for clarity
plot(test_data, main="Test Period: One-step Rolling Forecasts vs Actual Data", col="grey", lwd=1)
lines(fc_drift, col="red", lwd=2)
lines(fc_holt, col="blue", lwd=2)
lines(fc_arima, col="green", lwd=2)
legend("topleft", legend=c("Actual", "Drift", "Holt Linear", "ARIMA"), col=c("grey", "red", "blue", "green"), lty=1, lwd=2)


####################################### PART 2 ########################################################


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
ldaOut.topics$Topic <- factor(ldaOut.topics$Topic, levels = 1:7, labels = topic_labels)

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