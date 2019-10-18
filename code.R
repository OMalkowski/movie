# Download data

###############################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Libraries
library(lubridate)
library(stringr)
library(ggplot2)

# Tidy data
edx%>%as_tibble()

# Unique users and movies
edx%>%summarize(n_users=n_distinct(userId),n_movies=n_distinct(movieId))

# Head (edx)
head(edx)

# Summarise (edx)
summary(edx)

# Check for missing data
any(is.na(edx))

# Add column - convert timestamp to date and year format
class(edx$timestamp)
edx<-edx%>%mutate(date=as.Date(as.POSIXlt(timestamp,origin="1970-01-01",format="%Y-%m-%d"),format="%Y-%m-%d"))
edx<-edx%>%mutate(year=format(date,"%Y"))
validation<-validation%>%mutate(date=as.Date(as.POSIXlt(timestamp,origin="1970-01-01",format="%Y-%m-%d"),format="%Y-%m-%d"))
validation<-validation%>%mutate(year=format(date,"%Y"))

# Add column for release year (separate year from the title)
edx<-edx%>%mutate(release_year=as.numeric(str_sub(title,-5,-2)))

# Split dataset by genres
edx_genre<-edx%>%separate_rows(genres,sep = "\\|")
edx_genre<-edx_genre%>%mutate(date=as.Date(as.POSIXlt(timestamp,origin="1970-01-01",format="%Y-%m-%d"),format="%Y-%m-%d"))
edx_genre<-edx_genre%>%mutate(year=format(date,"%Y"))
edx_genre<-edx_genre%>%mutate(release_year=as.numeric(str_sub(title,-5,-2)))
validation_genre<-validation%>%separate_rows(genres,sep = "\\|")
validation_genre<-validation_genre%>%mutate(date=as.Date(as.POSIXlt(timestamp,origin="1970-01-01",format="%Y-%m-%d"),format="%Y-%m-%d"))
validation_genre<-validation_genre%>%mutate(year=format(date,"%Y"))

# Display number of ratings per genre
ratings_genre <- edx_genre%>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
ratings_genre

# Rating distributions
options(scipen=999)
edx%>%ggplot(aes(rating))+geom_histogram(bins=10,fill="grey",color="black")+ ggtitle("Movie Count per Rating")+xlab("Rating") +ylab("Count")

# Plot to see which ratings were awarded each year
edx%>%ggplot(aes(year))+geom_point(aes(year,rating))+ ggtitle("Range of Ratings Awarded by Year")+xlab("Year") +ylab("Rating")

# Number of users who submitted ratings per year
options(scipen=999)
edx%>%group_by(year)%>%ggplot(aes(year, n_distinct(userId)))+geom_bar(stat="identity")+ggtitle("Number of users who submitted ratings per year")+xlab("Year")+ylab("User Count")

# Number of different movies rated per year
options(scipen=999)
edx%>%group_by(year)%>%ggplot(aes(year, n_distinct(movieId)))+geom_bar(stat="identity")+ggtitle("Number of movies rated per year")+xlab("Year")+ylab("Movie Count")

# Smooth plot of mean rating based on release year
edx %>% group_by(release_year)%>%summarize(rating=mean(rating)) %>%  ggplot(aes(release_year,rating))+geom_point()+geom_smooth()+ ggtitle("Mean Rating per Release Year")+xlab("Release Year") +ylab("Mean Rating")

# Genres per release year
options(scipen=999)
edx_genre%>%mutate(genres=as.factor(genres))%>%group_by(release_year,genres)%>%summarize(n=n())%>%ggplot(aes(release_year,n))+geom_line(aes(color=genres))+ ggtitle("Movies by Genre based on Release Year")+xlab("Release Year") +ylab("Count")

# Mean rating per genre
edx_genre%>%group_by(genres)%>%summarize(n=n(),avg=mean(rating),se=sd(rating/sqrt(n())))%>%mutate(genres=reorder(genres,avg))%>%ggplot(aes(genres,avg,ymax=avg+se,ymin=avg-se))+geom_point()+geom_errorbar()+theme(axis.text.x=element_text(angle=90,hjust=1))+ggtitle("Mean rating ± SE per Genre")+xlab("Genres")+ylab("Mean Rating")

# Summarize the data with no genre
edx_genre%>%filter(genres=="(no genres listed)")
        
# MovieID Count
edx%>%count(movieId)%>%ggplot(aes(n))+geom_histogram(bins = 30, fill="grey", color="black") + scale_x_log10() + ggtitle("Ratings per MovieID") + xlab("MovieID (log scale)") +ylab("Count")

# UserID Count
edx%>%count(userId)%>%ggplot(aes(n))+geom_histogram(bins = 30, fill="grey", color="black") + scale_x_log10() + ggtitle("Ratings per UserID") + xlab("UserID (log scale)") +ylab("Count")

# RMSE
RMSE <- function(true_ratings, predicted_ratings){ sqrt(mean((true_ratings - predicted_ratings)^2))}

# Partitioning the edx dataset into train and test sets
set.seed(1,sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE) 
train_set <- edx[-test_index,] # Create train set
test_set <- edx[test_index,] # Create test set

# Make sure `userId` and `movieId` in the test_set set are also in the train_set
test_set <- test_set %>% semi_join(train,by="movieId") %>% semi_join(train,by="userId")

# Model 1 - Naive Bayes
mu_hat <- mean(train_set$rating) 
mu_hat
naive_rmse <- RMSE(test_set$rating, mu_hat) 
naive_rmse
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

# Model 2 - Movie Effect
library(caret)
library(dslabs)
library(tidyverse)
library(stringr)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
predicted_ratings <- mu + test_set %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, test_set$rating) 
rmse_results <- bind_rows(rmse_results,tibble(method="Movie Effect Model", RMSE = model_1_rmse))
rmse_results

# Model 3 - Movie + User effect
train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>% filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")
user_avgs <- train_set %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
model_2_rmse <- RMSE(predicted_ratings, test_set$rating) 
rmse_results <- bind_rows(rmse_results, tibble(method="Movie + User Effects Model", RMSE = model_2_rmse))
rmse_results

# Model 4 - Regularization Movie Effect
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating) 
just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){ 
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by='movieId') %>% mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating)) 
})
qplot(lambdas, rmses) 
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,tibble(method="Regularized Movie Effect Model",RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Model 5 - Regularization Movie + User Effect
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)] 
lambda
rmse_results <- bind_rows(rmse_results,tibble(method="Regularized Movie + User Effect Model",RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Model 6 - Regularization Movie + User + Genre Effect
train_genre<-train_set%>%separate_rows(genres,sep = "\\|")
train_genre<-train_genre%>%mutate(date=as.Date(as.POSIXlt(timestamp,origin="1970-01-01",format="%Y-%m-%d"),format="%Y-%m-%d"))
train_genre<-train_genre%>%mutate(year=format(date,"%Y"))
train_genre<-train_genre%>%mutate(release_year=as.numeric(str_sub(title,-5,-2)))
test_genre<-test_set%>%separate_rows(genres,sep = "\\|")
test_genre<-test_genre%>%mutate(date=as.Date(as.POSIXlt(timestamp,origin="1970-01-01",format="%Y-%m-%d"),format="%Y-%m-%d"))
test_genre<-test_genre%>%mutate(year=format(date,"%Y"))
lambdas <- seq(0, 20, 1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_genre %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_genre %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_y <- train_genre %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l), n_y = n())
  b_g <- train_genre %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by = "year") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+l), n_g = n())
  predicted_ratings <-
    test_genre %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_y, by = "year") %>% left_join(b_g, by = "genres") %>% mutate(pred = mu + b_i + b_u + b_y + b_g) %>% pull(pred)
  return(RMSE(predicted_ratings, test_genre$rating))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)] 
lambda
rmse_results <- bind_rows(rmse_results,tibble(method="Regularized Movie + User + Genres Effect Model",RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Model 7 - Regularization Movie Effect (Validation)
lambdas <- seq(0, 10, 0.25)
mu <- mean(edx$rating) 
just_the_sum <- edx %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){ 
  predicted_ratings <- validation %>%
    left_join(just_the_sum, by='movieId') %>% mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating)) 
})
qplot(lambdas, rmses) 
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,tibble(method="Regularized Movie Effect Model",RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Model 8 - Regularization Movie + User Effect (Validation)
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)] 
lambda
rmse_results <- bind_rows(rmse_results,tibble(method="Regularized Movie + User Effect Model",RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Model 9 - Regularization Movie + User + Genre Effect (Validation)
lambdas <- seq(0, 20, 1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx_genre %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx_genre %>%
    left_join(b_i, by="movieId") %>% group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_y <- edx_genre %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l), n_y = n())
  b_g <- edx_genre %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by = "year") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+l), n_g = n())
  predicted_ratings <-
    validation_genre %>%
    left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_y, by = "year") %>% left_join(b_g, by = "genres") %>% mutate(pred = mu + b_i + b_u + b_y + b_g) %>% pull(pred)
  return(RMSE(predicted_ratings, validation_genre$rating))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)] 
lambda
rmse_results <- bind_rows(rmse_results,tibble(method="Regularized Movie + User + Genres Effect Model",RMSE = min(rmses)))
rmse_results %>% knitr::kable()


