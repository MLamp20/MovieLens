##########################################################
#Movielens Project – Build Movie Recommendation System
##########################################################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
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

save(edx, file = "projects/data/rda/edx.rda")
save(validation, file = "projects/data/rda/validation.rda")

####################################################################
# Exploratory Data Analysis on Edx pre-split into train_edx/test_edx
####################################################################
# View structure for class, naming conventions, variable names
str(edx)
dim(edx)
dim(validation)

edx %>% 
 summarize(n_movies = n_distinct(movieId), n_users = n_distinct(userId), n_genres = n_distinct(genres))

edx %>% summarize(mean_rating_count_per_movie = round(nrow(edx)/n_distinct(movieId)), 
                  mean_rating_count_per_user = round(nrow(edx)/n_distinct(userId)), 
                  mean_rating_count_per_genre = round(nrow(edx)/n_distinct(genres)))

options(digits=3)

c(mean_edx_rating = mean(edx$rating),sd_edx_rating = sd(edx$rating))

z<-scale(edx$rating)
two_sd<-mean(abs(z)<2)
two_sd

# Plot distribution of how many times a movie gets rated
edx %>% dplyr::count(movieId) %>%
  ggplot(aes(n)) + geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + ggtitle("Movies - Quantity of Ratings")

ggsave("projects/figs/movieratingcthist.png")

# Plot distribution of how many movie users rate
edx %>% dplyr::count(userId) %>% 
  ggplot(aes(n)) + geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + ggtitle("Users - Quantity of Ratings")

ggsave("projects/figs/userratingcthist.png")

#Plot distribution of star ratings
edx %>% ggplot(aes(rating)) + geom_bar() + 
 ggtitle("Distribution of Ratings")

ggsave("projects/figs/ratingdistrbar.png")

# Top ratings for movies with greater than 10 user ratings
best_rated_10_plus_ratings <- edx %>% group_by(movieId) %>% 
  summarize(title=title[1], count=n(),avg_rating=sum(rating)/count) %>%
  filter( count > 10 )%>% top_n(25,avg_rating) %>% arrange(desc(avg_rating,count))

head(best_rated_10_plus_ratings,10)

# Assess correlation rating count to average star rating
best_rated_10_plus_ratings_for_cor <- edx %>% group_by(movieId) %>% 
  summarize(title=title[1], count=n(),avg_rating=sum(rating)/count) %>%
  filter( count > 10 )

count_avg_rating_cor<-
  cor(best_rated_10_plus_ratings_for_cor$count,best_rated_10_plus_ratings_for_cor$avg_rating)

count_avg_rating_cor

# User rating spread min vs max
user_rating_range<-edx %>% group_by(userId) %>% 
  summarize(min=min(rating),max=max(rating), rating_spread=max-min)
qplot(user_rating_range$rating_spread, bins = 20,xlab="User Movie Rating Spread")

ggsave("projects/figs/userratingspread.png")

# Top ratings by genre for genres with > 1000 ratings
best_genre_1000_plus_ratings_value <- edx %>% group_by(genres) %>% 
 summarize(count = n(), avg_rating=sum(rating)/count) %>% 
 filter( count > 1000 ) %>% top_n(25,avg_rating) %>% arrange(desc(avg_rating))

head(best_genre_1000_plus_ratings_value)

best_genre_1000_plus_ratings_count <- edx %>% group_by(genres) %>% 
 summarize(count = n(), avg_rating=sum(rating)/count) %>% 
 filter( count > 1000 ) %>% top_n(25,count) %>% arrange(desc(count))

head(best_genre_1000_plus_ratings_count)

# Review distinct genre # of ratings and highest rated
genre_split_rating_count <- edx %>% separate_rows(genres,sep="\\|") %>% 
  group_by(genres)%>% 
  summarize(count=n(), avg_rating=sum(rating)/count) %>% arrange(desc(count))

genre_split_rating_count

genre_split_rating_desc <- edx %>% separate_rows(genres,sep="\\|") %>% 
  group_by(genres)%>%
  summarize(count=n(), avg_rating=sum(rating)/count) %>% arrange(desc(avg_rating))

genre_split_rating_desc

# Evaluate rating trends for different time periods - week, month, quarter
library(lubridate)

edx_time_wk <- mutate(edx, date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>% ggplot(aes(date, rating)) +
  geom_point() + geom_smooth() + ggtitle("Rating by Week")

ggsave("projects/figs/timeweeksmooth.png")

edx_time_month <- mutate(edx, date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "month")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>% ggplot(aes(date, rating)) + 
  geom_point() + geom_smooth(fill="blue")+ ggtitle("Rating by Month")

ggsave("projects/figs/timemonthsmooth.png")

edx_time_qtr <- mutate(edx, date = as_datetime(timestamp)) %>% 
  mutate(date = round_date(date, unit = "quarter")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>% ggplot(aes(date, rating)) + 
  geom_point() + geom_smooth(fill="darkgrey") + ggtitle("Rating by Quarter")

ggsave("projects/figs/timeqtrsmooth.png")

library(gridExtra)
grid.arrange(edx_time_wk, edx_time_month ,edx_time_qtr, nrow=3)

####################################
# Split edx into train and test sets
####################################
# Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_edx <- edx[-test_index,]
temp <- edx[test_index,]

# Ensure userId and movieId in test set are also in train set
test_edx <- temp %>% 
  semi_join(train_edx, by = "movieId") %>% 
  semi_join(train_edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, test_edx)
train_edx <- rbind(train_edx, removed)

rm(test_index, temp, removed)

save(train_edx,file = "projects/data/rda/train_edx.rda")
save(test_edx,file = "projects/data/rda/test_edx.rda" )

# Validate data split executed correctly
dim(train_edx)
dim(test_edx)
identical((dim(train_edx)[[1]]+dim(test_edx)[[1]]),dim(edx)[[1]])

##########################################################
# Create Baseline Average Model 
##########################################################

options(digits=4)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat<-mean(train_edx$rating)
mu_hat
mean_rmse <- RMSE(test_edx$rating, mu_hat)
rmse_result <- data_frame(Method = "Average As Predictor", RMSE = mean_rmse)
rmse_result

##########################################################
# Create Movie User Model Non-Regularized Model
##########################################################

movie_avgs <- train_edx %>% group_by(movieId) %>% 
 summarize(b_i=mean(rating-mu_hat))

user_avgs <- train_edx %>% left_join(movie_avgs, by = "movieId") %>% 
 group_by(userId) %>% summarize(b_u=mean(rating-mu_hat-b_i))

predicted_ratings <- test_edx %>% left_join(movie_avgs, by="movieId") %>%
 left_join(user_avgs,by="userId") %>% mutate(pred = mu_hat + b_i + b_u) %>% pull(pred)

model_1<-RMSE(test_edx$rating, predicted_ratings)

rmse_result <- bind_rows(rmse_result,
                          data_frame(Method="Movie and User Model",
                                     RMSE = model_1))

rmse_result %>% knitr::kable()

##########################################################
# Add Genre Model Effect to Movie and User Model
##########################################################

genre_avgs <- train_edx %>% left_join(movie_avgs, by = "movieId") %>% 
 left_join(user_avgs, by = "userId") %>% group_by(genres) %>% 
 summarize(b_g = mean(rating - mu_hat -b_i -b_u))

#Plot the distribution of variance to average – movie, user, genre

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
 xlab="Residual Movie Bias")

ggsave("projects/figs/residmoviebiashist.png")

user_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"),
 xlab="Residual User Bias")

ggsave("projects/figs/residuserbiashist.png")

genre_avgs %>% qplot(b_g, geom ="histogram", bins = 10, data = ., color = I("black"),
 xlab="Residual Genre Bias")

ggsave("projects/figs/residgenrebiashist.png")

predicted_ratings <- test_edx %>% left_join(movie_avgs, by="movieId") %>% 
 left_join(user_avgs,by="userId") %>% left_join(genre_avgs, by="genres") %>% 
 mutate(pred = mu_hat + b_i + b_u + b_g) %>% pull(pred)

model_2<-RMSE(test_edx$rating,predicted_ratings)

rmse_result <- bind_rows(rmse_result,
                         data_frame(Method="Movie and User and Genre Model",  
                                    RMSE = model_2))
rmse_result %>% knitr::kable()

##################################################
# Regularize Movie User Genre Model Tuning Lambda
##################################################

#Generate single optimal lambda for movie user genre
lambdas <- seq(0, 10, 0.05)
rmses <- sapply(lambdas, function(l){
  mu_hat <- mean(train_edx$rating)
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  b_u <- train_edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  b_g <- train_edx %>%
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  predicted_ratings <- 
    test_edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(test_edx$rating,predicted_ratings))
})
tuned_lambda <- lambdas[which.min(rmses)]
tuned_lambda 

rmse_result <- bind_rows(rmse_result,
                          data_frame(Method="Regularized Movie User Genre Bias Model",  
                                     RMSE = min(rmses)))
model_3<-min(rmses)

rmse_result %>% knitr::kable()

#############################################################
# Plots to evaluate impact of regularization on train_edx set
#############################################################

#Apply tuned lambda to training set
movie_reg_avgs_trained <- train_edx %>% group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+tuned_lambda),n_i=n())

user_reg_avgs_trained <- train_edx %>% 
  left_join(movie_reg_avgs_trained, by="movieId") %>% group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+tuned_lambda),n_i=n())

genre_reg_avgs_trained <- train_edx %>%
  left_join(movie_reg_avgs_trained, by="movieId") %>% 
  left_join(user_reg_avgs_trained, by="userId") %>% group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+tuned_lambda),n_i=n())

#Plots for .rmd document on extent of regularization
data_frame(original_movie_avgs = movie_avgs$b_i, 
           regularized = movie_reg_avgs_trained$b_i, 
           n = movie_reg_avgs_trained$n_i) %>%
  ggplot(aes(original_movie_avgs, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)+ ggtitle("Movie Bias Regularized")

ggsave("projects/figs/movieregularizedvsoriginal.png")

data_frame(original_user_avgs = user_avgs$b_u, 
           regularized = user_reg_avgs_trained$b_u, 
           n = user_reg_avgs_trained$n_i) %>%
  ggplot(aes(original_user_avgs, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.6)+ ggtitle("User Bias Regularized")

ggsave("projects/figs/userregularizedvsoriginal.png")

data_frame(original_genre_avgs = genre_avgs$b_g, 
           regularized = genre_reg_avgs_trained$b_g, 
           n = genre_reg_avgs_trained$n_i) %>%
  ggplot(aes(original_genre_avgs, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.7)+ ggtitle("Genre Bias Regularized")

ggsave("projects/figs/genreregularizedvsoriginal.png")

###########################################################
# Final Model - Movie, User, Genre, Reg'd & Tuned- Full Edx
###########################################################

mu <- mean(edx$rating)

movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+tuned_lambda))

user_reg_avgs <- edx %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+tuned_lambda))

genre_reg_avgs <- edx %>%
  left_join(movie_reg_avgs, by="movieId") %>% 
  left_join(user_reg_avgs, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i- b_u - mu)/(n()+tuned_lambda))

predicted_ratings <- 
  validation %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  left_join(genre_reg_avgs, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% .$pred
  
model_final <- RMSE(validation$rating,predicted_ratings)
rmse_final_validation <-
  data_frame(Method="Regularized Movie User Genre Model (Full Edx & Validation)",  RMSE = model_final)

rmse_final_validation
