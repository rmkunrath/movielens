##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

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

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# # if using R 4.0 or later:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                            title = as.character(title),
#                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

########################################################################################################
# Saving and loading edx and evaluation objects to prevent from running the code above in multiple days.
# This code is unecessary in case the above section has been run during current session.
########################################################################################################

# Saving edx object as a file
save(edx, file = "edx_movielist.RData")
# Saving validation object as a file
save(validation, file = "validation_movielist.RData")

###########################################----------------#############################################

# rm(edx)

# loading edx file as object
ifelse(exists("edx"), "edx already exists", load("edx_movielist.RData"))
# loading validation file as object
ifelse(exists("validation"), "validation already exists", load("validation_movielist.RData"))

# Ensure the same seed as the above section.
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

########################################################################################################
# Necessary packages
########################################################################################################

library(tidyverse)
library(caret)
library(stringr)
library(lubridate)
library(broom)

# Parallel processing
# library(doMC)
# registerDoMC(cores=1)

########################################################################################################
# Exploring and Wrangling edx.
########################################################################################################

############################################## Exploring ###############################################

# With a brief look at edx we can see that the column title also contains the year and genres are somehow aggregated
  
head(edx)
nrow(edx)

# Also, we can see that the 9000055 entries are comprized of 69878 users and 10677 
# movies, with a mean rating of 3.5 and a standard deviation of 1

edx %>% summarize(n_row = nrow(.),
                  n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId))

edx %>% summarise(mean = mean(rating), sd = sd(rating))

# Users do no tend to give half star ratings.
edx %>% ggplot(aes(rating)) + geom_histogram()

# The movies have a rating distribution that is far from linear. 
# Most part of the ratings are comprized in a few movies.
# Creating an object that contains total ratings and the mean rating per movie
dat_permovie <- edx %>% 
  group_by(title) %>% 
  summarise(totalratings = sum(userId != 0), mean_rate = mean(rating)) %>% 
  arrange(desc(totalratings))

# Plotting the total amount of ratings per rank in logarithmic scale
dat_permovie %>% 
  ggplot(aes(10677-rank(totalratings), totalratings)) +
  geom_line() +
  scale_x_log10() + 
  xlab("Top n movies") +
  ylab("Total amount of ratings")

# The most viewed movies are, unsurprisingly, the ones with better mean ratings
# Plotting the ratings per rank in logarithmic scale.
dat_permovie %>% 
  ggplot(aes(totalratings, mean_rate)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  geom_smooth() +
  xlab("Total ratings") +
  ylab("Rating")

# Movies mean and sd ratings
dat_permovie %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Top 100 movies mean and sd ratings
dat_permovie %>% top_n(100, wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Top 1000 movies mean and sd ratings
dat_permovie %>% top_n(1000, wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Rest of users mean and sd ratings
dat_permovie %>% top_n(-(10676-1000), wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))

# The users also have a rating distribution that is far from linear. 
# Most part of the ratings are comprized in few users.
# Creating an object that contains total ratings and the mean rating per user.
dat_peruser <- edx %>% 
  group_by(userId) %>% 
  summarise(totalratings = sum(userId != 0), mean_rate = mean(rating)) %>% 
  arrange(desc(totalratings))

# Plotting the total amount of ratings per rank in logarithmic scale
dat_peruser %>% 
  ggplot(aes(69878-rank(totalratings), totalratings)) + # 69878 is the total user count
  geom_line() +
  scale_x_log10() + 
  xlab("Top n users") +
  ylab("Total amount of ratings")

# Users that assess more movies also tend to be more critical with their reviews.
# Plotting the ratings per rank. Polynom of order 5
dat_peruser %>% 
  ggplot(aes(rank(totalratings), mean_rate)) + # 69878 is the total movie count
  geom_point(alpha = 0.2) +
  geom_smooth() +
  xlab("User rank") +
  ylab("Rating")

# Users mean and sd ratings
dat_peruser %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Top 1000 users mean and sd ratings
dat_peruser %>% top_n(1000, wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Rest of users mean and sd ratings
dat_peruser %>% top_n(-(69878-1000), wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))

# Exploring whether the genre can affect mean rating.
# Creating and object with movie ratings per genre.
dat_pergenres <- edx %>% group_by(genres) %>% 
  summarize(mean = mean(rating), sd = sd(rating), total_ratings = sum(userId != 0)) %>% 
  arrange(desc(mean))

head(dat_pergenres)

# Different genres have also different means and standard deviations
edx %>% summarise(mean = mean(rating), sd = sd(rating))
edx %>% filter(str_detect(genres, "Drama")) %>% summarise(mean = mean(rating), sd = sd(rating))
edx %>% filter(str_detect(genres, "Film-Noir")) %>% summarise(mean = mean(rating), sd = sd(rating))
edx %>% filter(str_detect(genres, "Mystery")) %>% summarise(mean = mean(rating), sd = sd(rating))
edx %>% filter(str_detect(genres, "Horror")) %>% summarise(mean = mean(rating), sd = sd(rating))
edx %>% filter(str_detect(genres, "Comedy")) %>% summarise(mean = mean(rating), sd = sd(rating))
edx %>% filter(str_detect(genres, "Children")) %>% summarise(mean = mean(rating), sd = sd(rating))

# Plotting aggregated genres and their mean. It looks like a cotangent function. Polynom of order 5
dat_pergenres %>%
  ggplot(aes(rank(mean), mean)) + # 797 is the total genre count
  geom_line() +  
  geom_smooth() +
  xlab("Genre rank") +
  ylab("Mean rating")

# Finnally, verifying if the year influences the mean rating.
# To do that, it is necessary to extract the year from the table.

# Getting the year within the parenthesis
year_vector <- str_extract(edx$title, "\\(\\d\\d\\d\\d\\)")
# Removing the parenthesis
year_vector <- substring(year_vector, 2, nchar(year_vector)-1)
# Converting to numeric
year_vector <- as.numeric(year_vector)

# Creating an object that contains the year
dat_peryear <- edx %>% mutate(year = year_vector)
dat_peryear <- dat_peryear %>% group_by(year) %>% summarise(totalratings = sum(userId != 0), mean_rate = mean(rating))

head(dat_peryear)

# Plot object of mean rating per year. Polynom of order 4
a <- ggplot(data = dat_peryear, aes(x = year, y = mean_rate)) +
  geom_point(alpha = 1, color = "blue") +
  geom_smooth(method='lm', formula = y~poly(x,1)) +
  xlab("Year") +
  ylab("Mean rating")
# Plot object of total ratings per year
b <- ggplot(data = dat_peryear, aes(x = year, y = totalratings)) +
  geom_point(alpha = 1, color = "red") +
  xlab("Year") +
  scale_y_log10() +
  ylab("Number of ratings")

# Arranged plot of mean ratings and year
require(gridExtra)
grid.arrange(a, b)

# Verifying if timestamp affects rating

# The rating year does not affect substancially the mean. The same is valid for month and weekday
edx %>% 
  mutate(rating_year = year(as_datetime(timestamp))) %>%
  group_by(rating_year) %>%
  summarise(mean = mean(rating))

edx %>% 
  mutate(rating_month = month(as_datetime(timestamp))) %>%
  group_by(rating_month) %>%
  summarise(mean = mean(rating))

edx %>% 
  mutate(rating_weekday = wday(as_datetime(timestamp))) %>%
  group_by(rating_weekday) %>%
  summarise(mean = mean(rating))

### Verifying user gender preference
# It is somewhat logic that users may have specific taste per genre
# User 11129 has more than 375 ratings and a mean rating of 4.1.
# Nevertheless, the user doesn't like Horror movies. This should be taken into account.

edx %>% filter(userId == 11129) %>% nrow()
edx %>% filter(userId == 11129) %>% summarize(mean(rating))
edx %>% filter(userId == 11129 & genres == "Horror")

# Cleaning the envinronment
rm(dat_pergenres, dat_permovie, dat_peruser, dat_peryear, 
   year_vector, a, b, table, lambda, lambdas, RMSE, rmses, mu, dat_guessing_mean)

############################################## Wrangling ###############################################

####################################### Extracting useful values #######################################
# It has been seen that there is information that seems to impact on the mean rating of a movie.
# The information is:
# Movie mean rating (1)
# User mean rating (2)
# Genre mean (3)
# User mean rating per genre (4)
# Year of the movie (5)

# To make value of this information, it is going to be extracted from the data and
# added to the table. 

# A function is going to be created to do so.
add_useful_columns <- function(table){

# table <- edx
# (1)
# Movie mean rating

mu <- mean(table$rating)
  
dat_permovie <- table %>% 
  group_by(movieId) %>% 
  summarise(movie_mean_rating = mean(rating))

# Adding the column
table <- left_join(table, dat_permovie, by = "movieId")

# (2)
# user mean rating
dat_peruser <- table %>% 
  group_by(userId) %>% 
  summarise(user_mean_rating = mean(rating))

# Adding the column
table <- left_join(table, dat_peruser, by = "userId")

# (3)
# Getting genre mean
dat_pergenres <- table %>% 
  group_by(genres) %>% 
  summarise(genre_mean_rating = mean(rating))

# Adding the column
table <- left_join(table, dat_pergenres, by = "genres")

# (4)
# Getting user mean per genre
# To not overtrain the mean, only considering where there is more than 4 ratings per genre. 
# If not, considering the average between user mean rating and movie mean rating
table <- table %>% 
  group_by(genres, userId) %>% 
  mutate(user_genre_mean = ifelse(n() >= 4, mean(rating), (user_mean_rating+movie_mean_rating)/2)) %>%
  ungroup()


# (5)
# Getting the year
# Getting the year within the parenthesis
year_vector <- str_extract(table$title, "\\(\\d\\d\\d\\d\\)")
# Removing the parenthesis
year_vector <- substring(year_vector, 2, nchar(year_vector)-1)
# Converting to numeric
year_vector <- as.numeric(year_vector)

# Adding the year column
table <- table %>% mutate(year = year_vector)

return(table)
}
# End of function

edx <- add_useful_columns(edx)

########################################################################################################
# Partitioning the data in test and train set. Creating RSME function
########################################################################################################


# Now we are ready to split the data into test and train set

# Leaving 10% of the data to test and creating test and train set
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.1, list = FALSE)
train_set <- edx %>% slice(-test_index)
test_set <- edx %>% slice(test_index)

rm(test_index)

# Creating a function to return the root-mean-square deviation
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

########################################################################################################
# Creating a model
########################################################################################################

# Determining a mean rate and predict it
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results

# Predicting using user mean rating
user_mean_rmse <- RMSE(test_set$rating, test_set$user_mean_rating)
user_mean_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method="User mean rating", RMSE = user_mean_rmse ))
rmse_results

# Predicting using movie mean rating
movie_mean_rmse <- RMSE(test_set$rating, test_set$movie_mean_rating)
movie_mean_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method="Movie mean rating", RMSE = movie_mean_rmse ))
rmse_results

rm(movie_mean_rmse, naive_rmse, user_mean_rmse)

# Applying movie mean rating and user mean rating
# beta is the fraction for the movie
beta <- seq(0.1, 1, 0.05)

# Applying beta
rmses <- sapply(beta, function(l){
  val <- RMSE(test_set$rating, (train_set$movie_mean_rating*l + (1-l)*train_set$user_mean_rating))
  return(val)
})

# Best beta
qplot(beta, rmses)  
beta <- beta[which.min(rmses)]

movie_user_mean_rmse <- RMSE(test_set$rating, (test_set$movie_mean_rating*beta + (1-beta)*test_set$user_mean_rating))
movie_user_mean_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method="Movie & user mean rating", RMSE = movie_user_mean_rmse ))
rmse_results


# Applying a linear regression
# head(edx)

lm_fit <- train_set %>%
  lm(rating ~ movie_mean_rating + user_mean_rating + genre_mean_rating + user_genre_mean + year, data=.)

mu_hat_linear <- predict(lm_fit, newdata = test_set, type = "response")

lm_rmse <- RMSE(test_set$rating, mu_hat_linear)
lm_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method="Linear regression", RMSE = lm_rmse ))
rmse_results

# Assessing the error

# Graph that plots true rating and estimate
a <- test_set %>%
  mutate(estimate = mu_hat_linear) %>%
  mutate(diff = rating - estimate)

box <- a %>% select(estimate, rating) %>%
  mutate(rating = as_factor(rating)) %>%
  ggplot(aes(rating, estimate)) +
  geom_boxplot()

smooth <- a %>% select(estimate, rating) %>%
  ggplot(aes(rating, estimate)) +
  geom_smooth()

require(gridExtra)
grid.arrange(box, smooth)
rm(a, box, smooth)

# Analysing the linear regression
tidy(lm_fit)

# Actually seeing as the coefficients relate to prediction

lm_fit$coefficients[1]
lm_fit$coefficients[2] * mean(test_set$movie_mean_rating )
lm_fit$coefficients[3] * mean(test_set$user_mean_rating)
lm_fit$coefficients[4] * mean(test_set$genre_mean_rating)
lm_fit$coefficients[5] * mean(test_set$user_genre_mean)
lm_fit$coefficients[6] * mean(test_set$year)

#####

# Although good, the estimate can get better.
# The minimum rate is 0.5 and the maximum is 5.

# Set limits
minV <- 0.5
maxV <- 5

# Limit vector
mu_hat_linear <- sapply(mu_hat_linear, function(y) min(max(y,minV),maxV))

lm_limited_rmse <- RMSE(test_set$rating, mu_hat_linear)
lm_limited_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method="Linear regression limited", RMSE = lm_limited_rmse ))
rmse_results

# ###############################################################
# # Getting principal components for user specific taste of genre.
# 
# # The whole session was commented. Just remove the coments with Ctrl+Shift+C to give it a try.
# # WARNING - This process takes a long time.
# 
# ######################
# # Considering pareto (80/20) law, 80% of variance is from 20% of the data
# # Let us explore where 20% of variance is.
# 
# test_set_with_estimate <- test_set %>%
#   mutate(estimate = mu_hat_linear) %>%
#   mutate(diff_sq = as.integer((rating - estimate)^2))
# 
# head(test_set_with_estimate)
# 
# # The top 5000 users present larger variance
# test_set_with_estimate %>%
#   group_by(total_user_ratings_rank) %>%
#   summarise(mean = mean(diff_sq)) %>%
#   ggplot(aes(69878-total_user_ratings_rank, mean)) +
#   geom_point(alpha = 0.2) +
#   xlab("Top users")
# 
# # Error per movie ratings doesn't seem to have a great variance
# test_set_with_estimate %>%
#   group_by(movie_mean_rating) %>%
#   summarise(mean = mean(diff_sq)) %>%
#   ggplot(aes(movie_mean_rating, mean)) +
#   geom_point(alpha = 0.2) +
#   xlab("Movie ratings")
# 
# # Error is larger for users with mean rating lower than 3.5 stars
# # This is also correlated with users with more ratings
# test_set_with_estimate %>%
#   group_by(user_mean_rating) %>%
#   summarise(mean = mean(diff_sq)) %>%
#   ggplot(aes(user_mean_rating, mean)) +
#   geom_point(alpha = 0.2) +
#   xlab("User mean rating")
# 
# # Gender rank does't seem to affect much the variance
# test_set_with_estimate %>%
#   group_by(genre_rank) %>%
#   summarise(mean = mean(diff_sq)) %>%
#   ggplot(aes(genre_rank, mean)) +
#   geom_point(alpha = 0.2) +
#   xlab("Genre rank")
# 
# rm(test_set_with_estimate)
# 
# #####################
# # filtering top 5000 users rank and and lower than 3.5 stars mean
# 
# rank_20 <- 0.9*max(edx$total_user_ratings_rank)
# 
# data20 <- edx %>%
#   filter(user_mean_rating < 3.0 & total_user_ratings_rank > rank_20)
# 
# 
# data20 <- data20 %>%
#   group_by(genre_rank, userId) %>%
#   summarise(mean_genre = mean(rating))
# 
# y <- data20 %>%
#   select(userId, genre_rank, mean_genre) %>%
#   spread(genre_rank, mean_genre) %>%
#   as.matrix()
# 
# rownames(y)<- y[,1]
# y <- y[,-1]
# 
# genre_titles <- edx %>%
#   select(genre_rank, genres) %>%
#   distinct()
# 
# colnames(y) <- with(genre_titles, genres[match(colnames(y), genre_rank)])
# 
# ### SVD and PCA
# 
# y[is.na(y)] <- mu_hat
# 
# y <- sweep(y, 1, rowMeans(y))
# pca <- prcomp(y)
# 
# dim(pca$rotation)
# 
# dim(pca$x)
# 
# plot(pca$sdev)
# 
# # Seeing that the first PCs do not cover much of the data, being PC1 and PC2 total acumulation
# # of only 6%
# s <- summary(pca)    # see PC1 Cumulative Proportion
# str(s)
# s$importance[3,]
# 
# # PC1 seems to relate to genres with a more complicated plot and movies easy to watch
# 
# library(ggrepel)
# pcs <- data.frame(pca$rotation, name = colnames(y))
# pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() +
#   geom_text_repel(aes(PC1, PC2, label=name),
#                   data = filter(pcs,
#                                 PC1 < -0.05 | PC1 > 0.075))
# 
# # PC2 seems to relate to horror and children movies
# pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() +
#   geom_text_repel(aes(PC1, PC2, label=name),
#                   data = filter(pcs,
#                                 PC2 < -0.075 | PC2 > 0.05))
# 
# user_preference <- data.frame(pca$x[,1:13])
# user_preferenceID <- user_preference %>% mutate(userId = as.integer(rownames(user_preference)))
# 
# # save(user_preferenceID, file = "user_preferenceID_PCA.RData")
# # Load table with principal components
# ifelse(exists("user_preferenceID"), "user_preferenceID already exists", load("user_preferenceID_PCA.RData"))
# 
# # Adding PCs to the table and applying 0 to NAs
# edx <- left_join(edx, user_preferenceID, by = "userId")
# edx[is.na(edx)] <- 0
# 
# # Re-aplying the Linear regression
# 
# # Leaving 20% of the data to test and creating test and train set
# test_index <- createDataPartition(y = edx$rating, times = 1,
#                                   p = 0.2, list = FALSE)
# train_set <- edx %>% slice(-test_index)
# test_set <- edx %>% slice(test_index)
# 
# rm(test_index)
# 
# # Only using first 4 primary components
# lm_fit_PCA <- train_set %>%
#   lm(rating ~ movie_mean_rating + user_mean_rating + genre_mean_rating + user_genre_mean + year
#      + PC1 + PC2 + PC3 + PC4, data=.)
# 
# mu_hat_linear_PCA <- predict(lm_fit_PCA, newdata = test_set, type = "response")
# 
# lm_PCA_rmse <- RMSE(test_set$rating, mu_hat_linear_PCA)
# lm_PCA_rmse
# 
# # There is no great change, so it does not worth the computational effort
# rmse_results <- bind_rows(rmse_results, data_frame(method="LM+PCA", RMSE = lm_PCA_rmse ))
# rmse_results
# 
# #########################################################################
# # Verifying if there are PCs that relate to usedId and movieId
# # Using pareto (80/20) law, filtering first 20% of userId and movieId to get 80% of relevance.
# # For the size of the dataset, just to get an insight.
# ######################
# 
# rank_20 <- 0.9*max(edx$total_user_ratings_rank)
# 
# data20 <- edx %>%
#   filter(user_mean_rating < 3.0 & total_user_ratings_rank > rank_20)
# 
# # rm(user_20, movie_20, top_20)
# 
# y <- data20 %>%
#   select(userId, movieId, rating) %>%
#   spread(movieId, rating) %>%
#   as.matrix()
# 
# rownames(y)<- y[,1]
# y <- y[,-1]
# 
# movie_titles <- edx %>%
#   select(movieId, title) %>%
#   distinct()
# 
# colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
# 
# ### SVD and PCA
# # This process takes a long time.
# 
# y[is.na(y)] <- mu_hat
# 
# y <- sweep(y, 1, rowMeans(y))
# pca <- prcomp(y)
# 
# dim(pca$rotation)
# 
# dim(pca$x)
# 
# plot(pca$sdev)
# 
# # Seeing that the first PCs do not cover much of the data, being PC1 and PC2 total acumulation
# # of only 5%
# s <- summary(pca)    # see PC1 Cumulative Proportion
# str(s)
# s$importance[3,]
# 
# # Like with gender PCs, it is not worth the computational expenditure
# 
# rm(pca, lm_fit_PCA, y, s, mu_hat_linear_PCA, rank_20, movie_mean_rmse, movie_user_mean_rmse, lm_PCA_rmse,
#    data20, movie_titles, genre_titles, user_preference, user_preferenceID, pcs)

##########################################
# We noticed that the PCs did not worth the trouble.
# Can we still get a better prediction? During data exploration we saw that many values, like the year,
# were not linear. Let us try with the very smooth used in ggplot mgcv::gam()

library(mgcv)

gam_fit <- train_set %>%
  gam(rating ~ movie_mean_rating + user_mean_rating + genre_mean_rating + user_genre_mean + year, data=.)

mu_hat_gam <- predict(gam_fit, newdata = test_set)

gam_rmse <- RMSE(test_set$rating, mu_hat_gam)
gam_rmse

# There is no great change, so it does not worth the computational effort
rmse_results <- bind_rows(rmse_results, data_frame(method="GAM", RMSE = gam_rmse ))
rmse_results

rm(gam_fit, mu_hat_gam, gam_rmse)

########################################################################################################
# Predicting the model
########################################################################################################

#################### Chosen ####################
# The best method was linear regression

# Validating the model
validation <- add_useful_columns(validation)

# Predict
mu_hat_linear <- predict(lm_fit, newdata = validation)
# Set limits
minV <- 0.5
maxV <- 5
mu_hat_linear <- sapply(mu_hat_linear, function(y) min(max(y,minV),maxV))

lm_rmse <- RMSE(validation$rating, mu_hat_linear)
lm_rmse

# Discussion
# As there are less genres and the prediction cut was 4 movies, it is 
# arguable the the regression could be using to much from the rating.
# For this reason, a new regression without considering the genre at all is presented

lm_fit_without_genre <- train_set %>%
  lm(rating ~ movie_mean_rating + user_mean_rating + genre_mean_rating + year, data=.)

mu_hat_linear_without_genre <- predict(lm_fit_without_genre, newdata = validation, type = "response")

# The prediction is still very good.
lm_rmse <- RMSE(validation$rating, mu_hat_linear_without_genre)
lm_rmse
