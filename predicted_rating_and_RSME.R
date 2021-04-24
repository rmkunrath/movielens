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
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


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

########################################################################################################
# Exploring and Wrangling edx.
########################################################################################################

# With a brief look at edx we can see that the column title also contains the year and genres are
# somehow aggregated
head(edx)

# Also, we can see that the 9000055 entries are comprized of 69878 users and 10677 
# movies, with a mean rating of 3.5 and a standard deviation of 1

edx %>% summarize(n_row = nrow(.),
                  n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId))

edx %>% summarise(mean = mean(rating), sd = sd(rating))

# Users do no tend to give half star ratings.
edx %>% ggplot(aes(rating)) + geom_histogram()

# The movies have a rating distribution that is far from linear. Most part of the ratings are comprized in 
# few movies.
# Creating an object that contains total ratings and the mean rating per movie
dat_permovie <- edx %>% 
  group_by(title) %>% 
  summarise(totalratings = sum(userId != 0), mean_rate = mean(rating)) %>% 
  arrange(desc(totalratings))

# Plotting the total amount of ratings per rank in logarithmic scale
dat_permovie %>% 
  ggplot(aes(10677-rank(totalratings), totalratings)) + # 10677 is the total movie count
  geom_line() +
  scale_x_log10() + 
  xlab("Top n movies") +
  ylab("Total amount of ratings")

# The most viewed movies are, unsurprisingly, the ones with better mean ratings
# Plotting the ratings per rank in logarithmic scale
dat_permovie %>% 
  ggplot(aes(10677-rank(totalratings), mean_rate)) + # 10677 is the total movie count
  geom_point(alpha = 0.2) +
  scale_x_log10() + 
  geom_smooth() +
  xlab("Top n movies") +
  ylab("Rating")

# Movies mean and sd ratings
dat_permovie %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Top 100 movies mean and sd ratings
dat_permovie %>% top_n(100, wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Top 1000 movies mean and sd ratings
dat_permovie %>% top_n(1000, wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))
# Rest of users mean and sd ratings
dat_permovie %>% top_n(-(10676-1000), wt = totalratings) %>% summarize(mean = mean(mean_rate), sd = sd(mean_rate))


# The users also have a rating distribution that is far from linear. Most part of the ratings are comprized in 
# few users
# Creating an object that contains total ratings and the mean rating per user
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
# Plotting the ratings per rank in logarithmic scale
dat_peruser %>% 
  ggplot(aes(69878-rank(totalratings), mean_rate)) + # 69878 is the total movie count
  geom_point(alpha = 0.2) +
  scale_x_log10() + 
  geom_smooth() +
  xlab("Top n users") +
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

# Plotting aggregated genres and their mean. It looks like a cotangent function.
dat_pergenres %>%
  ggplot(aes(797-rank(mean), mean)) + # 797 is the total genre count
  geom_line() +  
  xlab("Top n genres") +
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

# Plot object of mean rating per year
a <- ggplot(data = dat_peryear, aes(x = year, y = mean_rate)) +
  geom_point(alpha = 1, color = "blue") +
  geom_smooth() +
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

rm(dat_pergenres, dat_permovie, dat_peruser, dat_peryear, year_vector, a, b)
  
#################################### Extracting useful values ######################################
# It has been seen that there is information that seems to impact on the mean rating of a movie.
# The information is:
# Order of most viewed movies (1)
# Order of users that most rate movies (2)
# Order of genre rating (3)
# Year of the movie (4)

# To make value of this information, it is going to be extracted from the data and
# added to the table. 

# A function is going to be created to do so.
add_useful_columns <- function(table){

# (1)
# Getting the total movie ratings column
dat_permovie <- table %>% 
  group_by(movieId) %>% 
  summarise(total_movie_ratings = sum(userId != 0)) %>% 
  arrange(desc(total_movie_ratings))

# Adding the column
table <- left_join(table, dat_permovie, by = "movieId")

# (2)
# Getting the total user ratings column
dat_peruser <- table %>% 
  group_by(userId) %>% 
  summarise(total_user_ratings = sum(userId != 0)) %>% 
  arrange(desc(total_user_ratings))

# Adding the column
table <- left_join(table, dat_peruser, by = "userId")

# (3)
# Getting gender rank
dat_pergenres <- table %>% 
  group_by(genres) %>% 
  summarise(genre_rank = mean(rating)) %>% 
  mutate(genre_rank = rank(genre_rank))

# Adding the column
table <- left_join(table, dat_pergenres, by = "genres")

# (4)
# Getting the year
# Getting the year within the parenthesis
year_vector <- str_extract(table$title, "\\(\\d\\d\\d\\d\\)")
# Removing the parenthesis
year_vector <- substring(year_vector, 2, nchar(year_vector)-1)
# Converting to numeric
year_vector <- as.numeric(year_vector)

# Adding the year column
table <- table %>% mutate(year = year_vector)

table
}
# End of function



########################################################################################################
# Partitioning the data in test and train set
########################################################################################################


# Now we are ready to split the data into test and train set

# Leaving 20% of the data to test and creating test and train set
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx %>% slice(-test_index)
test_set <- edx %>% slice(test_index)

rm(test_index)

#Use 6_2 and 6_3 handout
