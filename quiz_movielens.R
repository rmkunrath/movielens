# Quiz: MovieLens Dataset

# It uses the data created in creating_data.R script
# In case you haven't generated it, you can load it as follows

# load("edx_movielist.RData")

# How many rows and columns are there in the edx dataset?
nrow(edx)
ncol(edx)


# How many zeros were given as ratings in the edx dataset?
library(tidyverse)

head(edx)
class(edx$rating)
edx %>% filter(rating == 0) %>% nrow()

# How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% nrow()

# How many different movies are in the edx dataset?
edx %>% group_by(movieId) %>% tally() %>% nrow()

# How many different users are in the edx dataset?
edx %>% filter(grepl("Drama", genres)) %>% nrow()
edx %>% filter(grepl("Comedy", genres)) %>% nrow()
edx %>% filter(grepl("Thriller", genres)) %>% nrow()
edx %>% filter(grepl("Romance", genres)) %>% nrow()

# Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% summarise(totalratings = sum(userId != 0)) %>% arrange(desc(totalratings)) %>% .[1,]

# What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(totalevaluations = sum(userId != 0)) %>% arrange(desc(totalevaluations))

                                       