library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

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
#-------------------------------------------------------------------------------------------
rm(list = ls())

.rs.restartR()

#write.csv(edx, "C:/Users/Mark/Documents/Harvard Captstone/edx.csv", row.names = FALSE)
#write.csv(validation, "C:/Users/Mark/Documents/Harvard Captstone/validation.csv", row.names = FALSE)

edx <- read.csv("C:/Users/Mark/Documents/Harvard Captstone/edx.csv")
validation <- read.csv("C:/Users/Mark/Documents/Harvard Captstone/validation.csv")

head(edx)

# Number a unique genre entries in the genres column 
uni_gens <- unique(edx$genres)
length(uni_gens)

# Number of unique individual genres
gen_split <- str_split(edx$genres, "\\|")
gen_char2 <- unlist(gen_split)
uni <- unique(gen_char2)
uni

uni_gens <- unique(edx$genres)
uni_gens

edx %>% filter(genres== '(no genres listed)')
#----------------------------------------------------------------------------------------
edx %>% group_by(userId)%>%summarize(n = n()) %>% ggplot(aes(x=n, y=..density..)) + geom_histogram(color = 'gray', fill = 'steelblue', bins = 20) +
  scale_x_log10()+geom_density(col='red')+ggtitle('Distribtion of how many reviews each user made')+xlab("Number of reviews of each user")

edx %>% group_by(movieId)%>%summarize(n = n()) %>% ggplot(aes(x=n, y=..density..)) + geom_histogram(color = 'gray', fill = 'steelblue', bins = 20) +
  scale_x_log10()+geom_density(col='red')+ggtitle('Distribtion of how many reviews each movie received')+xlab("Number of reviews for each movie")

edx1 <-  edx %>% mutate( year_rated = year(as_datetime(timestamp)))  %>% select(-timestamp) %>% 
  extract(col=title, into = c('title', 'year_released'), regex = "(.+)\\((\\d{4})") %>% 
  mutate(year_released = round(as.numeric(year_released),2)) %>%
  mutate(released_to_rated = year_rated - year_released) 

edx2 <- edx1 %>% group_by(title, movieId, genres, year_released) %>% summarize(num_of_ratings=n(),  avg_rate = round(mean(rating),2), 
              avg_time_to_rate = round(mean(released_to_rated),2))


head(edx2)



edx3 <- as.data.frame(edx2) %>% mutate(num_of_genres = str_count(edx2$genres, pattern = "\\|") +1)

edx_scale <- scale(edx3[c(4:5, 7:8)])
edx4 <- cbind(edx3[, c(1:3)], edx_scale, edx3['avg_rate'])        

head(edx4)

edx %>% ggplot(aes(x=rating, y= ..density..)) + geom_histogram(color = 'gray', fill = 'steelblue', bins=10) +
  ggtitle('Distribution of the all ratings of all movies')


edx4 %>% ggplot(aes(x=avg_rate, y= ..density..)) + geom_histogram(color = 'gray', fill = 'steelblue', bins =10) + 
  ggtitle('Distribution of the average rating of each movie')



model <- lm(avg_rate~year_released + num_of_ratings + num_of_genres + avg_time_to_rate, edx4)
summary(model)
y_hat <- predict(model, edx4)

rmse <- sqrt(mean((y_hat - edx4$avg_rate)^2, na.rm = TRUE))
rmse

edx4 <- edx3  %>%
                mutate(Drama = as.numeric(str_detect(genres, "Drama")), 
                       Animation = as.numeric(str_detect(genres, "Animation")),
                        Children = as.numeric(str_detect(genres, "Children")),
                        Musical = as.numeric(str_detect(genres, "Musical")),
                        Thriller = as.numeric(str_detect(genres, "Thriller")),
                        Action = as.numeric(str_detect(genres, "Action")),
                        Adventure = as.numeric(str_detect(genres, "Adventure")),
                        Sci_Fi = as.numeric(str_detect(genres, "Sci-Fi")),
                        War = as.numeric(str_detect(genres, "War")),
                        Fantasy = as.numeric(str_detect(genres, "Fantasy")),
                        Horror = as.numeric(str_detect(genres, "Horror")),
                        Romance = as.numeric(str_detect(genres, "Romance")),
                        Comedy = as.numeric(str_detect(genres, "Comedy")),
                        Crime = as.numeric(str_detect(genres, "Crime")),
                        Western = as.numeric(str_detect(genres, "Western")),
                        Mystery = as.numeric(str_detect(genres, "Mystery")),
                        IMAX = as.numeric(str_detect(genres, "IMAX")),
                        Documentary = as.numeric(str_detect(genres, "Documentary")),
                        Film_Noir = as.numeric(str_detect(genres, "Film-Noir")))
                        


head(edx4)
edx5 <- cbind(edx_scale, edx4[, 9:27], edx4['avg_rate'])
head(edx5)
model2 <- lm(avg_rate ~ ., edx5)
summary(model2)

y_hat2 <- predict(model2, edx5)

rmse2 <- sqrt(mean((y_hat2 - edx5$avg_rate)^2, na.rm = TRUE))
rmse2



#-----------------------------------------------------------------------------------------------------------------------------
val1 <-  validation %>% mutate( year_rated = year(as_datetime(timestamp))) %>% 
  extract(col=title, into = c('title', 'year_released'), regex = "(.+)\\((\\d{4})") %>% 
  mutate(year_released = round(as.numeric(year_released),2)) %>%
  mutate(released_to_rated = year_rated - year_released) %>% select(-timestamp)

val2 <- val1 %>% group_by(title, movieId, genres) %>% summarize(num_of_ratings=n(),  avg_rate = round(mean(rating),2), 
                avg_time_to_rate = round(mean(released_to_rated),2), year_released = mean(year_released) )


head(val2)




val3 <- as.data.frame(val2) %>% mutate(num_of_genres = str_count(val2$genres, pattern = "\\|") +1)

val_scale <- scale(val3[c(4, 6:8)])
val4 <- cbind(val3[, c(1:3)], val_scale, val3['avg_rate'])        

head(val4)


y_hat <- predict(model, val4)

val_rmse <- sqrt(mean((y_hat - val4$avg_rate)^2, na.rm = TRUE))
val_rmse

val4 <- val3 %>% mutate(Drama = as.numeric(str_detect(genres, "Drama")), Animation = as.numeric(str_detect(genres, "Animation")),
                        Children = as.numeric(str_detect(genres, "Children")),
                        Musical = as.numeric(str_detect(genres, "Musical")),
                        Thriller = as.numeric(str_detect(genres, "Thriller")),
                        Action = as.numeric(str_detect(genres, "Action")),
                        Adventure = as.numeric(str_detect(genres, "Adventure")),
                        Sci_Fi = as.numeric(str_detect(genres, "Sci-Fi")),
                        War = as.numeric(str_detect(genres, "War")),
                        Fantasy = as.numeric(str_detect(genres, "Fantasy")),
                        Horror = as.numeric(str_detect(genres, "Horror")),
                        Romance = as.numeric(str_detect(genres, "Romance")),
                        Comedy = as.numeric(str_detect(genres, "Comedy")),
                        Crime = as.numeric(str_detect(genres, "Crime")),
                        Western = as.numeric(str_detect(genres, "Western")),
                        Mystery = as.numeric(str_detect(genres, "Mystery")),
                        IMAX = as.numeric(str_detect(genres, "IMAX")),
                        Documentary = as.numeric(str_detect(genres, "Documentary")),
                        Film_Noir = as.numeric(str_detect(genres, "Film-Noir")),
                        No_genre_listed = as.numeric(str_detect(genres, "(no genres listed)" )))
head(val4)
val5 <- cbind(val_scale, val4[, 9:28], val4['avg_rate'])
head(val5)

summary(model2)

y_hat2 <- predict(model2, val5)

val_rmse2 <- sqrt(mean((y_hat2 - val5$avg_rate)^2, na.rm = TRUE))
val_rmse2

#---------------------------------------------------------------------------------------------------
edx1 %>% group_by(year_released) %>% summarize(avg_rate_per_year=mean(rating)) %>% ggplot(aes(year_released, avg_rate_per_year)) +
  geom_point(col='steelblue') + geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange')+ ggtitle('Average rating by year')+
  labs(y='Average Rating', "Year of Release")

avg_rating_by_year_1950_or_later <- edx1 %>% group_by(year_released) %>% summarize(avg_rate_per_year=mean(rating)) %>% 
  filter(year_released>=1950)
head(avg_rating_by_year)

avg_rating_by_year_1950_or_later %>% ggplot(aes(year_released, avg_rate_per_year)) +
  geom_point(col='steelblue') + geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange') + ggtitle('Average rating by year')

post_1950_model <- lm(avg_rate_per_year ~ year_released, avg_rating_by_year_1950_or_later)
summary(post_1950_model)

y_hat_1950 <- predict(post_1950_model, avg_rating_by_year_1950_or_later)

rmse_1950 <- sqrt(mean((y_hat_1950 - avg_rating_by_year_1950_or_later$avg_rate_per_year)^2, na.rm = TRUE))
rmse_1950

val1 %>% group_by(year_released) %>% summarize(avg_rate_per_year=mean(rating)) %>% ggplot(aes(year_released, avg_rate_per_year)) +
  geom_point(col='steelblue') + geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange') + ggtitle('Average rating by year')

val_avg_rating_by_year_1950_or_later <- val1 %>% group_by(year_released) %>% summarize(avg_rate_per_year=mean(rating)) %>% 
  filter(year_released>=1950)
head(val_avg_rating_by_year_1950_or_later)

val_avg_rating_by_year_1950_or_later %>% ggplot(aes(year_released, avg_rate_per_year)) +
  geom_point(col='steelblue') +  geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange')  + ggtitle('Average rating by year')

val_y_hat_1950 <- predict(post_1950_model, val_avg_rating_by_year_1950_or_later)

val_rmse_1950 <- sqrt(mean((val_y_hat_1950 -
              val_avg_rating_by_year_1950_or_later$avg_rate_per_year)^2, na.rm = TRUE))
val_rmse_1950
#_______________________________________________________________________________________________

edx1 %>% group_by(released_to_rated) %>% summarize(avg_rating_by_avg_time_to_rate=mean(rating)) %>% 
  ggplot(aes(released_to_rated, avg_rating_by_avg_time_to_rate)) +
  geom_point(col='steelblue') + geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange')+
  ggtitle('Average rating per average number of year from release to rating')+
  labs(y="Average Ratings", x= 'Average number of years from date of release to date rated')

avg_rating_by_time_to_rate <- edx1 %>% filter((released_to_rated > 0) & released_to_rated < 60) %>%
          group_by(released_to_rated) %>%
          summarize(avg_rating_by_avg_time_to_rate=mean(rating)) 
      

avg_rating_by_time_to_rate  %>% ggplot(aes(released_to_rated, avg_rating_by_avg_time_to_rate)) +
  geom_point(col='steelblue') + geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange')+
  ggtitle('Average rating per average number of year from release to rating')+
  labs(x='Average time in years from release date to time of rating', y='Average Rating')

avg_by_time_to_rate_model <- lm(avg_rating_by_avg_time_to_rate ~ released_to_rated, avg_rating_by_time_to_rate)
summary(avg_by_time_to_rate_model)



y_hat_avg_time_to_rate <- predict(avg_by_time_to_rate_model, 
                                  avg_rating_by_time_to_rate)

rmse_avg_time_to_rate <- sqrt(mean((y_hat_avg_time_to_rate  - 
                      avg_rating_by_time_to_rate$avg_rating_by_avg_time_to_rate)^2, 
                            na.rm = TRUE))
rmse_avg_time_to_rate

val_avg_rating_by_time_to_rate <- val1 %>% filter((released_to_rated > 0) & released_to_rated < 60) %>%
  group_by(released_to_rated) %>%
  summarize(avg_rating_by_avg_time_to_rate=mean(rating)) 


val_avg_rating_by_time_to_rate  %>% ggplot(aes(released_to_rated, avg_rating_by_avg_time_to_rate)) +
  geom_point(col='steelblue') + geom_smooth(col='red', se=FALSE, method='auto') + 
  geom_smooth(method='lm', se=FALSE, col='orange')+
  ggtitle('Validation set:  Average rating per average number of year from release to rating')+
  labs(x='Average time in years from release date to time of rating', y='Average Rating')

val_y_hat_avg_time_to_rate <- predict(avg_by_time_to_rate_model, 
                                  val_avg_rating_by_time_to_rate)

val_rmse_avg_time_to_rate <- sqrt(mean((val_y_hat_avg_time_to_rate  - 
                                      val_avg_rating_by_time_to_rate$avg_rating_by_avg_time_to_rate)^2, 
                                   na.rm = TRUE))
val_rmse_avg_time_to_rate
