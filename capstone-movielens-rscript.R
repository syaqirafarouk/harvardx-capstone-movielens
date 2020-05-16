library(tidyverse)
library(lubridate)
library(caret)
library(repmis)

#load the wrangled edx and validation sets from the Github repository from file "capstone-data.rda"
source_data("https://github.com/syaqirafarouk/harvardx-capstone-movielens/blob/master/capstone-data.rda?raw=true")

edx <- edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "week")) #convert timestamp (in seconds) into date and time rounded to the start of the week

set.seed(1,sample.kind = "Rounding")    #for R version 3.5 and earlier, use set.seed(1)
test_index<- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE) #create partitions on the edx set for training the algorithm
train_set<- edx[-test_index,]
test_set<- edx[test_index,]

test_set <- test_set %>%   #to ensure consistency in movieId,userId and date released variables between train_set and test_set
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "date")


#to find the most suitable value of lambda for regularization
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)    #the average rating for all movies by all users at all date released points
  b_i <- train_set %>%  #the average rating by movies (regularized movie effects)
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%   #the average rating by users (regularized user effects)
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_d <- train_set %>%  #the average rating by date released(rounded to start of week) (regularized date released effects)
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(date) %>%
    summarize(b_d = sum(rating - b_i - b_u - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by= "movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_d, by= "date" ) %>%
    mutate(pred_rating = mu + b_i + b_u + b_d) %>%
    .$pred_rating
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas,rmses) #visualise the change in rmses with differing lambdas

lambda <- lambdas[which.min(rmses)] #best lambda value to be used in predictions minimises RMSE
lambda 


#predictions from the edx set on the validation set using given lambda
mu <- mean(edx$rating)
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
b_d <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by= "userId") %>%
  group_by(date) %>%
  summarize(b_d = sum(rating - b_i - b_u - mu)/(n()+lambda))
predicted_ratings <- 
  validation %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_i, by= "movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_d, by= "date" ) %>%
  mutate(pred_rating = mu + b_i + b_u + b_d) %>%
  .$pred_rating

predicted_ratings #print the predicted ratings of the validation set 


#to compare between predicted and observed ratings
comparison <- validation %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_d, by= "date" ) %>%
  mutate(pred_rating = mu + b_i + b_u + b_d) %>%
  select(userId,movieId,timestamp,title,rating,pred_rating)

comparison #print the entire data frame for comparison


final_rmse<- RMSE(predicted_ratings,validation$rating)
final_rmse


################ algorithms and RMSE calculations for comparison models ###################

#"Only average effect"
predicted_ratings1 <- 
  validation %>% 
  mutate(pred_rating = mu) %>%
  .$pred_rating

RMSE1<- RMSE(predicted_ratings1,validation$rating)

#"Movie effects"
rmses2 <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)    
  b_i <- train_set %>%  
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by= "movieId") %>%
    mutate(pred_rating = mu + b_i) %>%
    .$pred_rating
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda2 <- lambdas[which.min(rmses2)]

b_i2 <- edx %>%  
  group_by(movieId) %>%
  summarize(b_i2 = sum(rating - mu)/(n()+lambda2))
predicted_ratings2 <- 
  validation %>% 
  left_join(b_i2, by= "movieId") %>%
  mutate(pred_rating = mu + b_i2) %>%
  .$pred_rating

RMSE2<- RMSE(predicted_ratings2,validation$rating)

#"User + Date released effects"
rmses3 <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)    
  b_u <- train_set %>%   
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu)/(n()+l))
  b_d <- train_set %>%  
    left_join(b_u, by= "userId") %>%
    group_by(date) %>%
    summarize(b_d = sum(rating - b_u - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_u, by= "userId") %>%
    left_join(b_d, by= "date" ) %>%
    mutate(pred_rating = mu + b_u + b_d) %>%
    .$pred_rating
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda3 <- lambdas[which.min(rmses3)]

b_u3 <- edx %>%   
  group_by(userId) %>%
  summarize(b_u3 = sum(rating - mu)/(n()+lambda3))
b_d3 <- edx %>%  
  left_join(b_u3, by= "userId") %>%
  group_by(date) %>%
  summarize(b_d3 = sum(rating - b_u3 - mu)/(n()+lambda3))
predicted_ratings3 <- 
  validation %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_u3, by= "userId") %>%
  left_join(b_d3, by= "date" ) %>%
  mutate(pred_rating = mu + b_u3 + b_d3) %>%
  .$pred_rating

RMSE3<- RMSE(predicted_ratings3,validation$rating)


#creating a data frame comparing all the models
model_comparisons<- data.frame(model = c("Only average effect","Movie effects","User + Date released effects","Movie + User + Date released effects"),
                               RMSE = c(RMSE1,RMSE2,RMSE3,final_rmse))

model_comparisons