setwd("/Users/ltl/Desktop/2019-Spring/MSPaper/Data")
library(tidyverse)

# Reviews data
reviews <- as.tibble(read.csv("./reviews.csv"))
# Airbnb homes data
abnb_homes <- as.tibble(read.csv("./listings.csv"))

abnb_homes$number_of_reviews

# Remove NA data
reviews <- na.omit(reviews) %>% rename(review_id = id)
reviews$id <- factor(reviews$id)
reviews$listing_id <- factor(reviews$listing_id)

# Select required data columns
abnb_homes <- select(abnb_homes, 
                     id, 
                     property_type, 
                     room_type, 
                     price, 
                     number_of_reviews, 
                     review_scores_rating, 
                     review_scores_accuracy, 
                     review_scores_cleanliness, 
                     review_scores_checkin, 
                     review_scores_communication, 
                     review_scores_location, 
                     review_scores_value)

# Format to suitable data type
abnb_homes$id <- factor(abnb_homes$id)
abnb_homes <- abnb_homes %>% rename(listing_id = id)

# Remove NA data
abnb_homes <- na.omit(abnb_homes)

abnb_homes_reviews <- abnb_homes %>% left_join(reviews, by = "listing_id")


# The distribution of # of reviews of each home

ggplot(abnb_homes, aes(number_of_reviews)) +
  geom_histogram(
    breaks = seq(50, 
                 max(abnb_homes$number_of_reviews) + 0.5,
                 by = 1)
  )

abnb_homes$price <- stringr::str_extract(abnb_homes$price, "\\d+")
abnb_homes$price <- as.integer(abnb_homes$price)

sample1_homes <- abnb_homes %>% filter(price <= 250)

ggplot(sample1_homes, aes(price)) +
  geom_freqpoly(
    breaks = seq(min(sample1_homes$price) - 0.5, 
                 max(sample1_homes$price) + 0.5,
                 by = 1)
  ) +
  geom_vline(xintercept = as.integer(mean(sample1_homes$price)),
             color = 'red') +
  geom_vline(xintercept = as.integer(median(sample1_homes$price)),
             color = 'blue')


ggplot(abnb_homes, aes(price)) +
  geom_freqpoly(
    breaks = seq(min(abnb_homes$price) - 0.5, 
                 max(abnb_homes$price) + 0.5,
                 by = 1)
  ) +
  geom_vline(xintercept = as.integer(mean(abnb_homes$price)),
             color = 'red') +
  geom_vline(xintercept = as.integer(median(abnb_homes$price)),
             color = 'blue')


abnb_homes %>% 
  group_by(price) %>% 
  #filter(price <= 200 && price >= 75) %>%
  count() %>%
  ggplot(aes(price, n)) +
  geom_area() +
  geom_hline(yintercept = mean(na.omit(n)))


abnb_homes %>% 
  group_by(price) %>% 
  count()