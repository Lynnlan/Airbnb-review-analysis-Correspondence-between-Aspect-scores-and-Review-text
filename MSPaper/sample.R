setwd("/Users/ltl/Desktop/Github/thesis/MSPaper/Data")
library(tidyverse)

# Reviews data
reviews <- as.tibble(read.csv("./reviews.csv"))
# Airbnb homes data
abnb_homes <- as.tibble(read.csv("./listings.csv"))

# abnb_homes$number_of_reviews

#########################################################
# Process the reviews data

## Remove NA data
reviews <- na.omit(reviews)
glimpse(reviews)

## change to suitable column name
reviews <- reviews %>% rename(review_id = id) %>% rename(home_id = listing_id)
glimpse(reviews)

## change to suitable datatype
reviews$review_id <- factor(reviews$review_id)
reviews$home_id <- factor(reviews$home_id)
reviews$reviewer_id <- factor(reviews$reviewer_id)
glimpse(reviews)

#########################################################
# Process the reviews data

## Select needed data columns
abnb_homes_for_selection <- select(abnb_homes, 
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
glimpse(abnb_homes_for_selection)

##Remove NA data
  # removed 660 homes with no score information
abnb_homes_for_selection <- na.omit(abnb_homes_for_selection)
glimpse(abnb_homes_for_selection)

## Format to suitable data type
abnb_homes_for_selection$id <- factor(abnb_homes_for_selection$id)
abnb_homes_for_selection <- abnb_homes_for_selection %>% rename(home_id = id)
glimpse(abnb_homes_for_selection)

# combine reviews table and home table together
abnb_homes_reviews <- abnb_homes %>% left_join(reviews, by = "home_id")
glimpse(abnb_homes_reviews)


# Change price to int datatype
abnb_homes_for_selection$price <- stringr::str_extract(abnb_homes_for_selection$price, "\\d+")
abnb_homes_for_selection$price <- as.integer(abnb_homes_for_selection$price)
glimpse(abnb_homes_reviews)

# The distribution of homes' price
ggplot(abnb_homes_for_selection, aes(price)) +
  geom_freqpoly(
    breaks = seq(min(abnb_homes_for_selection$price) - 0.5, 
                 max(abnb_homes_for_selection$price) + 0.5,
                 by = 1)
  ) +
  geom_vline(xintercept = as.integer(mean(abnb_homes_for_selection$price)),
             color = 'red') +
  geom_vline(xintercept = as.integer(median(abnb_homes_for_selection$price)),
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

# The distribution of the number of reviews of each home
ggplot(abnb_homes, aes(number_of_reviews)) +
  geom_histogram(
    breaks = seq(50, 
                 max(abnb_homes$number_of_reviews) + 0.5,
                 by = 1)
  )

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