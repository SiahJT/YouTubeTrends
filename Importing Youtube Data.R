setwd("C:/Users/Siah Jin Thau/OneDrive/Desktop/Documents/Professional/Projects/Youtube Trending Video Analysis Project")

library(jsonlite)
library(dplyr)

US_category_id = as.data.frame(fromJSON("Data/US_category_id.json"))

CA_category_id = as.data.frame(fromJSON("Data/CA_category_id.json"))

Category_id = unique(as.data.frame(
  cbind(
    c(US_category_id$items.id, CA_category_id$items.id),
    c(US_category_id$items.snippet$title, CA_category_id$items.snippet$title)
  )
))
rm(US_category_id, CA_category_id)

names(Category_id) = c("categoryId", "categoryName")
Category_id$categoryId = as.integer(Category_id$categoryId)

NA_youtube_trending_data = rbind(
  read.csv("Data/US_youtube_trending_data.csv", skipNul = TRUE),
  read.csv("Data/CA_youtube_trending_data.csv", skipNul = TRUE)
) %>%
  left_join(Category_id, by=c('categoryId' = 'categoryId'))
rm(Category_id)

glimpse(NA_youtube_trending_data)

