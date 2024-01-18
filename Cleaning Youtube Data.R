setwd("C:/Users/Siah Jin Thau/OneDrive/Desktop/Documents/Professional/Projects/Youtube Trending Video Analysis Project")

library(dplyr)

source('Importing Youtube Data.R')

sum(is.na(NA_youtube_trending_data))

NA_youtube_trending_data = NA_youtube_trending_data %>%
  select(!c("video_id",
            "channelId",
            "categoryId",
            "thumbnail_link"))

NA_youtube_trending_data$published_Date = as.Date(
  NA_youtube_trending_data$publishedAt,
  format='%Y-%m-%d')

NA_youtube_trending_data$trending_Date = as.Date(
  NA_youtube_trending_data$trending_date,
  format='%Y-%m-%d')

NA_youtube_trending_data = NA_youtube_trending_data %>%
  select(!c("publishedAt", "trending_date"))

NA_youtube_trending_data$tags = 
  as.list(
    strsplit(NA_youtube_trending_data$tags, split = "|", fixed=TRUE))

NA_youtube_trending_data = NA_youtube_trending_data %>% 
  mutate(
    across(c(channelTitle, 
             comments_disabled,
             ratings_disabled,
             categoryName), 
           as.factor)
  )

glimpse(NA_youtube_trending_data)

