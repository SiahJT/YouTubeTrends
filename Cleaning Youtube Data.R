# Set working directory
setwd("C:/Users/Siah Jin Thau/OneDrive/Desktop/Documents/Professional/Projects/Youtube Trending Video Analysis Project")

# Import packages
library(dplyr)

# Import data from prev file
source('Importing Youtube Data.R')

# Check for NA/NULL values
sum(is.na(NA_youtube_trending_data))
# No NA/NULL values in the dataset

# Remove variables that will not be used in the analysis
# Video ID, Channel ID, Category ID are each unique random values
# Analyzing the thumbnails would be cool, but that will be saved
# for another, more visual heavy analysis
NA_youtube_trending_data = NA_youtube_trending_data %>%
  select(!c("video_id",
            "channelId",
            "categoryId",
            "thumbnail_link"))

# Convert date column data type from string to DateTime
NA_youtube_trending_data$published_Date = as.Date(
  NA_youtube_trending_data$publishedAt,
  format='%Y-%m-%d')
NA_youtube_trending_data$trending_Date = as.Date(
  NA_youtube_trending_data$trending_date,
  format='%Y-%m-%d')
NA_youtube_trending_data = NA_youtube_trending_data %>%
  select(!c("publishedAt", "trending_date"))

# Separate tags from one large string into a list, with
# each tag representing an element in the list
NA_youtube_trending_data$tags = 
  as.list(
    strsplit(NA_youtube_trending_data$tags, split = "|", fixed=TRUE))

# Convert to appropriate data types as needed
NA_youtube_trending_data = NA_youtube_trending_data %>% 
  mutate(
    across(c(channelTitle,
             categoryName), 
           as.factor)
  ) %>%
  mutate(
    across(c(comments_disabled,
             ratings_disabled), 
           as.logical)
  )

# Standardize column names
names(NA_youtube_trending_data) = 
  c("title", "channel_title", "tags", "view_count", "likes",
    "dislikes", "comment_count", "comments_disabled",
    "ratings_disabled", "description", "category_name",
    "published_date", "trending_date")

# Glimpse dataset
glimpse(NA_youtube_trending_data)
