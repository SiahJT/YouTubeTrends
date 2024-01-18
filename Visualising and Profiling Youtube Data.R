setwd("C:/Users/Siah Jin Thau/OneDrive/Desktop/Documents/Professional/Projects/Youtube Trending Video Analysis Project")

library(dplyr)
library(ggplot2)

source('Cleaning Youtube Data.R')

glimpse(NA_youtube_trending_data)

NA_youtube_trending_data %>%
  group_by(categoryName) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(categoryName, Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "white", color = "black") +
  geom_hline(yintercept = as.numeric(
    NA_youtube_trending_data %>%
      group_by(categoryName) %>%
      summarise(Count = n()) %>%
      summarise(Avg = mean(Count))
  ), linetype = 2) +
  ggtitle("Most popular video categories") +
  xlab("Category Name") +
  coord_flip()

print(NA_youtube_trending_data %>%
        group_by(channelTitle) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)),
      n = 25)

NA_youtube_trending_data %>%
  mutate(DatesDiff = trending_Date - published_Date) %>%
  ggplot(aes(x = as.numeric(DatesDiff))) + 
  geom_bar() +
  ggtitle("Distribution of Days until Trending, faceted by category") +
  xlab("Days until trending") +
  ylab("Count") +
  facet_wrap(~ categoryName, 
             ncol = 3,
             scales = "free_y")

NA_youtube_trending_data %>%
  ggplot(aes(likes, view_count)) +
  geom_point(alpha = 0.05) +
  ggtitle("Scatterplot of View Count vs Likes") +
  xlab("Likes") +
  ylab("View Count") +
  scale_y_log10() +
  scale_x_log10()

NA_youtube_trending_data %>%
  ggplot(aes(likes, view_count)) +
  geom_point()
