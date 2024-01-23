# Set working directory
setwd("C:/Users/Siah Jin Thau/OneDrive/Desktop/Documents/Professional/Projects/Youtube Trending Video Analysis Project")

# Import packages
library(dplyr)
library(ggplot2)

# Import data from prev file
source('Cleaning Youtube Data.R')



NA_youtube_trending_data %>%
  group_by(category_name) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(category_name, Count), y = Count)) + 
  geom_bar(stat = "identity", fill = "white", color = "black") +
  geom_hline(yintercept = as.numeric(
    NA_youtube_trending_data %>%
      group_by(category_name) %>%
      summarise(Count = n()) %>%
      summarise(Avg = mean(Count))
  ), linetype = 2) +
  ggtitle("Most popular video categories") +
  xlab("Category Name") +
  ylab("Number of videos") +
  coord_flip()

NA_youtube_trending_data %>%
  mutate(DatesDiff = trending_date - published_date) %>%
  ggplot(aes(x = as.numeric(DatesDiff))) + 
  geom_bar(fill = "white", color = "black") +
  ggtitle("Distribution of Days until Trending, faceted by category") +
  xlab("Days until trending") +
  ylab("Count") +
  facet_wrap(~ category_name, 
             ncol = 3,
             scales = "free_y")

print(NA_youtube_trending_data %>%
        group_by(channel_title) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)), n = 25)

tagsList = unlist(NA_youtube_trending_data$tags)
tagsFrequency = as.data.frame(table(tagsList))
rm(tagsList)

tagsFrequency %>%
  arrange(desc(Freq)) %>%
  head()


NA_youtube_trending_data %>%
  ggplot(aes(likes, view_count)) +
  geom_point(alpha = 0.05) +
  geom_smooth(se = TRUE) +
  ggtitle("Scatterplot of View Count vs Likes") +
  xlab("Likes") +
  ylab("View Count") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10))

# Calculations for plotting density histograms

# Mean of log(view_count)
meanValvc = mean(NA_youtube_trending_data %>%
                 filter(view_count != 0) %>%
                 pull(view_count) %>%
                 log(base = 10))

# Standard deviation of log(view_count)
sdValvc = sd(NA_youtube_trending_data %>%
             filter(view_count != 0) %>%
             pull(view_count) %>%
             log(base = 10))

# Plotting density histogram of view count
NA_youtube_trending_data %>%
  filter(view_count != 0) %>%
  mutate(log_view_count = log(view_count, base = 10)) %>%
  ggplot(aes(x = log_view_count)) +
  geom_histogram(aes(y = ..density..), bins = 100,
                 fill = "white", color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = meanValvc,
                            sd = sdValvc),
                color = "red") +
  labs(title = "Density histogram of log(View Count)",
       subtitle = "With overlaid normal distribution",
       x = "log(View Count)", y = "Density")
rm(meanValvc, sdValvc)

# Given that the log(View Count) distribution closely
# resembles that of a normal distribution, we can
# conclude that View Count has a lognormal distribution

# Mean of log(likes)
meanVall = mean(NA_youtube_trending_data %>%
                 filter(likes != 0) %>%
                 pull(likes) %>%
                 log(base = 10))

# Standard deviation of log(likes)
sdVall = sd(NA_youtube_trending_data %>%
             filter(likes != 0) %>%
             pull(likes) %>%
             log(base = 10))

# Plotting density histogram of likes
NA_youtube_trending_data %>%
  filter(likes != 0) %>%
  mutate(log_likes = log(likes, base = 10)) %>%
  ggplot(aes(x = log_likes)) +
  geom_histogram(aes(y = ..density..), bins = 100,
                 fill = "white", color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = meanVall,
                            sd = sdVall),
                color = "red") +
  labs(title = "Density histogram of log(Likes)",
       subtitle = "With overlaid normal distribution",
       x = "log(View Count)", y = "Density")
rm(meanVall, sdVall)

# Given that the log(Likes) distribution closely
# resembles that of a normal distribution, we can
# conclude that Likes has a lognormal distribution

# Given that both view count and likes have a lognormal
# distribution, they need to be transformed before they
# can be modelled linearly.

NA_youtube_trending_data %>%
  filter(view_count != 0 & likes != 0 )%>%
  mutate(logViewCount = log(view_count),
         logLikes = log(likes)) %>%
  ggplot(aes(logLikes, logViewCount)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~ category_name, scales = "free") +
  geom_quantile(quantiles = c(0.1, 0.5, 0.9))

m1 = lm(logViewCount ~ logLikes, 
        data = NA_youtube_trending_data %>%
          filter(view_count != 0 & likes != 0 )%>%
          mutate(logViewCount = log(view_count),
                 logLikes = log(likes)))
summary(m1)

plot(m1, which = 1)
plot(m1, which = 3)

library(nlme)

vm1 <- gls(logViewCount ~ logLikes, 
           weights = varFixed(~ logLikes),
           data = NA_youtube_trending_data %>%
             filter(view_count != 0 & likes != 0 )%>%
             mutate(logViewCount = log(view_count),
                    logLikes = log(likes)))
summary(vm1)
plot(vm1)


vm2 <- gls(logViewCount ~ logLikes, 
           weights = varPower(form = ~ logLikes),
           data = NA_youtube_trending_data %>%
             filter(view_count != 0 & likes != 0 )%>%
             mutate(logViewCount = log(view_count),
                    logLikes = log(likes)))
summary(vm2)
plot(vm2)


vm3 <- gls(logViewCount ~ logLikes, 
           weights = varExp(form = ~ logLikes),
           data = NA_youtube_trending_data %>%
             filter(view_count != 0 & likes != 0 )%>%
             mutate(logViewCount = log(view_count),
                    logLikes = log(likes)))
summary(vm3)
plot(vm3)

vm4 <- gls(logViewCount ~ logLikes, 
           weights = varConstPower(form = ~ logLikes),
           data = NA_youtube_trending_data %>%
             filter(view_count != 0 & likes != 0 )%>%
             mutate(logViewCount = log(view_count),
                    logLikes = log(likes)))
summary(vm4)
plot(vm4)

NA_youtube_trending_data %>%
  ggplot(aes(dislikes, view_count)) +
  geom_point(alpha = 0.05) +
  ggtitle("Scatterplot of View Count vs Likes") +
  xlab("Likes") +
  ylab("View Count") +
  scale_y_log10() +
  scale_x_log10()

NA_youtube_trending_data %>%
  ggplot(aes(likes, dislikes)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = 'lm', fullrange = TRUE) +
  facet_wrap(~ category_name) +
  scale_y_log10() +
  scale_x_log10()

