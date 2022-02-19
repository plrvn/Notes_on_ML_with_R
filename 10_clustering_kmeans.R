library(tidyverse)
library(psychTools)

# data -----------------------------------------------------------------------------------------------

data(bfi)
data(bfi.dictionary)

## plot exploration

ggplot(bfi, aes(x = A2, y = A1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method=lm)

# k-means clustering ---------------------------------------------------------------------------------

set.seed(123)

km.out <- kmeans(bfi %>% drop_na(), centers = 5, nstart = 50)

# predictions ----------------------------------------------------------------------------------------

summary(km.out)

km.out$cluster

plot(bfi$A1, bfi$A2, col = km.out$cluster)

cluster_summary <- bfi %>% drop_na() %>% 
  mutate(cluster = km.out$cluster) %>% 
  group_by(cluster) %>% 
  summarize_all(mean)

# determining clusters -----------------------------------------------------------------------------

# Initialize total within sum of squares error: wss
wss <- 0

# data 
x <- 
  bfi %>% drop_na

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x, centers = i, nstart = 20, iter.max = 50)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# final model --------------------------------------------------------------------------------------

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

km.out <- kmeans(x, centers = 5, nstart = 20, iter.max = 50)
