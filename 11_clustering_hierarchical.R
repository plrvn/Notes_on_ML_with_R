library(tidyverse)
library(psychTools)

# data -----------------------------------------------------------------------------------------------

data(bfi)
data(bfi.dictionary)

# need to normalize data -----------------------------------------------------------------------------
x <- bfi %>% drop_na()

colMeans(x)
apply(x, 2, sd)

scaled_x <- as_tibble(scale(x))

colMeans(x)
apply(scaled_x, 2, sd)

# k-means clustering ---------------------------------------------------------------------------------

set.seed(123)

# hierarchical clustering in R -----------------------------------------------------------------------

dist_matrix <- dist(scaled_x)

hclust.out <- hclust(d = dist_matrix, method = "complete")
# complete and average are most common linkage methods and tend to produce balanced trees

# draw a dendrogram
plot(hclust.out, main = "Dendrogram")
abline(h = 6, col = "red")

# cut by number of clusters k

cutree(hclust.out, k = 5)

scaled_x %>% mutate(cluster = cutree(hclust.out, k = 5)) %>% 
  group_by(cluster) %>% 
  summarize_all(mean)