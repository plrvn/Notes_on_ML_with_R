library(tidyverse)
library(psychTools)

# data -----------------------------------------------------------------------------------------------

data(bfi)
data(bfi.dictionary)

# need to normalize data -----------------------------------------------------------------------------
x <- bfi %>% drop_na()

scaled_x <- as_tibble(scale(x))

# pca ------------------------------------------------------------------------------------------------
pr.x <- prcomp(scaled_x[-(26:28)],
               scale = FALSE,
               center = FALSE)

summary(pr.x)

# visualizing results ------------------------------------------------------------------------------

biplot(pr.x)

# scree plot for proportion of variance

pr.var <- pr.x$sdev^2
pve <- pr.var / sum(pr.var)

plot(pve, xlab = "PC", ylab = "Proportion Explained", type = "b")