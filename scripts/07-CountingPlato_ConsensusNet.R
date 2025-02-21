library(phangorn)
library(TreeTools)
library(tidyverse)

# консенсус phylo
load("./data/dtms.Rdata")
dtm <- dtms[[5]]

# helper function: dtm to phylo
dtm_to_dist <- function(dtm) {
  dist_mx <- as.dist(1-philentropy::distance(scale(dtm),
                                             method = "cosine",
                                             use.row.names=T))
 }


# bootstrap
FUN <- function(xx) nj(dtm_to_dist(xx))  
tree <- FUN(dtm)
bs <- boot.phylo(tree, dtm, FUN, B = 100, block = 1,
                 rooted = FALSE, trees = TRUE)


# build consensus net
my_cons_net <- consensusNet(bs$trees, prob = 0.33, rooted = FALSE)

# plot consensus net
png(filename="img7.png",
    width = 7, height = 7, units = "in", pointsize = 9,
    bg = "white",  res = 300)
par(mar = c(0,0,0,0))
plot(my_cons_net,
     no.margin = TRUE,
     type = "equal angle",
     use.edge.length = FALSE,
     show.tip.label = TRUE,
     #tip.color = cols[factor(my_cons_net$group)],
     edge.color = "grey40",
     edge.width = 1,
     edge.lty = 1,
     font = 1, 
     cex = 1,
     underscore = T,
     direction = "axial")
dev.off()

