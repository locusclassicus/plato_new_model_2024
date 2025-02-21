# библиотеки
library(stylo)
library(tidyverse)
library(ape)
library(philentropy)
library(TreeTools)
library(phangorn)
library(paletteer)

# консенсус stylo
par(mar=c(0,0,0,0), font.lab = 1)
bct_result <- stylo(corpus.dir = "corpus_new",
                    analysis.type = "BCT",
                    mfw.min = 50,
                    mfw.max = 450,
                    mfw.incr = 50,
                    distance.measure = "wurzburg",
                    write.png.file = TRUE,
                    gui = FALSE,
                    corpus.lang = "Other",
                    consensus.strength = 0.5,
                    rooted = TRUE,
                    plot.custom.width = 9, 
                    plot.custom.height = 9,
                    colors.on.graphs = "black",
)

# consensus phangorn
load("./data/dtms.Rdata")

# dtm
dtm300 <- dtms[[5]]


# функция для вычисления расстояния
dtm_to_dist <- function(data){
  dist_mx <- data |> 
    scale() |> 
    philentropy::distance(method = "cosine", use.row.names = T) |>  
    as.dist()
  
  return(1 - dist_mx)
} 

# матрица расстояния
dist_mx <- dtm_to_dist(dtm300)

# кластеризация NJ
nj <- nj(dist_mx)

nj |> 
  plot(type = "unrooted",
       lab4ut = "axial")

# bootstrap
FUN <- function(xx) nj(dtm_to_dist(xx)) 
tree <- FUN(dtm300)
bs <- boot.phylo(tree, dtm300, FUN, B = 100, block = 1,
                 rooted = FALSE, trees = TRUE)

# consensus
cons <- consensus(bs$trees, p = 0.5)

# # colors
# cols <- paletteer_d("MetBrewer::Renoir")
# 
# cols_tbl <- tibble(label = cons$tip.label) |> 
#   mutate(color = case_when(str_detect(label, "L") ~ cols[5],
#                            str_detect(label, "R") ~ cols[11],
#                            str_detect(label, "O") ~ cols[2],
#                            str_detect(label, "Lys") ~ cols[7],
#                            str_detect(label, "X") ~ cols[12],
#                            str_detect(label, "T") ~ cols[3],
#                            .default = cols[4]))

#cons$color <- cols_tbl$color

png(filename="img5.png",
    width = 8, height = 5, units = "in", pointsize = 9,
    bg = "white",  res = 300)
par(mar = c(0,0,0,0))
plot(cons, 
     type = "unrooted", 
     lab4ut = "axial",
     tip.color = "black", 
     edge.color = "grey50", 
     cex = 0.8,
     label.offset = 0.1)
dev.off()

