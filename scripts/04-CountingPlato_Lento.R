load("./data/dtms.Rdata")

# libraries
library(phangorn)
library(ape)
library(stringr)
library(dendextend)

# dtm
dtm200 <- dtms[[3]]

# более короткие имена для наглядности
names_old <- rownames(dtm200)
names_new <- str_remove(names_old, ".+?_")
rownames(dtm200) <- names_new

# выборка наблюдений
idx <- which(rownames(dtm200) %in% c("Alcibiades1", "Laches", "Crito", "Lysis", "Parmenides", "Phaedrus", "Cratylus", "Laws_6", "Laws_7", "Laws_9"))
dtm200 <- dtm200[idx, ]

# функция для вычисления расстояния
dtm_to_dist <- function(data){
  dist_mx <- data |> 
    scale() |> 
    philentropy::distance(method = "cosine", use.row.names = T) |>  
    as.dist()
  
  return(1 - dist_mx)
} 

# матрица расстояния
dist_mx <- dtm_to_dist(dtm200)

# кластеризация методом Уорда  
hc <- hclust(dist_mx, method = "ward.D2") 

# кластеризация NJ
nj <- nj(dist_mx)

# цвета 
cols <- paletteer_d("MetBrewer::Renoir")[c(2, 5, 7)]

# график
clus5 = cutree(hc, 3)
par(mar = c(0,0,0,0))
hc |>
  as.phylo() |>
  plot(type = "phylogram",
       tip.color = cols[clus5],
       edge.color = "grey40",
       cex = 0.7)

nj |> 
  plot(type = "unrooted")
  
# bootstrap
#FUN <- function(xx) nj(dtm_to_dist(xx)) 
FUN <- function(xx) as.phylo(hclust(dtm_to_dist(xx), method = "ward.D2")) 

tree <- FUN(dtm200)
bs <- boot.phylo(tree, dtm200, FUN, B = 100, block = 1,
                 rooted = TRUE, trees = TRUE)

# lento
s <- as.splits(bs$trees)
par(mar = c(0,2,2,0), cex = 0.7)
lento(s, trivial = TRUE, 
      xlim = c(0,57))


