load("./data/dtms.Rdata")

# библиотеки
library(tidyverse)
library(dendextend)
library(paletteer)

# матрицы с частотностями
dtm100 <- dtms[[1]]
dtm200 <- dtms[[3]]

# более короткие имена для наглядности

names_old <- rownames(dtm100)
names_new <- str_remove(names_old, ".+?_")

rownames(dtm100) <- names_new
rownames(dtm200) <- names_new

# выборка 10 наблюдений
idx <- which(rownames(dtm100) %in% c("Alcibiades1", "Laches", "Crito", "Lysis", "Parmenides", "Phaedrus", "Cratylus", "Laws_6", "Laws_7", "Laws_9"))

dtm100 <- dtm100[idx, ]
dtm200 <- dtm200[idx, ]

# функция для вычисления расстояния и кластеризации
dtm_to_clust <- function(data){
  dist_mx <- data |> 
    scale() |> 
    philentropy::distance(method = "cosine", use.row.names = T) 
    
   hc <-  as.dist(1 - dist_mx) |> 
     hclust(method = "ward.D2") 
   
   return(hc)
}

# применяем функцию к матрицам с частотностями 
hc100 <- dtm_to_clust(dtm100)
hc200 <- dtm_to_clust(dtm200)

# выбор палитры (b/w)
cols <- pals::stepped()[c(21,23,22)]

# танглграмма
d1 <- hc100 |> 
  as.dendrogram()  |>  
  set("labels_col", value = cols, k=3)  |> 
  set("branches_k_color", value = cols, k = 3)

d2 <- hc200 |> 
  as.dendrogram() |> 
  set("labels_col", value = cols, k=3)  |> 
  set("branches_k_color", value = cols, k = 3)

dlist <- dendlist(d1, d2)

par(family = "Arial Bold")
tanglegram(dlist, 
           common_subtrees_color_lines = FALSE, 
           highlight_distinct_edges  = TRUE, 
           highlight_branches_lwd=FALSE, 
           margin_inner=8, 
           margin_outer = 1.5,
           lwd=2, 
           edge.lwd = 2,
           axes=FALSE, 
           main_left = "100 mfw", 
           main_right = "200 mfw", 
           cex_main = 1.5,
           lab.cex = 1.1)


