library(philentropy)
library(tidyverse)

# might fail on mac, try posit.cloud
# or be brave and try everything else https://stackoverflow.com/questions/65251887/clang-7-error-linker-command-failed-with-exit-code-1-for-macos-big-sur
#remotes::install_github("KlausVigo/phangorn")
library(phangorn)

# mx
load("./data/dtms.Rdata")
dtm <- dtms[[5]]

# dist
dist_mx <- as.dist(1-philentropy::distance(scale(dtm),
                                           method = "cosine",
                                           use.row.names=T))
# nnet
nnet <- neighborNet(dist_mx)

# # color tips
# cols = paletteer_d("MetBrewer::Thomas")[2:8]
# col_tbl <- tibble(label = unique(nnet$label),
#                   col = cols[1:length(label)])
# color_group <- tibble(label = nnet$label) |>  
#   left_join(col_tbl)
 
# assigning tip cols 
#nnet$tip_color <- color_group$col

# assigning edge cols
# top_splits <- tibble(edge_length = nnet$edge.length,
#                  split_id = nnet$splitIndex) |> 
#   filter(split_id > 105) |> 
#   arrange(-edge_length) |> 
#   distinct() |> 
#   slice_head(n = 15) |> 
#   pull(split_id)

splits_tbl <- tibble(edge_length = nnet$edge.length,
                     split_id = nnet$splitIndex) |> 
  mutate(col = case_when(split_id %in% c(180, 153, 156, 185, 189) ~ "black",
                         .default = "grey70"))
  # mutate(col = case_when(split_id == 180 ~ "black",
  #                        split_id == 153 ~ "red",
  #                        split_id == 156 ~ "green",
  #                        split_id == 185 ~ "blue",
  #                        split_id == 189 ~ "yellow",
  #                        .default = "grey70")) 
         
  
# попытка что-то изобразить
png(filename="img8.png",
    width = 9, height = 9, units = "in", pointsize = 9,
    bg = "white",  res = 300)
par(mar = c(0,0,0,0))
plot(nnet,
      show.tip.label = TRUE,
      tip.color = "grey20",
      type = "equal angle",
      edge.color = splits_tbl$col,
      edge.width = 1.5,
      # show.edge.label = TRUE,
      # col.edge.label = "red",
      font = 1,
      cex = 1.2,
      use.edge.length = T,
      direction ="axial"
)
dev.off()



# library(rgl) 
# # create animated gif file 
# #play3d(spin3d(axis=c(0,1,0), rpm=6), duration=10)
# par3d(windowRect = c(20, 30, 800, 800))
# plot(nnet,
#      type = "3D",
#      tip.color = nnet$color, 
#      edge.color = "grey20",
#      edge.width = 1,
#      font = 1,
#      cex = 1,
#      use.edge.length = T,
#      label.offset = 0.1
# )
# movie3d(spin3d(axis=c(0,1,0), rpm=3), 
#         duration=8, 
#         dir = ".",  
#         type = "gif")
