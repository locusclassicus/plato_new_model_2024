library(stylo)
library(tidyverse)
library(tidytext)
library(paletteer)

corpus <- load.corpus.and.parse(corpus.dir = "corpus_new")

counts <- tibble(title = names(map_int(corpus, length)), 
                 length = map_int(corpus, length) |> unname()) |>
  arrange(length) |> 
  slice_head(n = 15) |> 
  separate(title, c("group", "title"), sep = "(?<=^.)_") |> 
  filter(!group %in% c("Lys", "T", "X")) |> 
  mutate(group = case_when(group == "S" ~ "Appendix",
                           group == "O" ~ "Тетралогии"
                           ))


#cols <- paletteer_d("MetBrewer::Renoir")
library(ggpattern)

counts |> 
  ggplot(aes(reorder(title, -length), length, 
             pattern_angle = group, 
             pattern_color = group,
             color = group)) +
  geom_col_pattern(linewidth = 0.3,
                   fill = "white",
                   pattern = "stripe",
                   pattern_fill = "grey30",
                   pattern_spacing = 0.012,
                   pattern_key_scale_factor = 0.6, 
                   pattern_size = 0.1,
                   width = 0.8) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL, y = "число слов") +
  theme_light(base_family = "sans") +
  scale_pattern_angle_manual(name = "Группа", values = c(45, 135)) +
  scale_pattern_color_grey(name = "Группа", start = 0.2, end = 0.6) +
  scale_color_grey(name = "Группа", start = 0.4, end = 0.8) +
  theme(legend.position = c(0.85,0.7),
        legend.box.background  = element_rect(color = "grey"))


         
  