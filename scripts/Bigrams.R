library(tidytext)
library(scales)

# load corpus
# corpus <- load.corpus.and.parse(corpus.dir = "plato_corpus_ht",
#                                 corpus.lang = "Other", 
#                                 features = "w", 
#                                 ngram.size = 2, 
#                                 encoding = "UTF-8", 
#                                 sampling = "no.sampling",
# )

#save(corpus, file = "./data/corpus2gram.Rdata")

# tidy 
corpus_tidy <- corpus |> 
  stack() |> 
  rename(ngram = values, title = ind) |> 
  relocate(title, .before = ngram)

# ngram freq per title
corpus_ngram_counts <- corpus_tidy |> 
  group_by(title) |> 
  count(ngram) |> 
  ungroup()

# tf, idf, tf-idf per title
corpus_tf <- corpus_ngram_counts |> 
  bind_tf_idf(ngram, title, n)

# plot καὶ μάλα
corpus_tf |> 
  filter(str_detect(ngram, "καὶ μάλα")) |> 
  arrange(-tf) |> 
  mutate(title = str_remove(title, "^.+?_")) |> 
  slice_head(n = 20) |> 
  ggplot(aes(reorder(title, tf), tf, fill = title)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(x = NULL) +
  theme_light()

# plot Republic
corpus_tf |> 
  filter(str_detect(title, "Repub")) |> 
  mutate(title = str_remove(title, "^.+?_")) |> 
  group_by(ngram) |> 
  summarise(mean_tf = mean(tf)) |> 
  ungroup() |> 
  arrange(-mean_tf) |> 
  slice_head(n = 20) |> 
  ggplot(aes(reorder(ngram, mean_tf), mean_tf, fill = ngram)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(x = NULL) +
  theme_light()



