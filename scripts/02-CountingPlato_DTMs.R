library(stylo)
library(tidyr)
library(tidytext)
library(tibble)
library(dplyr)
library(purrr)
library(stringr)

# load corpus
corpus <- load.corpus.and.parse(corpus.dir = "corpus_new",
                                corpus.lang = "Other", 
                                features = "w", 
                                ngram.size = 1, 
                                encoding = "UTF-8", 
                                sampling = "no.sampling",
                                )

save(corpus, file = "./data/corpus.Rdata")

# tidy 
corpus_tidy <- corpus |> 
  stack() |> 
  rename(word = values, title = ind) |> 
  relocate(title, .before = word)

# general stats
corpus_counts <- corpus_tidy |> 
  group_by(title)  |>  
  count() |> 
  ungroup() |> 
  rename (total = n)

# word freq by title
corpus_word_counts <- corpus_tidy |> 
  group_by(title) |> 
  count(word) |> 
  ungroup()

# add total counts by title
corpus_counts_total <- corpus_word_counts |> 
  inner_join(corpus_counts)

# tf, idf, tf_idf by title
corpus_tf <- corpus_counts_total |> 
  bind_tf_idf(word, title, n)


# mfw
freq_vec <- corpus_tidy |> 
  count(word) |> 
  arrange(-n)  |> 
  pull(word)


# several mfw lists
mfws_nr <- seq(100, 600, 50) 
mfws <- map(mfws_nr, ~freq_vec[1:.])
save(mfws, file = "./data/mfws.Rdata")

# prepare dtm function
tf_to_dtm <- function(mfw_vec) {
  corpus_tf |> 
    filter(word %in% mfw_vec) |> 
    dplyr::select(title, word, tf)  |>  
    pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
    as.data.frame() |> 
    tibble::column_to_rownames("title")
}

# iterate
dtms <- map(mfws, tf_to_dtm)
names(dtms) <- paste0("dtm_", mfws_nr)

save(dtms, file = "./data/dtms.Rdata")

