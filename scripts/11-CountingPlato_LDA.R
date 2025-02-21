library(tidyverse)
library(tidymodels)
library(discrim)
library(stylo)
library(textrecipes)
library(tidytext)

# load data
corpus <- load.corpus.and.parse(corpus.dir = "plato_corpus_type")

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
  filter(!str_detect(word, "σώκρ")) |> 
  pull(word)

#  mfw vec
mfw_vec <- freq_vec[1:300]

# prepare dtm function
tf_to_dtm <- function(mfw_vec) {
  corpus_tf |> 
    filter(word %in% mfw_vec) |> 
    dplyr::select(title, word, tf)  |>  
    pivot_wider(names_from = word, values_from = tf, values_fill = 0) |> 
    as.data.frame() |> 
    tibble::column_to_rownames("title")
}


dtm300 <- tf_to_dtm(mfw_vec)

# type variable
old_rownames <- rownames(dtm300) 
type <- str_remove(old_rownames, "_.+")
new_rownames <- str_remove(old_rownames, ".+_")
rownames(dtm300) <- new_rownames

dtm_final <- dtm300 |> 
  mutate(type = type, .before = 1)


# initial split
data_split <- initial_split(dtm_final, strata = type, prop = 0.8)
data_train <- training(data_split)
data_test <- testing(data_split)

# vfolds
folds <- vfold_cv(data_train, strata = type, v = 10)

# prep recipe
lda_rec <- recipe(type ~ ., data = data_train) 

# prep model
lda_spec <- discrim_linear() |> 
  set_mode("classification")  |>  
  set_engine("MASS")

lda_spec

# workflow 
lda_wflow <- workflow() |> 
  add_model(lda_spec) |> 
  add_recipe(lda_rec)

lda_wflow


# fit_resamples
model_fit_resample <- lda_wflow  |>  
  fit_resamples(folds,
                control = control_resamples(save_pred = T))

# evaluate
collect_metrics(model_fit_resample) 

# roc curve
lda_pred <- collect_predictions(model_fit_resample, 
                                summarize = F)

# conf mat
conf_mat_resampled(model_fit_resample, tidy = F) |> 
  autoplot(type = "heatmap") +
  scale_fill_gradient(low = "#eaeff6", high = "#233857") +
  theme(panel.grid.major = element_line(colour = "#233857"),
        axis.text = element_text(color = "#233857"),
        axis.title = element_text(color = "#233857"),
        plot.title = element_text(color = "#233857")) +
  ggtitle("LDA, 300 признаков")

# fit model 
model_fit <- lda_wflow  |> 
  fit(data_train)

# examine fitted model
weights <- model_fit  |> 
  extract_fit_engine(estimated = T) |> 
  pluck("scaling") |> 
  as.data.frame() |> 
  tibble::rownames_to_column("term")

mins <- weights |>  
  slice_min(LD1, n = 10)

maxs <- weights |> 
  slice_max(LD1, n = 10)

# predict
pred_train <- predict(model_fit, data_train)


# validation set
predictions <- predict(model_fit, 
                       data_test, type = "class")

test_res <- tibble(predicted = predictions$.pred_class, 
                    expected = data_valid$type, 
                    value = predicted == expected)

sum(test_res$value) / nrow(test_res)

