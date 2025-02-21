load("./data/dtms.Rdata")
dtm <- dtms[[5]]

# select rows
target <- c("Plato_Charmides", "Plato_Euthydemus",  "Plato_Lysis",  "Plato_Republic_1",
            "Plato_Republic_2",    "Plato_Republic_3",    "Plato_Republic_4",   "Plato_Republic_5",    "Plato_Republic_6",    "Plato_Republic_7",  "Plato_Republic_8",    "Plato_Republic_9",   
            "Semi_Laws_1",  "Semi_Laws_10",  "Semi_Laws_2", "Semi_Laws_3", "Semi_Laws_4", "Semi_Laws_7")
idx <- which(rownames(dtm) %in% target)
dtm <- dtm[idx, ]

# add group
rownames_old <- rownames(dtm)
rownames_new <- str_remove(rownames_old, ".+?_")
rownames(dtm) <- rownames_new
dtm <- dtm |> 
  mutate(group = c(rep("A", 4), rep("B", 8), rep("C", 6))) |> 
  rownames_to_column("title")

# select discriminators
means_data <- dtm |> 
  dplyr::select(-title) |> 
  pivot_longer(-group, names_to = "word") |> 
  group_by(group, word) |> 
  summarize(means = mean(value)) |> 
  ungroup()

sds_data <- dtm  |> 
  dplyr::select(-title) |> 
  pivot_longer(-group, names_to = "word") |> 
  group_by(group, word) |>  
  summarize(sds = sd(value)) |> 
  ungroup()

joined_data <- means_data |> 
  left_join(sds_data)

a_data <- joined_data |> 
  filter(group == "A")  |>  
  dplyr::select(-group) |> 
  rename(means_a = means, sds_a = sds)

c_data <- joined_data |> 
  filter(group == "C") |> 
  dplyr::select(-group) |> 
  rename(means_c = means, sds_c = sds)

b_data <- joined_data |> 
  filter(group == "B") |> 
  dplyr::select(-group) |> 
  rename(means_b = means, sds_b = sds)

joined_ac <- a_data |> 
  left_join(c_data) |> 
  mutate(wt = (means_a - means_c) / (sds_a^2 + sds_c^2))

joined_ab <- a_data |> 
  left_join(b_data) |> 
  mutate(wt = (means_a - means_b) / (sds_a^2 + sds_b^2))

top_ab <- joined_ab  |>  
  slice_max(abs(wt), n = 100) |> 
  pull(word)

top_ac <- joined_ac  |>  
  slice_max(abs(wt), n = 100) |> 
  pull(word)

top_ac

stop <- c("φύσει", "χρόνον", "πόλεως", "πόλιν", "πόλει", "σώματος", "φύσιν",  
         "ψυχῆς", "ὄνομα",  "ὅς",  "ψυχῆς", "ἦν", "δ", "ἔφη", "ἦ", "ἦν", "ἐγώ", 
         "ἐγὼ")

