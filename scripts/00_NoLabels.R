names_old <- list.files("corpus_new")
names_new <- str_remove(names_old, "._")
names_new <- str_remove(names_new, "_")
names_new

names_old <- list.files("corpus_new", full.names = TRUE)
names_old

names_new <- paste0("corpus_new/", names_new)
names_new

walk2(names_old, names_new, file.rename)
