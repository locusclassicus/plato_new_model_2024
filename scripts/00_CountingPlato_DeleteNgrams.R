# this step is needed to minimize the influence of the genre on the clusterization

library(stringr)
library(purrr)
library(stylo)
library(readr)

# separate folders for direct and reported dialogues

what <- list.files("plato_corpus_type")
direct <- what[str_detect(what, "^D")]
reported <- what[str_detect(what, "^R")]

dir.create("corpus_direct")
dir.create("corpus_reported")

old_direct <- paste0("./plato_corpus_type/", direct)
new_direct <- paste0("./corpus_direct/", direct)

walk2(old_direct, new_direct, file.copy)

old_reported <- paste0("./plato_corpus_type/", reported)
new_reported <- paste0("./corpus_reported/", reported)

walk2(old_reported, new_reported, file.copy)

# zeta score

result <- oppose(corpus.format = "plain",
       corpus.lang = "Other",
       primary.corpus.dir = "corpus_direct" , 
       secondary.corpus.dir = "corpus_reported", 
       splitting.rule = "[ \t\n]+",
       text.slice.length = 1000,
       text.slice.overlap = 0,
       rare.occurrences.threshold = 3,
       zeta.filter.threshold = 0.05,
       oppose.method = "craig.zeta",
       display.on.screen = TRUE,
       ngram.size = 3,
       gui = FALSE)

markers <- c(result$words.avoided[1:4], "ὦ σώκρατες", "ἔφη[, ]", "ἔφην ἐγώ", "ὦ ξένε")
regex <- str_c(markers, collapse = "|")
regex

# clean corpus
old_files <- list.files("plato_corpus_ht", full.names = TRUE)
corpus_old <- map(old_files, read_lines)
names(corpus_old) <- list.files("plato_corpus_ht")
corpus_old <- map(corpus_old, str_squish)

# check 
str_view(corpus_old[["Plato_Republic_7.txt"]], regex)

# replace
corpus_new <- map(corpus_old, str_replace_all, regex, " ")

# check
str_view(corpus_new[["Plato_Republic_7.txt"]], regex)

# write 
dir.create("corpus_new")

names_new <- paste0("corpus_new/", list.files("plato_corpus_ht"))

walk2(corpus_new, names_new, write_lines)


# some renaming for the visualizations 
names_old <- list.files("corpus_new", full.names = TRUE)
names_new <- str_replace(names_old, "Semi_Laws", "L_Laws")
names_new <- str_replace(names_new, "Plato_Republic", "R_Republic")
names_new <- str_replace(names_new, "Plato|Hypo|Dub|Pseudo|Semi", "O")
names_new <- str_replace(names_new, "Xen", "X")

names_new

walk2(names_old, names_new, file.rename)
