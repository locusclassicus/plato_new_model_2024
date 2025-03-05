library(stylo)
library(tidyverse)
library(ggrepel)

# разделить на 2-грамы
my_corpus = load.corpus.and.parse(files = "all", 
                                  corpus.dir = "./corpus_new", 
                                  corpus.lang = "Other", 
                                  features = "w", 
                                  ngram.size = 2)

# тайдифицировать
my_corpus_tidy <- my_corpus |> 
  stack() |> 
  rename(title = ind, ngram = values) |> 
  relocate(title, .before = ngram)


# относительные частотности
total_counts <- my_corpus_tidy |> 
  add_count(title) |> 
  rename(total = n)

wide_counts <- my_corpus_tidy |> 
  count(title, ngram) |> 
  left_join(total_counts) |> 
  mutate(tf = n / total) |> 
  select(-n, -total) |> 
  distinct() |> 
  pivot_wider(id_cols = title, 
              names_from = ngram,
              values_from = tf,
              values_fill = 0)

# отбор признаков
idx <- str_detect(colnames(wide_counts), "(καὶ|ἀλλὰ|τί|γε) μ[ήὴ]ν$") |> 
  which()

mhn_counts <- wide_counts[,c(1,idx)]

# суммарные показатели
mhn_sums <- mhn_counts |> 
  mutate(total_mhn = rowSums(across(where(is.numeric)))) |> 
  select(title, total_mhn)

# группы по Диттенбергеру
mhn_sums <- mhn_sums |> 
  mutate(group = case_when(str_detect(title, "(Crito|Euthyph|Protag|Charm|Lach|HippiasMi|Euthyd|Meno|Gorg|Crat|Phaedo)") ~ "I",
                           str_detect(title, "(Symp|Lysis|Phaedr|Repub|Theaet)") ~ "IIa",
                           str_detect(title, "(Parmen|Sophis|Phileb|States|Laws)") ~ "IIb",
                           .default = "NA"))

save(mhn_sums, file = "./data/mhn_sums.Rdata")

# диаграмма рассеяния 
mhn_sums |> 
  ggplot(aes(title, total_mhn, color = group)) + 
  geom_point() +
  geom_text_repel(aes(label = title), label.size = 1) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

ggsave("Dittenberger.png", height = 9, width = 13)

# диаграмма размаха
mhn_sums |> 
  ggplot(aes(group, total_mhn, color = group)) + 
  geom_boxplot(fill = "white") +
  geom_jitter() +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  coord_flip()

ggsave("Dittenberger_boxplot.png", height = 7, width = 9)


# тесты
groupI <- mhn_sums |> 
  filter(group == "I") |> 
  pull(total_mhn)

groupIIa <- mhn_sums |> 
  filter(group == "IIa") |> 
  pull(total_mhn)

groupIIb <- mhn_sums |> 
  filter(group == "IIb") |> 
  pull(total_mhn)

groupNA <- mhn_sums |> 
  filter(group == "NA") |> 
  pull(total_mhn)

t.test(groupI, groupIIa)
t.test(groupIIa, groupIIb)
t.test(groupI, groupNA)

