load("./data/dtms.Rdata")
dtm <- dtms[[1]]

library(FactoMineR)
library(factoextra)


# select rows
target <- c("Plato_Charmides", "Plato_Euthydemus",  "Plato_Lysis",  "Plato_Republic_1",
            "Plato_Republic_2",    "Plato_Republic_3",    "Plato_Republic_4",   "Plato_Republic_5",    "Plato_Republic_6",    "Plato_Republic_7",  "Plato_Republic_8",    "Plato_Republic_9",   
            "Semi_Laws_1",  "Semi_Laws_10",  "Semi_Laws_2", "Semi_Laws_3", "Semi_Laws_4", "Semi_Laws_7")
group <- c(rep("other", 4), rep("Republic", 8), rep("Laws", 6))

idx <- which(rownames(dtm) %in% target)
dtm_red <- dtm[idx, ]

# pca

pca_object <- PCA(dtm_red, graph = FALSE)


# plot ind
fviz_pca_biplot(pca_object, 
                geom = "point",
                habillage = as.factor(group),
                addEllipses = TRUE,
                select.var = list(cos2 = 20)
)

ggsave(filename = "img9.png", 
       width = 7,
       height = 7, 
       units = "in",
       bg = "white")

# count bigrams