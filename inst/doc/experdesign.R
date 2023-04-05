## ----knitsetup, message=FALSE, warning=FALSE, include=FALSE-------------------
knitr::opts_knit$set(root.dir = ".")
knitr::opts_chunk$set(collapse = TRUE, warning = TRUE)
set.seed(445)
library("experDesign")

## ----experDesign_setup--------------------------------------------------------
library("experDesign")
metadata <- expand.grid(height = seq(60, 80, 5), 
                        weight = seq(100, 300, 50),
                        sex = c("Male","Female"))
head(metadata, 15)

## ----check_data---------------------------------------------------------------
check_data(metadata)

## ----size---------------------------------------------------------------------
size_data <- nrow(metadata)
size_batch <- 24
(batches <- optimum_batches(size_data, size_batch))
# So now the best number of samples for each batch is less than the available
(size <- optimum_subset(size_data, batches))
# The distribution of samples per batch
sizes_batches(size_data, size, batches)

## ----design-------------------------------------------------------------------
desi <- design(metadata, size_batch)
# It is a list but we can convert it to a vector with:
batch_names(desi)

## ----replicates---------------------------------------------------------------
repli <- replicates(metadata, size_batch, 5)
lengths(repli)
repli

## ----spatial------------------------------------------------------------------
spati <- spatial(repli, metadata, rows = LETTERS[1:6], columns = 1:4)
head(spati)

## ----report-------------------------------------------------------------------
report <- inspect(repli, metadata)
report2 <- inspect(spati, report, index_name = "position")
head(report2)

## ----compare_index------------------------------------------------------------
desi2 <- create_subset(nrow(metadata), size_batch)
compare_index(metadata, desi, desi2)

## ----unbalanced---------------------------------------------------------------
n <- 99
samples <- 100
unbalanced <- data.frame(Classroom = rep(c("A", "B"), each = samples/2),
                         Sex = c(rep("M", n), rep("F", samples-n)),
                         Age = rnorm(samples, mean = 25, sd = 3))
table(unbalanced[, 1:2])

## ----unbalanced_design--------------------------------------------------------
i <- design(unbalanced, 15)
evaluation <- evaluate_index(i, unbalanced)
# Mean entropy en each subset
rowMeans(evaluation["entropy", , ])
# Original entropy on the dataset
evaluate_orig(unbalanced)["entropy", ]
# Dispersion of the entropy
apply(evaluation["entropy", , ], 1, sd)

## ----QC-----------------------------------------------------------------------
data(survey, package = "MASS") 
head(survey)
samples <- extreme_cases(survey, size = 10)
survey[samples, ]

## ----check_index--------------------------------------------------------------
check_index(unbalanced, i)

## ----internal, fig.show='hold'------------------------------------------------
# To reduce the variables used:
omit <- c("Wr.Hnd", "NW.Hnd", "Fold", "Pulse", "Clap", "Exer", "Height", "M.I")
(keep <- colnames(survey)[!colnames(survey) %in% omit])
head(survey[, keep])

# Set a seed for reproducibility
# Looking for groups at most of 70 samples.
index <-  create_subset(nrow(survey), size_subset = 70)
index

## ----check_index1-------------------------------------------------------------
score_index1 <- check_index(survey[, keep], index)
score_index1

## ----design2, warning=FALSE---------------------------------------------------
index2 <- create_subset(nrow(survey), size_subset = 70)

## ----check_index2-------------------------------------------------------------
score_index2 <- check_index(survey[, keep], index2)
sum(rowMeans(abs(score_index2-score_index1)))

## ----sessioninfo--------------------------------------------------------------
sessionInfo()

