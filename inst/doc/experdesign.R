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

## ----size---------------------------------------------------------------------
size_data <- nrow(metadata)
size_batch <- 24
(batches <- optimum_batches(size_data, size_batch))
# So now the best number of samples for each batch is less than the available
(size <- optimum_subset(size_data, batches))
# The distribution of samples per batch
sizes_batches(size_data, size, batches)

## ----design-------------------------------------------------------------------
d <- design(metadata, size_batch)
# It is a list but we can convert it to a vector with:
batch_names(d)

## ----replicates---------------------------------------------------------------
r <- replicates(metadata, size_batch, 5)
lengths(r)
r

## ----spatial------------------------------------------------------------------
s <- spatial(r, metadata, rows = LETTERS[1:6], columns = 1:4)
head(s)

## ----report-------------------------------------------------------------------
report <- inspect(r, metadata)
report2 <- inspect(s, report, index_name = "position")
head(report2)

## ----unbalanced---------------------------------------------------------------
n <- 99
samples <- 100
unbalanced <- data.frame(Classroom = rep(c("A", "B"), each = samples/2),
                         Sex = c(rep("M", n), rep("F", samples-n)),
                         Age = rnorm(samples, mean = 25, sd = 3))
table(unbalanced)[, , 1:5]

## ----unbalanced_design--------------------------------------------------------
i <- design(unbalanced, 15)

# Mean entropy en each subset
rowMeans(evaluate_index(i, unbalanced)["entropy", , ])
# Original entropy on the dataset
evaluate_orig(unbalanced)["entropy", ]
# Dispersion of the entropy
apply(evaluate_index(i, unbalanced)["entropy", , ], 1, sd)

## ----QC-----------------------------------------------------------------------
data(survey, package = "MASS") 
head(survey)
samples <- extreme_cases(survey, size = 10)
survey[samples, ]

## ----sessioninfo--------------------------------------------------------------
sessionInfo()

