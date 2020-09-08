## ----knitsetup, message=FALSE, warning=FALSE, include=FALSE-------------------
knitr::opts_knit$set(root.dir = ".")
knitr::opts_chunk$set(collapse = TRUE, warning = TRUE)
BiocStyle::markdown()
library("BiocStyle")
set.seed(445)
library("experDesign")

## -----------------------------------------------------------------------------
data(survey, package = "MASS") 
head(survey)

## ---- fig.show='hold'---------------------------------------------------------
# To reduce the variables used:
omit <- c("Wr.Hnd", "NW.Hnd", "Fold", "Pulse", "Clap", "Exer", "Height", "M.I")
(keep <- colnames(survey)[!colnames(survey) %in% omit])
head(survey[, keep])

# Looking for groups at most of 70 samples.
index <- design(pheno = survey, size_subset = 70, omit = omit)
index

## ----batch_names--------------------------------------------------------------
batch_names(index)

## ----evaluate_index-----------------------------------------------------------
out <- evaluate_index(index, survey[, keep])
out[, "Age", ]

## ----evaluate_orig------------------------------------------------------------
orig <- evaluate_orig(survey)
orig[, "Age"]

## ----unbalanced---------------------------------------------------------------
n <- 99
samples <- 100
unbalanced <- data.frame(Classroom = rep(c("A", "B"), each = samples/2),
                         Sex = c(rep("M", n), rep("F", samples-n)),
                         Age = rnorm(samples, mean = 25, sd = 3))
table(unbalanced)[, , 1:5]

## -----------------------------------------------------------------------------
i <- design(unbalanced, 15)

# Mean entropy en each subset
rowMeans(evaluate_index(i, unbalanced)["entropy", , ])
# Original entropy on the dataset
evaluate_orig(unbalanced)["entropy", ]
# Dispersion of the entropy
apply(evaluate_index(i, unbalanced)["entropy", , ], 1, sd)

## ----QC-----------------------------------------------------------------------
samples <- extreme_cases(survey, size = 10)
survey[samples, ]

## ----replicates---------------------------------------------------------------
# Looking for groups at most of 70 samples.
index_replicates <- replicates(pheno = survey, size_subset = 70, 
                               controls = 10, omit = omit)
index_replicates

## -----------------------------------------------------------------------------
survey[Reduce(intersect, index_replicates), ]

## ----batch--------------------------------------------------------------------
batch <- batch_names(index)

## -----------------------------------------------------------------------------
df <- inspect(index, survey, omit = omit)
head(df)

## -----------------------------------------------------------------------------
evaluate_entropy(index, survey)
evaluate_independence(index, survey)

evaluate_na(index, survey)

evaluate_mean(index, survey)
evaluate_sd(index, survey)
evaluate_mad(index, survey)

## -----------------------------------------------------------------------------
ev <- evaluate_index(index, survey)
ev["entropy", "Sex",]
ev[1:4, "Age",]
evaluate_independence(index, survey)

## -----------------------------------------------------------------------------
sessionInfo()

