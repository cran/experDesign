#' Select the subset of extreme cases to evaluation
#'
#' Subset some samples that are mostly different.
#' @param size The number of samples to subset.
#' @inheritParams qcSubset
#' @inheritParams design
#' @return A vector with the number of the rows that are selected.
#' @seealso [optimum()]
#' @export
#' @examples
#' metadata <- expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
#'  sex = c("Male","Female"))
#' sel <- extreme_cases(metadata, 10)
#' # We can see that it selected both Female and Males and wide range of height
#' # and weight:
#' metadata[sel, ]
extreme_cases <- function(pheno, size, omit = NULL, iterations = 500) {

  # Calculate batches
  pheno_o <- omit(pheno, omit)
  num <- is_num(pheno_o)
  original_pheno <- .evaluate_orig(pheno_o, num = num)

  # Find the numeric values
  dates <- vapply(pheno_o, is_date, logical(1L))
  if (any(dates)) {
    warning("The dates will be treat as categories")
  }

  nSamples <- nrow(pheno)
  opt <- -Inf
  size_batches <- internal_batches(nSamples, size, 1)
  for (x in seq_len(iterations)) {
    i <- create_index(nSamples, size_batches, 1)
    # Can't use .check_index because there we want to control just some variables.
    # Double check this!
    subsets <- .evaluate_index(i, pheno_o, num)
    # Evaluate the differences between the subsets and the originals
    differences <- drop(abs(sweep(subsets, c(1, 2), original_pheno)))
    differences <- differences[-c(1, 4), ]
    differences["entropy", ] <- differences["entropy", ]/0.5

    # Maximize the entropy and the dispersion
    optimize <- sum(colSums(differences))

    # store index if "better"
    if (optimize > opt) {
      opt <- optimize
      val <- i
    }
  }
  unlist(val, use.names = FALSE)
}

#' Random subset
#'
#' Select randomly some samples from an index
#' @param index A list of indices indicating which samples go to which subset.
#' @param size The number of samples that should be taken.
#' @param each A logical value if the subset should be taken from all the
#' samples or for each batch.
#' @export
#' @examples
#' set.seed(50)
#' index <- create_subset(100, 50, 2)
#' QC_samples <- qcSubset(index, 10)
#' QC_samplesBatch <- qcSubset(index, 10, TRUE)
qcSubset <- function(index, size, each = FALSE) {

  if (!is_logical(each)) {
    stop("each should be either TRUE or FALSE")
  }

  if (!is_numeric(size)) {
    stop("size should be a numeric value")
  }

  if (!is.list(index)) {
    stop("index should be a list with numeric values")
  }

  if (each) {
    out <- lapply(index, sample, size = size)
  } else {
    out <- sample(unlist(index, recursive = FALSE, use.names = FALSE), size = size)
  }
  out
}

#' Check index distribution on batches
#'
#' Report the statistics for each subset and variable compared to the original.
#'
#' The closer the values are to 0, the less difference is with the original
#' distribution, so it is a better randomization.
#' @inheritParams design
#' @inheritParams qcSubset
#' @return A matrix with the differences with the original data.
#' @seealso Functions that create an index [design()], [replicates()],
#' [spatial()]. See also [create_subset()] for a random index.
#' @export
#' @examples
#' index <- create_subset(50, 24)
#' metadata <- expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
#'                         sex = c("Male","Female"))
#' check_index(metadata, index)
check_index <- function(pheno, index, omit = NULL) {
  batches <- length(index)

  pheno_o <- omit(pheno, omit)
  if (!.check_data(pheno_o)) {
    warning("There might be some problems with the data use check_data().")
  }
  num <- is_num(pheno_o)
  eval_n <- evaluations(num)
  original_pheno <- .evaluate_orig(pheno_o, num)
  original_pheno["na", ] <- original_pheno["na", ]/batches

  .check_index(index, pheno_o, num, eval_n, original_pheno)
}


.check_index <- function(index, pheno_o, num, eval_n, eval_orig) {

  subsets <- .evaluate_index(index, pheno_o, num)
  # Evaluate the differences between the subsets and the originals
  differences <- abs(sweep(subsets, c(1, 2), eval_orig))
  # Add the independence of the categorical values
  subset_ind <- evaluate_independence(index, pheno_o)
  # Calculate the score for each subset by variable
  mean_difference(differences, subset_ind, eval_n)
}
