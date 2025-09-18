#' lc_vsum: Vertical sum of list-column vectors
#'
#' Collapse N K-vectors into a single K-vector by summing elementwise.
#'
#' @param lc A list of numeric vectors (all length K).
#' @return A numeric vector of length K (the summed vector).
#' @export
lc_vsum <- function(lc) {
  if (length(lc) == 0) return(numeric())
  K <- length(lc[[1]])
  total <- numeric(K)
  for (vec in lc) {
    if (length(vec) != K) stop("Vector length mismatch")
    total <- total + vec
  }
  total
}

#' lc_add: Elementwise add of two list-columns
#'
#' @param lc1 A list of numeric vectors.
#' @param lc2 A list of numeric vectors (same length, same K).
#' @return A list-column with elementwise sums.
#' @export
lc_add <- function(lc1, lc2) {
  purrr::map2(lc1, lc2, ~ .x + .y)
}

#' lc_subtract: Elementwise subtraction of two list-columns
#'
#' @param lc1 A list of numeric vectors.
#' @param lc2 A list of numeric vectors (same length, same K).
#' @return A list-column with elementwise lc1 - lc2.
#' @export
lc_subtract <- function(lc1, lc2) {
  purrr::map2(lc1, lc2, ~ .x - .y)
}

#' lc_mpy: Multiply list-column by scalar or list-column
#'
#' @param lc1 A list of numeric vectors.
#' @param x Either a numeric vector (scalars) or a list of numeric vectors.
#' @return A list-column with elementwise products.
#' @export
lc_mpy <- function(lc1, x) {
  if (is.list(x)) {
    purrr::map2(lc1, x, ~ .x * .y)
  } else {
    purrr::map2(lc1, x, ~ .x * .y)
  }
}

#' lc_zero: Create zero-filled list-column
#'
#' @param N Number of rows.
#' @param K Length of each vector.
#' @return A list of N numeric vectors of length K, all zero.
#' @export
lc_zero <- function(N, K) {
  replicate(N, rep(0, K), simplify = FALSE)
}

#' lc_one: Create one-filled list-column
#'
#' @param N Number of rows.
#' @param K Length of each vector.
#' @return A list of N numeric vectors of length K, all one.
#' @export
lc_one <- function(N, K) {
  replicate(N, rep(1, K), simplify = FALSE)
}

#' lc_indicator: Create one-hot indicator vectors
#'
#' @param idx An integer vector with values 1..K.
#' @param K Number of categories.
#' @return A list of one-hot vectors of length K.
#' @export
lc_indicator <- function(idx, K) {
  purrr::map(idx, ~ {
    vec <- rep(0, K)
    vec[.x] <- 1
    vec
  })
}

#' lc_fn: Apply a function elementwise to list-column vectors
#'
#' @param lc A list of numeric vectors.
#' @param fn A function applied to each vector.
#' @return A list-column with fn applied to each vector.
#' @export
lc_fn <- function(lc, fn) {
  purrr::map(lc, fn)
}

#' lc_prob: Clamp and normalize list-column vectors
#'
#' Ensures each vector lies in [0,1] and sums to 1.
#'
#' @param lc A list of numeric vectors.
#' @param eps Small value for fallback uniform distribution.
#' @return A list-column of normalized probability vectors.
#' @export
lc_prob <- function(lc, eps = 1e-12) {
  purrr::map(lc, ~ {
    vec <- pmin(1, pmax(0, .x))
    sum_vec <- sum(vec)
    if (sum_vec < eps) rep(1/length(vec), length(vec)) else vec / sum_vec
  })
}

#' lc_dot: Rowwise dot product of two list-columns
#'
#' @param lc1 First list of numeric vectors.
#' @param lc2 Second list of numeric vectors.
#' @return A numeric vector of dot products.
#' @export
lc_dot <- function(lc1, lc2) {
  purrr::map2_dbl(lc1, lc2, ~ sum(.x * .y))
}

#' lc_norm: Norm of list-column vectors
#'
#' @param lc A list of numeric vectors.
#' @param type Either "l1" (sum of abs) or "l2" (Euclidean).
#' @return A numeric vector of norms.
#' @export
lc_norm <- function(lc, type = c("l2", "l1")) {
  type <- match.arg(type)
  purrr::map_dbl(lc, ~ {
    if (type == "l1") sum(abs(.x)) else sqrt(sum(.x^2))
  })
}

#' lc_bind: Bind list-column into a matrix
#'
#' @param lc A list of numeric vectors (all same length).
#' @return A numeric matrix with rows bound from the list-column.
#' @export
lc_bind <- function(lc) {
  do.call(rbind, lc)
}

#' lc_logsumexp: Rowwise log-sum-exp
#'
#' Compute log(sum(exp(x))) for each vector in a list-column, in a numerically stable way.
#'
#' @param lc A list of numeric vectors (each element is log-probs for a row).
#' @return A numeric vector, one value per row.
#' @export
lc_logsumexp <- function(lc) {
  purrr::map_dbl(lc, function(x) {
    m <- max(x)
    m + log(sum(exp(x - m)))
  })
}

#' lc_pull: Extract an element by index from each vector in a list-column
#'
#' @param lc A list of numeric vectors (all same length).
#' @param idx Integer index (1-based) of the element to extract.
#'
#' @return A numeric vector with the idx-th element from each vector.
#' @examples
#' lc <- list(c(0.7, 0.3), c(0.4, 0.6), c(1, 0))
#' lc_pull(lc, 2) # returns c(0.3, 0.6, 0.0)
#' @export
lc_pull <- function(lc, idx = 1) {
  purrr::map_dbl(lc, ~ .x[idx])
}

#' lc_clone: Clone selected element across each vector
#'
#' For each vector in a list-column, extract the value at the position
#' given by `idx` (which can vary by row), and replicate it across all
#' positions of that vector.
#'
#' @param lc A list of numeric vectors (all same length).
#' @param idx An integer vector of indices (1-based), same length as `lc`.
#'
#' @return A list-column where each vector has been replaced by
#'   a constant vector equal to the selected element.
#'
#' @examples
#' lc <- list(c(0.1, 0.2, 0.3, 0.4),
#'            c(5, 6, 7, 8))
#' idx <- c(3, 2)
#' lc_clone(lc, idx)
#' # [[1]] 0.3 0.3 0.3 0.3
#' # [[2]] 6.0 6.0 6.0 6.0
#' @export
lc_clone <- function(lc, idx) {
  purrr::map2(lc, idx, ~ rep(.x[.y], length(.x)))
}


