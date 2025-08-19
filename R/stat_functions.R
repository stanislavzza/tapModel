#' Fleiss kappa
#' @description Function to calculate Fleiss' kappa for binary data
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters for each subject and number of raters who rated class 1, and n for the
#' number of these (N_r, N_c) pairs. Optionally the row counts can be used, and the
#' function will convert
#' @return a data frame with one row and columns for parameters: t, a, p, log likelihood: ll,
#' and a true/false flag for degenerate solutions.
#' @details
#' The log likelihood returned is the expected value given the parameters, using `expected_bits_per_rating()`
#'
#' @export
fleiss_kappa <- function(counts) {

  # verify that the counts are in the right format, fix if we can
  verify_counts(counts)

  matches <- function(N_c, N_r) {
    N_c_c  = N_r - N_c                # number of raters who rated class 0
    N_c*(N_c-1)/2 + N_c_c*(N_c_c-1)/2 # sums up matches of class 1 + matches of class 0
  }

  # Calculate the proportion of raters that assigned each category (0 or 1) to each case
  stats <- counts |>
    mutate(p_ij = N_c / N_r, # random Pr[Class = 1]
           q_ij = 1 - p_ij,  # random Pr[Class = 0]
           m = matches(N_c, N_r), # actual matches
           p = m / matches(N_r,N_r)) |> # actual match rate
    summarize(N = sum(n), #total number of cases
              P_bar= sum(p*n)/N, # n is the multiplicity of each (N_r, N_c) pair
              c = sum(N_c*n)/sum(N_r*n),
              P_e_bar= c^2 + (1-c)^2) # probability of random match

  # Calculate Fleiss' kappa
  kappa <- (stats$P_bar - stats$P_e_bar) / (1 - stats$P_e_bar)

  # see problems to kappa=0
  kappa <- if_else(kappa < 0 | is.na(kappa) | is.nan(kappa), 0, kappa)

  t <- stats$c
  a <- sqrt(kappa)
  p <- stats$c # unbiased means t = p = c

  ll    <- bits_per_count(counts, params = list(t = t, a = a, p = p))
  degenerate <- is_degenerate(c(t, a, p)) || kappa <= 0

  return(data.frame(t = t, a = a, p = p, ll = ll, degenerate = degenerate))
}

#' Information bits from a binary vector
#' @param x a probability
#' @return bits of information using Shannon entropy
#' @export
information_bits <- function(x){
 if_else(x == 0 | x== 1, 0, -x*log2(x) - (1-x)*log2(1-x))
}

#' rater entropy
#' @description Calculates rater-specific entropy s-dot, which can be averaged
#' to estimate the average rater entropy.
#' @param rater_params Either a dataframe, typically from
#' `pull_rating_params(rating_params)$raters` with columns for rater_id, a_j, p_j,
#' OR the results of `fit_counts(counts)` with the average t-a-p parameters.
#' @return The same dataframe with an additional column s_dot for the
#' rater entropy.
#' @details This is experimental. It attempts to normalize bits per rating of
#' entropy by artificially setting t = .5, to remove the effect of the Class 1
#' proportion.
#' There are several functions that return bits per rating. Here's a
#' guide. `bits_per_rating(rating_params)` is the hierarchical version, using
#' the fitted model from `fit_ratings()` or `fit_ratings_mcmc()`. For the average
#' three-parameter t-a-p model use `bits_per_count(counts, params)`, which is
#' the same as upscaling the counts to rating_params and then using `bits_per_rating()`,
#' e.g. `counts |> as_rating_params(params) |> bits_per_rating()`, but it's a little
#' faster to do it directly. There's also a calculation for the expected value
#' of the bits per rating for a parameter set, using `params |> expected_bits_per_rating()`.
#' The `bits_per_rating()` function uses an intermediate function `ll_per_subject(rating_params)`,
#' which gives a subject-specific result in log likelihood, base e. This can be
#' useful for other purposes.
#' @export
rater_entropy <- function(rater_params){

  xlogx <- function(x){
    if_else(x == 0, 0, -x*log(x)/log(2))
  }

    rater_params |>
    mutate(s_dot = .5*( xlogx(a +(1-a)*(1-p))+
                         xlogx((1-a)*p) +
                         xlogx( (1-a)*(1-p)) +
                         xlogx(a + (1-a)*p)))
}

#' bernoulli trial
#' @description Simulate a Bernoulli trial with a given probability
#' @param p a vector of probabilities of success
#' @return a vector of 0s and 1s
#' @export
bernoulli_trial <- function(p){
  as.integer(runif(length(p)) < p)
}
