#' get ratings by sampling a known distribution
#' @param N_s number of subjects
#' @param N_r number of raters per subject
#' @param K the number of categories, labeled 1 to K
#' @param params A list with t, a, p. t and p are non-negative vectors of length K, which
#' will be normalized to make probability distributions. If NULL, uniform values are provided.
#' @param details logical defaulting to FALSE. If TRUE, return the full set
#' of parameters to include the T_i, P_ij, and A_ij values.
#' @return A list that comprises the cat_ratings type: data frames ratings, subjects, and raters,
#'  an integer K > 1 with the number of categories, and a vector labels that
#'  give names to the categorizes. For the sample ratings, nominal
#'  values are provided for subject and rater parameters (.5 each) and
#'  the category labels (Class 1, Class 2, etc).
#' @export
generate_sample_ratings_cat <- function(N_s = 100, N_r = 5,
                                        K = 3,
                                        params = NULL,
                                        details = FALSE) {

  if(K < 2) stop("K must be at least 2")

  if(is.null(params)){
    params <- list(t = rep(.5,K),
         a = .7,
         p = rep(.5,K))
  }

  # complete param list and put into environment
  list2env(params, envir = environment())
  t <- t/sum(t)
  p <- p/sum(p)


  subject_params <- tibble(subject_id = 1:N_s,
                           T_i = sample(1:K,
                                        N_s,
                                        replace = TRUE,
                                        prob = t))


  rater_params <- tibble(rater_id = 1:N_r)

  param_grid <- subject_params %>%
    cross_join(rater_params)

  n_ratings <- nrow(param_grid)

  # generate ratings based on t_i-a_j-p_i model
  param_grid <- param_grid %>%
    mutate(A_ij = as.integer(runif(n_ratings) < a),
           P_ij = sample(1:K, n_ratings, replace = TRUE, prob = p),
           C_ij = if_else(A_ij == 1,T_i,P_ij))

  if(details) {
    param_grid <- param_grid %>%
      select(subject_id, rating = C_ij, rater_id, T_i, A_ij, P_ij)
  } else {
    param_grid <- param_grid %>%
      select(subject_id, rating = C_ij, rater_id)
  }

  subjects <- param_grid |>
    distinct(subject_id) |>
    mutate(t = list(rep(1/K,K))) # nominal value

  raters <- param_grid |>
    distinct(rater_id) |>
    mutate(a = .5,
           p = list(rep(1/K,K))) # nominal value

  return(list(K = K,
              labels = paste("Class",1:K),
              ratings = param_grid,
              subjects = subjects,
              raters = raters))
}

#' Truth probabilities by Class 1 rate for each subject
#' @description Given a rating_params data frame, which contains the t, a, and p
#' parameters for each rating, as well as the rating, recalculate the t_i
#' for each subject using the general (multinomial) calculation.
#' @param cat_ratings, with the appropriate structure. See `generate_sample_ratings_cat()`
#' @return a cat_ratings object with updated t vectors
#'
#' @export
estimate_ti_cat <- function(cat_ratings){

  eps <- 1e-7

  # probabilities of random assignments for inaccurate ratings
  rater_pi <- cat_ratings$raters |>
    mutate(pi = map2(p, a, ~ (1 - .y) * .x)) |>
    select(rater_id, a, pi)

  # attach to ratings, adjusting for the assigned class
  # this generates a lot of warnings
  rating_log_pi <- cat_ratings$ratings |>
    left_join(rater_pi, by = "rater_id") |>
    # add rater's a to pi in the position of pi
    # that matches the rating = accurate ratings
    mutate(
      logpi = map2(pi, rating, ~ {
        .x[.y] <- .x[.y] + a  # add accuracy to the position matching the rating
        log(.x + eps) # log pr(T_i = k | rating_ij)
      })
    )

  subject_log_probs <- rating_log_pi |>
    group_by(subject_id) |>
    summarize(logprob = list(lc_vsum(logpi)))

  subject_probs <- subject_log_probs |>
    mutate(
      t = map(logprob, ~ {
        p_unnorm <- exp(.x - max(.x))  # stability trick
        p_unnorm / sum(p_unnorm)
      })
    ) |>
    select(subject_id, t)

  cat_ratings$subjects <- subject_probs

  return(cat_ratings)
}

#' Empirical confusion matrix for K-categories
#' @param rating_params_cat ratings with vector t added to
#' subjects and a, vector p
#' @param K the number of categories
#' @param normalize if TRUE normalizes sum(C) = 1
#' @details
#' This function is designed to be used inside data
#' grouped by rater if desired.
#' @return The estimated C matrix
estimate_C <- function(rating_params_cat, K, normalize = TRUE) {

  M <- matrix(0, nrow = K, ncol = K)

  for (i in seq_len(nrow(rating_params_cat))) {
    t_i <- unlist(rating_params_cat$t[[i]])
    r <- rating_params_cat$rating[i]
    if (!is.na(r) && r >= 1 && r <= K) {
      M[, r] <- M[, r] + t_i  # t_i contributes to column r
    }
  }

  if(normalize == FALSE) return(M)

  return(M/sum(M))
}

#' Estimate a and p for K categories
#' @param rating_params_cat ratings with vector t added to
#' subjects
#' @param K the number of categories
#' @return A 1-row data frame(a, p) with updated params
#' @export
estimate_a_p_cat <- function(rating_params_cat, K) {

  # empirical proportion matrix
  C <- estimate_C(rating_params_cat, K )

  # average t vector
  t <- lc_vsum(rating_params_cat$t) / nrow(rating_params_cat)

  # estimate a ########
  trace_C <- sum(diag(C))

  # Off-diagonal column sums
  C_offdiag <- colSums(C - diag(diag(C)))

  # t ratio
  t_ratio <- exp(log(t) - log(1-t))

  # Compute adjusted trace correction term
  a <- trace_C - sum(C_offdiag*t_ratio )

  # and p vector
  log_p_vector <- log(C_offdiag) - log(1-t) - log(1-a)

  # gracefully handle divide by zero since exp(-Inf) = 0
  p_vector <- exp(log_p_vector)

  return(tibble(a = a, p = list(p_vector)))

}

#' Perform an EM step for categorical ratings
#' @param cat_ratings A cat_ratings object
#' @param group If TRUE, groups by rater_id to obtain hierarchical
#' parameters. Otherwise returns average a, p
#' @return Updated cat_ratings object
#' @export
e_m_step_cat <- function(cat_ratings, group) {

  K <- cat_ratings$K

  # E step: update subject truth estimates
  cat_ratings <- estimate_ti_cat(cat_ratings)

  # M step: join in the updated t vectors for each rating
  rating_params_cat <- cat_ratings$ratings |>
    left_join(cat_ratings$subjects, by = "subject_id")

  if (group) {
    # Per-rater M-step estimation
    cat_ratings$raters <- rating_params_cat |>
      group_by(rater_id) |>
      group_modify(~ estimate_a_p_cat(.x, K)) |>
      ungroup()
  } else {
    # Global/shared parameter estimate
    avg_rater <- estimate_a_p_cat(rating_params_cat, K)
    cat_ratings$raters[,2:3] <- avg_rater # fill in the values
  }

  # normalize p
  cat_ratings$raters$p <- lc_prob(cat_ratings$raters$p)

  return(cat_ratings)
}


#' Average params for a cat_ratings object
#' @param cat_ratings A cat_ratings object
#' @return a list with average t, a, p
#' @details This is a convenience function for collapsing the complexity of a
#' cat_ratings$subjects and cat_ratings$raters parameters into averages over
#' the three parameters t, a, and p, where t and p are K-vectors.
#' @export
avg_params_cat <- function(cat_ratings){
  t <- lc_vsum(cat_ratings$subjects$t) / nrow(cat_ratings$subjects)
  a <- mean(cat_ratings$raters$a)
  p <- lc_vsum(cat_ratings$raters$p) / nrow(cat_ratings$raters)
  return(list( t = t, a = a, p = p))
}

#' cat_ratings to rating_params
#' @param cat_ratings A cat_ratings object after parameter estimation
#' @return a data frame with ratings, ids, and parameters
#' @details This is a convenience function for joining the pieces of
#' information in cat_ratings into one data frame. The number of labels
#' must match the number of unique values of rating and will be matched
#' in ascending order of the ratings.
as_rating_params_cat <- function(cat_ratings){

  codes <- data.frame(rating = sort(unique(cat_ratings$ratings$rating)),
                      label = cat_ratings$labels)

  cat_ratings$ratings |>
    left_join(codes) |>
    left_join(cat_ratings$subjects, by = "subject_id") |>
    left_join(cat_ratings$raters, by = "rater_id")
}

#' Fit categorical t-a-p model (average parameters only)
#' @param cat_ratings A cat_ratings object (see generate_sample_ratings_cat()).
#' @param max_iter Maximum number of EM iterations (default 30).
#' @param tol Convergence tolerance on log-likelihood (default 1e-6).
#' @return A tibble with average accuracy a and guess distribution p.
#' @export
fit_counts_cat <- function(cat_ratings, max_iter = 20, tol = 1e-4) {

  # helper functions
  delta_param <- function(params1, params2){

    delta <- ( sum(abs(params1$t - params2$t)) +
                 sum(abs(params1$p - params2$p)) +
                 abs(params1$a - params2$a) ) / 7 # number of params

    return(delta)
  }


  delta1 <- 1
  params1 <- avg_params_cat(cat_ratings)


  for (iter in seq_len(max_iter)) {
    cat_ratings <- e_m_step_cat(cat_ratings, group = FALSE)

    params2 <- avg_params_cat(cat_ratings)
    delta2 <- delta_param(params1, params2)

    if(delta2 < tol) break
    if(delta2 > delta1) return(params1)

    params1 <- params2
    delta1 <- delta2

  }

  return(params2) # averaged parameters
}

#' Fit categorical t-a-p model (full hierarchical EM)
#' @param cat_ratings A cat_ratings object (see generate_sample_ratings_cat()).
#' @param max_iter Maximum number of EM iterations (default 50).
#' @param tol Convergence tolerance on log-likelihood (default 1e-6).
#' @return Updated cat_ratings object with fitted subject and rater parameters.
#' @export
fit_ratings_cat <- function(cat_ratings, max_iter = 20, tol = 1e-4) {

  # helper functions
  delta_param <- function(params1, params2){

    delta <- ( sum(abs(params1$t - params2$t)) +
                 sum(abs(params1$p - params2$p)) +
                 abs(params1$a - params2$a) ) / 7 # number of params

    return(delta)
  }


  delta1 <- 1
  params1 <- avg_params_cat(cat_ratings)

  cat_ratings_old <- cat_ratings

  for (iter in seq_len(max_iter)) {
    cat_ratings <- e_m_step_cat(cat_ratings, group = TRUE)

    params2 <- avg_params_cat(cat_ratings)
    delta2 <- delta_param(params1, params2)

    if(delta2 < tol) break
    if(delta2 > delta1) return(cat_ratings_old)

    params1 <- params2
    delta1 <- delta2
    cat_ratings_old <- cat_ratings

  }

  return(cat_ratings)
}

#' Turn ratings into cat_ratings with default parameters
#' @param ratings A ratings data frame, with subject_id, rating, rater_id
#' @param labels An optional K-vector of character strings
#' @param K An optional vector length; the number of categories
#' @return A cat_ratings object
#' @details Providing K may be necessary if there are missing values in the
#' ratings, so that some classes 1..K don't appear at all.
#' @export
as_cat_ratings <- function(ratings, labels = NULL, K = NULL){

  cat_ratings <- list(K = NULL,
                      labels = NULL,
                      ratings = NULL,
                      subjects = NULL,
                      raters = NULL)

  if(is.null(K)) {
    K <- n_distinct(ratings$rating)
  }

  cat_ratings$K <- K

  # check that ratings are between 1 and K
  if(min(ratings$rating) < 1) stop("Ratings must be between 1 and K")
  if(max(ratings$rating) > K) stop("Ratings must be between 1 and K")

  if(is.null(labels)) {
    cat_ratings$labels <- str_c("Class ", 1:K)
  } else {
    if(length(labels != K)) stop("Must provide on label for each rating type")
    cat_ratings$labels <- labels
  }

  cat_ratings$ratings <- ratings
  cat_ratings$subjects <- ratings |>
    distinct(subject_id) |>
    mutate(t = list(rep(1/K,K)))

  cat_ratings$raters <- ratings |>
    distinct(rater_id) |>
    mutate(a = .5,
           p = list(rep(1/K,K)))

  return(cat_ratings)

}

#' kits_per_rating_cat: Average entropy per rating (base 1/K)
#'
#' Computes the average normalized entropy ("kits") per rating for a
#' categorical ratings object, using a base of log(1/K). This generalizes
#' bits-per-rating from the binary case to the K-category case.
#'
#' @param cat_ratings A cat_ratings object.
#' @return A numeric scalar: average entropy per rating in the range [0, 1].
#' @details
#' - Returns 0 when ratings are deterministic.
#' - Returns 1 when ratings are maximally uncertain (uniform over K).
#' - Uses log-sum-exp internally for numerical stability.
#' @seealso \code{\link{bits_per_rating}}
#' @export

kits_per_rating_cat <- function(cat_ratings){

  # assemble rating params
  rating_params_cat <- as_rating_params_cat(cat_ratings)
  K <- cat_ratings$K
  N <- nrow(rating_params_cat)

  # compute the ll for each rating
  ll <- rating_params_cat |>
    mutate(

      a_bar_p = lc_subtract(lc_one(N, K), a) |>
        lc_mpy(p),  # (1-a)p

      ll = lc_indicator(rating, K) |>
        lc_mpy(a) |>
        lc_add(a_bar_p) |>
        lc_fn(log), # log(a + (1-a)p) in the rating position, else log((1-a)p)

      lt = lc_fn(t, log) #log(t)
    ) |>
    group_by(subject_id) |>
    summarize(
      lt       = list(first(lt)),
      ll_sum_j = list(lc_vsum(ll)) |> lc_add(lt),
      ll_sum_i = lc_logsumexp(ll_sum_j)
    ) |>
    summarize(total = sum(ll_sum_i)) |>
    pull(total)

  # convert to log 1/k and divide by ratings
  kpr <- -ll / log(K) / N

  return(kpr)

}


#' Fleiss' kappa from cat_ratings
#'
#' @description Compute Fleiss' kappa for categorical ratings (K >= 2)
#' @param cat_ratings A list object returned by generate_sample_ratings_cat()
#'   with elements ratings, subjects, raters, K, and labels
#' @return cat_ratings with updated t, a, p, ll, and degenerate flag
#' @export
fleiss_kappa_cat <- function(cat_ratings) {
  ratings <- cat_ratings$ratings
  K <- cat_ratings$K

  # counts per subject per category
  counts <- ratings |>
    dplyr::count(subject_id, rating, name = "n") |>
    tidyr::pivot_wider(
      names_from = rating,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(N_r = rowSums(dplyr::across(-subject_id)))

  category_cols <- setdiff(names(counts), c("subject_id", "N_r"))

  # observed agreement per subject
  counts <- counts |>
    dplyr::rowwise() |>
    dplyr::mutate(
      m = sum(dplyr::across(
        dplyr::all_of(category_cols),
        ~ .x * (.x - 1)
      )),
      P_i = m / (N_r * (N_r - 1))
    ) |>
    dplyr::ungroup()

  # average observed agreement
  P_bar <- mean(counts$P_i)

  # marginal category proportions across all subjects
  total_ratings <- sum(counts$N_r)
  p_vec <- colSums(counts[category_cols]) / total_ratings

  # expected agreement
  P_e_bar <- sum(p_vec^2)

  # Fleiss' kappa
  kappa <- (P_bar - P_e_bar) / (1 - P_e_bar)
  kappa <- ifelse(kappa < 0 | is.na(kappa) | is.nan(kappa), 0, kappa)

  # update params
  cat_ratings$subjects <- cat_ratings$subjects |>
    dplyr::mutate(t = list(p_vec))
  cat_ratings$raters <- cat_ratings$raters |>
    dplyr::mutate(a = sqrt(kappa), p = list(p_vec))

  cat_ratings$t <- p_vec
  cat_ratings$a <- sqrt(kappa)
  cat_ratings$p <- p_vec

  # log likelihood via ratings-level function
  ll <- kits_per_rating_cat(cat_ratings)
  degenerate <- is_degenerate(c(p_vec, sqrt(kappa), p_vec)) || kappa <= 0

  tibble::tibble(
    t = list(p_vec),
    a = a,
    p = list(p_vec),
    ll = ll,
    degenerate = degenerate
  )

}


