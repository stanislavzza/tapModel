#' sum component-wise over a given list-vector
#' @param df a data frame with list-vector `col`
#' @param col the name of the column, e.g. t, p, logpi
#' @return A single vector that is the componenent-wise (vertical)
#' sum of the components, collapsing a column of K-vectors into a single
#' K-vector
#' @export
sum_list_column_vectors <- function(group_df, col) {
  # Use tidy evaluation to get the column
  col <- rlang::ensym(col)

  # find the vector length
  first_vec <- unlist(group_df |> dplyr::pull(!!col) %>% .[[1]])
  K <- length(first_vec)

  # Initialize total vector to zeros
  total <- numeric(K)

  # Iterate through each row and accumulate
  for (i in seq_len(nrow(group_df))) {
    vec <- unlist(group_df[[rlang::as_string(col)]][[i]])
    if (length(vec) != K) stop("Vector length mismatch in column ", rlang::as_string(col))
    total <- total + vec
  }

  return(total)
}

#' Keep parameters within bounds
#' @param df A data frame with a list-col
#' @param col The name of the column to be checked. It should be
#' K-vectors wrapped in lists.
#' @return The list column is nudged up to 0 or down to 1 as needed and
#' then renormed to sum to 1
#' @export
clamp_and_normalize_list_column <- function(df, col, eps = 1e-12) {
  col <- rlang::ensym(col)

  df |> mutate(
    !!col := map(!!col, ~ {
      vec <- pmin(1, pmax(0, .x))       # clamp to [0, 1]
      sum_vec <- sum(vec)
      if (sum_vec < eps) {
        vec[] <- 1 / length(vec)        # fallback: uniform vector
      } else {
        vec <- vec / sum_vec            # normalize
      }
      vec
    })
  )
}

#' get ratings by sampling a known distribution
#' @param N_s number of subjects
#' @param N_r number of raters per subject
#' @param K the number of categories, labeled 1 to K
#' @param params A list with t, a, p. t and p are non-negative vectors of length K, which
#' will be normalized to make probability distributions
#' @param details logical defaulting to FALSE. If TRUE, return the full set
#' of parameters to include the T_i, P_ij, and A_ij values.
#' @return A list that comprises the cat_ratings type: data frames ratings, subjects, and raters,
#'  an integer K > 1 with the number of categories, and a vector labels that
#'  give names to the categorizes. For the sample ratings, nominal
#'  values are provided for subject and rater parameters (.5 each) and
#'  the categories (Class 1, Class 2, etc)
#' @export
generate_sample_ratings_cat <- function(N_s = 100, N_r = 5,
                                        K = 3,
                                        params = list(t = rep(.5,3),
                                                      a = .7,
                                                      p = rep(.5,3)),
                                        details = FALSE) {

  if(K < 2) stop("K must be at least 2")

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
    group_modify(~ {
      tibble(logprob = list(sum_list_column_vectors(.x, col = logpi)))
    })

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
#' @details
#' This function is designed to be used inside data
#' grouped by rater if desired.
#' @return The estimated C matrix
estimate_C <- function(rating_params_cat, K) {

  M <- matrix(0, nrow = K, ncol = K)

  for (i in seq_len(nrow(rating_params_cat))) {
    t_i <- unlist(rating_params_cat$t[[i]])
    r <- rating_params_cat$rating[i]
    if (!is.na(r) && r >= 1 && r <= K) {
      M[, r] <- M[, r] + t_i  # t_i contributes to column r
    }
  }

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
  t <- sum_list_column_vectors(rating_params_cat, "t") / nrow(rating_params_cat)

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
#' @param cat_ratings a cat_ratings object. See `generate_sample_ratings_cat()`
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
  cat_ratings$raters <- clamp_and_normalize_list_column(cat_ratings$raters, "p")

  return(cat_ratings)
}

#' Fit categorical t-a-p model (average parameters only)
#' @param cat_ratings A cat_ratings object (see generate_sample_ratings_cat()).
#' @param max_iter Maximum number of EM iterations (default 30).
#' @param tol Convergence tolerance on log-likelihood (default 1e-6).
#' @return A tibble with average accuracy a and guess distribution p.
#' @export
fit_counts_cat <- function(cat_ratings, max_iter = 30, tol = 1e-4) {

  # helper functions
  delta_param <- function(params1, params2){

    delta <- ( sum(abs(params1$t - params2$t)) +
                 sum(abs(params1$p - params2$p)) +
                 abs(params1$a - params2$a) ) / 7 # number of params

    return(delta)
  }

  param_list <- function(cat_ratings){
    t <- sum_list_column_vectors(cat_ratings$subjects, "t") / nrow(cat_ratings$subjects)
    a <- cat_ratings$raters$a[1]
    p <- unlist(cat_ratings$raters$p[1])
    return(list( t = t, a = a, p = p))
  }

  delta1 <- 1
  params1 <- param_list(cat_ratings)


  for (iter in seq_len(max_iter)) {
    cat_ratings <- e_m_step_cat(cat_ratings, group = FALSE)

    params2 <- param_list(cat_ratings)
    delta2 <- delta_param(params1, params2)

    if(delta2 < tol) break
    if(delta2 > delta1) return(params1)

    params1 <- params2
    delta1 <- delta2

  }

  return(params1) # averaged parameters
}
