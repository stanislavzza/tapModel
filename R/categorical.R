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
#'  give names to the categorizes. For the sample ratings,
#'  values are provided for subject and rater parameters that match the
#'  generation process and nominal category labels (Class 1, Class 2, etc) are
#'  created.
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

  params$t <- params$t/sum(params$t)
  params$p <- params$p/sum(params$p)


  subject_params <- tibble(subject_id = 1:N_s,
                           T_i = sample(1:K,
                                        N_s,
                                        replace = TRUE,
                                        prob = params$t ))


  rater_params <- tibble(rater_id = 1:N_r)

  param_grid <- subject_params %>%
    cross_join(rater_params)

  n_ratings <- nrow(param_grid)

  # generate ratings based on t_i-a_j-p_i model
  param_grid <- param_grid %>%
    mutate(A_ij = as.integer(runif(n_ratings) < params$a ),
           P_ij = sample(1:K, n_ratings, replace = TRUE, prob = params$p ),
           C_ij = if_else(A_ij == 1,T_i,P_ij))

  if(details) {
    param_grid <- param_grid %>%
      select(subject_id, rating = C_ij, rater_id, T_i, A_ij, P_ij)
  } else {
    param_grid <- param_grid %>%
      select(subject_id, rating = C_ij, rater_id)
  }

  subjects <- subject_params |>
    mutate(t = list(params$t))

  if(details != TRUE) {
    subjects <- subjects |>
      select(-T_i)
  }

  raters <- param_grid |>
    distinct(rater_id) |>
    mutate(a = params$a,
           p = list(params$p))

  return(list(K = K,
              labels = paste("Class",1:K),
              ratings = param_grid,
              subjects = subjects,
              raters = raters))
}

#' Generate ratings from a cat_ratings object
#' @param cat_ratings a cat_ratings object with subjects, raters, and ratings data frames
#' @param use_avg_t If TRUE, sets each subject's t vector to the average (default)
#' @return A cat_ratings object with ratings generated from the t, a, p parameters
#' @details If use_avg_t is TRUE, then the t vector for each subject is set to the
#' average t vector over all subjects.
#' @export
generate_ti_aj_pj_ratings_cat <- function(cat_ratings, use_avg_t = TRUE){

  n_subjects <- nrow(cat_ratings$subjects)
  n_ratings  <- nrow(cat_ratings$ratings)

  # set the t_is to the average if requested
  if(use_avg_t == TRUE){
    cat_ratings$subjects <- cat_ratings$subjects |>
      mutate(t = t |> lc_vsum() |>
               lc_mpy(1/n_subjects) |>
               unlist() |> list())
  }

  # generate the t_i values, which are the same within a subject
  cat_ratings$subjects <- cat_ratings$subjects |>
    rowwise() |>
    mutate(T_i = list(rmultinom(1, 1, t))) |> # true class
    ungroup() |>
    select(subject_id, T_i)

  # convert to single data frame
  rating_params_cat <- cat_ratings |>
    as_rating_params_cat() |>
    select(subject_id, rater_id, T_i, a, p)


  rating_params_cat <- rating_params_cat %>%
    mutate(A_ij = bernoulli_trial(a)) |>
    rowwise() %>%
    mutate(P_ij = list(rmultinom(1, 1, p)),
           rating = which.max( unlist(T_i)*A_ij + (1-A_ij)*unlist(P_ij)) )|>
    select(subject_id, rating, rater_id, a, p)

  # replace the ratings in the original object
  cat_ratings$ratings <- rating_params_cat |>
    select(subject_id, rater_id, rating)

  return(cat_ratings)
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

  K <- cat_ratings$K

  # as a prior for t use the averages over all subjects
  lt <- cat_ratings$subjects |> # log t
    summarize(lt = list(lc_vsum(t)) |> lc_mpy(1/n()) |> lc_fn(log))

  # attach to ratings, adjusting for the assigned class
  # this generates a lot of warnings
  rating_log_pi <- cat_ratings$ratings |>
    left_join(cat_ratings$raters, by = "rater_id") |>

    # add rater's a to pi in the position of pi
    # that matches the rating = accurate ratings
    mutate(
      a_bar = lc_subtract(lc_one(n(), K), a),
      a_bar_p = lc_clone(p, rating) |> lc_mpy(a_bar),

      logpi = lc_indicator(rating, K) |>
        lc_mpy(a) |>
        lc_add(a_bar_p) |>
        lc_fn(log) # log(a + (1-a)p) in the rating position, else log((1-a)p)

    ) |>
    select(subject_id, logpi)

  subject_log_probs <- rating_log_pi |>
    group_by(subject_id) |>
    summarize(logprob = list(lc_vsum(logpi))) |>
    mutate(logprob = logprob |> lc_add(lt$lt))

  subject_probs <- subject_log_probs |>
    mutate( # log-sum-exp
      t = map(logprob, ~ {
        p_unnorm <- exp(.x - max(.x))  # stability trick
        p_unnorm / sum(p_unnorm)
      })
     # t = lc_logsumexp(t) # need to fix this
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
#' @export
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
  rating_params_cat <- cat_ratings |> as_rating_params_cat()

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
  #cat_ratings$raters$p <- lc_prob(cat_ratings$raters$p)

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

#' Compare two average parameter sets for abs difference
#' @param p1 Parameter set (t, a, p) with t and p k-vectors
#' @param p2 A second parameter set
#' @param tolerance What absolute difference to require?
#' @export
check_convergence_cat <- function(p1, p2, tolerance){
  t_okay <- if_else(mean(abs(p1$t - p2$t)) <= tolerance, 1, 0)
  a_okay <- if_else(abs(p1$a - p2$a) <= tolerance, 1, 0)
  p_okay <- if_else(mean(abs(p1$p - p2$p)) <= tolerance, 1, 0)

  if(t_okay & a_okay & p_okay) return(TRUE)
  return(FALSE)
}

#' cat_ratings to rating_params
#' @param cat_ratings A cat_ratings object after parameter estimation
#' @return a data frame with ratings, ids, and parameters
#' @details This is a convenience function for joining the pieces of
#' information in cat_ratings into one data frame. The number of labels
#' must match the number of unique values of rating and will be matched
#' in ascending order of the ratings.
#' @export
as_rating_params_cat <- function(cat_ratings){

  df <- cat_ratings$ratings |>
    left_join(cat_ratings$subjects, by = "subject_id") |>
    left_join(cat_ratings$raters, by = "rater_id")

  df$label <- cat_ratings$labels[df$rating]

  return(df)
}

#' Fit categorical t-a-p model (average parameters only)
#' @param cat_ratings A cat_ratings object (see generate_sample_ratings_cat()).
#' @param max_iter Maximum number of EM iterations (default 30).
#' @param tolerance Average convergence criterion. See `check_convergence_cat()`
#' @return A tibble with average accuracy a and guess distribution p.
#' @export
fit_counts_cat <- function(cat_ratings, max_iter = 20, tolerance = .01) {

  # get the average parameter values
  p1 <- cat_ratings |> avg_params_cat()

  for (iter in seq_len(max_iter)) {
    cat_ratings<- e_m_step_cat(cat_ratings, group = FALSE)
    p2 <- cat_ratings |> avg_params_cat()

    if(check_convergence_cat(p1, p2, tolerance)) {
      return(p2)
    }
    p1 <- p2
  }

  return(p2) # averaged parameters
}

#' Fit categorical t-a-p model (full hierarchical EM)
#' @param cat_ratings A cat_ratings object (see generate_sample_ratings_cat()).
#' @param max_iter Maximum number of EM iterations (default 50).
#' @param tolerance Average convergence criterion. See `check_convergence_cat()`
#' @return Updated cat_ratings object with fitted subject and rater parameters.
#' @export
fit_ratings_cat <- function(cat_ratings, max_iter = 20, tolerance = .01) {

  # get the average parameter values
  p1 <- cat_ratings |> avg_params_cat()

  for (iter in seq_len(max_iter)) {
    cat_ratings<- e_m_step_cat(cat_ratings, group = TRUE)
    p2 <- cat_ratings |> avg_params_cat()

    if(check_convergence_cat(p1, p2, tolerance)) {
      return(cat_ratings)
    }
    p1 <- p2
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
    if(length(labels) != K) stop("Must provide on label for each rating type")
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

#' krits_per_rating_cat: Average entropy per rating (base 1/K)
#'
#' Computes the average normalized entropy ("krit" = k-ary digit) per rating for a
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

krits_per_rating_cat <- function(cat_ratings){

  # assemble rating params
  rating_params_cat <- as_rating_params_cat(cat_ratings)
  K <- cat_ratings$K
  N <- nrow(rating_params_cat)


  # compute the ll for each rating
  ll <- rating_params_cat |>
    mutate(

      a_bar = lc_subtract(lc_one(N, K), a),
      a_bar_p = lc_clone(p, rating) |> lc_mpy(a_bar),

      ll = lc_indicator(rating, K) |>
        lc_mpy(a) |>
        lc_add(a_bar_p) |>
        lc_fn(log) # log(a + (1-a)p) in the rating position, else log((1-a)p)
    ) |>
    group_by(subject_id) |>
    summarize(
      lt       = list(first(t)) |> lc_fn(log), # log(t)
      ll_sum_k = list(lc_vsum(ll)) |> lc_add(lt), # scaled sum over each t_k
      ll_sum_i = lc_logsumexp(ll_sum_k) # completes inner sum-product in log form
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

  # log likelihood via ratings-level function
  ll <- krits_per_rating_cat(cat_ratings)
  degenerate <- is_degenerate(c(p_vec, sqrt(kappa), p_vec)) || kappa <= 0

  tibble::tibble(
    t = list(p_vec),
    a = sqrt(kappa),
    p = list(p_vec),
    ll = ll,
    degenerate = degenerate
  )

}

#' expected_krits_per_rating_cat: Average entropy per rating (base 1/K) from the model.
#'
#' @param params A list with t, a, and p
#' @param uniform_t If TRUE, t is set to a uniform distribution to eliminate
#' the effect of sampling from true classes
#' @return A numeric scalar: average entropy per rating in the range [0, 1].
#' @seealso \code{\link{bits_per_rating}}
#' @export

expected_krits_per_rating_cat <- function(params, uniform_t = FALSE){

  K <- length(params$t)

  xlogx <- function(x){
    if_else(x == 0, 0, -x*log(x)/log(K))
  }

  if(uniform_t == TRUE){
    t <- rep(1/K, K)
  } else {
    t <- params$t/sum(params$t)
  }

  a <- params$a
  a_ <- 1 - a
  p <- params$p/sum(params$p)

  ll_sum <- 0

  for(k in 1:K){
    for(j in 1:K) {
      if(k == j){
        ll <- xlogx(a + a_*p[j])
      } else {
        ll <- xlogx(a_*p[j])
      }
      ll_sum <- ll_sum + t[k]*ll
    }
  }

  return(ll_sum)

}

#' Rating Calibration Plot for categorical ratings
#' @description Compares predicted rating frequencies to observed frequencies
#' @param cat_ratings A cat_ratings object
#' @return A ggplot object with the calibration plot and error statistics
#' @details The title shows the average accuracy a,
#' the mean absolute error (MAE), the root mean squared error (RMSE),
#' and the average krits per rating (KPR). The plot compares modeled frequencies
#' to actuals for each rating category. The error bars show two standard errors.
#' @export
rating_calibration_cat <- function(cat_ratings){

  K <- cat_ratings$K
  N <- nrow(cat_ratings$ratings)

  rating_params_cat <- as_rating_params_cat(cat_ratings)

  # verify the input
  #verify_cat_ratings(cat_ratings)
  modeled <- rating_params_cat |>
    mutate(
      a_bar_p = lc_subtract(lc_one(n(), K), a) |> lc_mpy(p),
      a_t = t |> lc_mpy(a),
      prob = lc_add(a_t, a_bar_p) # a*t + (1-a)*p
    ) |>
    select(prob) |>
    pivot_longer_lc("prob") |>
    rename(modeled = prob_k, rating = k) |>
    group_by(rating) |>
    summarize(modeled = mean(modeled))

  observed <- rating_params_cat |>
    count(rating) |>
    mutate(observed = n/N,
           observed_se = sqrt( observed*(1-observed) / N),
           observed = if_else(n < 3, NA_real_, observed),
           observed_se = if_else(n < 3, NA_real_, observed_se))

  comparison <- modeled |>
    left_join(observed, by = "rating") |>
    rename( N = n)

  # average accuracy
  a_avg <- cat_ratings$raters |>
    summarize(a = mean(a)) |>
    pull(a)

  # find sum abs error
  MAE <- sum(abs(
    (comparison$modeled - comparison$observed) *
      comparison$N), na.rm = TRUE) / sum(comparison$N, na.rm = TRUE)

  RMSE <- sqrt(sum((
    (comparison$modeled - comparison$observed)^2 *comparison$N), na.rm = TRUE)
    / sum(comparison$N, na.rm = TRUE))

  KPR = krits_per_rating_cat(cat_ratings)

  my_title <- str_c("a = ", round(a_avg, 2),
                    " MAE=", round(MAE, 3),
                    "  RMSE=", round(RMSE, 3),
                    "  LL=", round(KPR, 3))

  # add average t, p values to the facets by modifying the rating
  t_avg <- cat_ratings$subjects |>
    pivot_longer_lc("t") |>
    group_by(k) |>
    summarize(t = mean(t_k)) |>
    rename(rating = k)

  p_avg <- cat_ratings$raters |>
    pivot_longer_lc("p") |>
    group_by(k) |>
    summarize(p = mean(p_k)) |>
    rename(rating = k)

  c_avg <- cat_ratings$ratings|>
    count(rating, name = "N") |>
    mutate(c = N/sum(N)) |>
    select(-N)

  pdf <- comparison |>
    left_join(c_avg, by = "rating") |>
    left_join(t_avg, by = "rating") |>
    left_join(p_avg, by = "rating")

  # Merge observed and simulated frequencies
  my_plot <- pdf |>
    ggplot(aes(x = modeled, y = observed,
               ymin = observed - 2*observed_se,
               ymax = observed + 2*observed_se,
               label = rating)) +
    geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
    geom_errorbar(width = 0, alpha = 0.5) +
    geom_text() +
    theme_bw()  +
    labs(title = my_title,
         x = "Modeled values",
         y = "Observed values")

  return(my_plot)

}


#' Subject Calibration Plot
#' @description For each subject compute the fraction of Class 1 ratings and
#' compare this to the the expected fraction from the t-a-p model parameters
#' @param rating_params A rating_params dataframe
#' @param n_bins Number of bins to use in the calibration plot. Defaults to 20.
#' @param n_sims Number of simulations to run to estimate the modeled
#' distribution from the probabilities. Defaults to 30.
#' @return A ggplot object with the calibration plot and error statistics
#' @details
#' The probabilities for each rating can be averaged to find the *expected* number
#' of Class 1 ratings per subject, but here we want a *distribution* of
#' that variation to use in the plot. That's what the n_sim parameter does. The
#' error bars are two standard errors of the proportion estimates for the observed
#' proportions, with any cases of N < 3 omitted.
#' @export
subject_calibration_cat <- function(cat_ratings, n_bins = 20, n_sims = 30){

  # verify the input
  #verify_ratings(rating_params)

  # simulate counts for each subject
  sim_counts_list <- vector("list", n_sims)

  for (i in seq_len(n_sims)) {
    sim_ratings <- generate_ti_aj_pj_ratings_cat(cat_ratings, use_avg_t = TRUE)
    sim_counts_list[[i]] <- sim_ratings$ratings |>
      count(subject_id, rating, name = "N_c") |>
      group_by(subject_id) |>
      mutate(N_r = sum(N_c)) |>
      ungroup() |>
      count(N_r, N_c, rating)


  }

  # Combine all into a single data frame
  sim_counts <- bind_rows(sim_counts_list)

  # Count up unique cases of N_r and N_c in the simulated data for each rating type
  sim_counts <- sim_counts |>
    filter(!is.na(N_r)) |>
    group_by(N_r, N_c, rating) |>
    summarize(modeled = sum(n)) |>
    ungroup()

  # Count up unique cases of N_r and N_c for the observed data for each rating type
  observed_counts <- cat_ratings$ratings |>
    count(subject_id, rating, name = "N_c") |>
    group_by(subject_id) |>
    mutate(N_r = sum(N_c)) |>
    ungroup() |>
    count(N_r, N_c, rating, name = "observed")

  # match up cases and insert zeros for missing data
  comparison <- sim_counts |>
    full_join(observed_counts) |>
    replace_na(list(observed = 0, modeled = 0)) |>
    mutate(c = round(N_c/N_r*n_bins)/n_bins) |>
    group_by(c, rating) |>
    summarize(n_observed = sum(observed),
              modeled = sum(modeled)) |>
    arrange(c) |>
    mutate(modeled = modeled/sum(modeled),
           observed = n_observed/sum(n_observed),
           SE = sqrt( observed*(1-observed) / sum(n_observed)),
           observed = if_else(n_observed <= 2, NA_real_, observed),
           SE = if_else(n_observed <= 2, NA_real_, SE))

  # find sum abs error
  MAE <- sum(abs(
    (comparison$modeled - comparison$observed) *
      (comparison$c - lag(comparison$c))), na.rm = TRUE)

  RMSE <- sqrt(sum((
    (comparison$modeled - comparison$observed)^2 *
      (comparison$c - lag(comparison$c))), na.rm = TRUE))

  KPR = krits_per_rating_cat(cat_ratings)

  a_avg <- cat_ratings$raters |>
    summarize(a = mean(a)) |>
    pull(a)

  my_title <- str_c("a = ", round(a_avg, 2),
                    " MAE=", round(MAE, 3),
                    "  RMSE=", round(RMSE, 3),
                    "  LL=", round(KPR, 3))

  # add average t, p values to the facets by modifying the rating
  t_avg <- cat_ratings$subjects |>
    pivot_longer_lc("t") |>
    group_by(k) |>
    summarize(t_avg = mean(t_k)) |>
    rename(rating = k)

  p_avg <- cat_ratings$raters |>
    pivot_longer_lc("p") |>
    group_by(k) |>
    summarize(p_avg = mean(p_k)) |>
    rename(rating = k)

  c_avg <- cat_ratings$ratings|>
    count(rating, name = "N") |>
    mutate(c_avg = N/sum(N)) |>
    select(-N)

  pdf <- comparison |>
    left_join(c_avg, by = "rating") |>
    left_join(t_avg, by = "rating") |>
    left_join(p_avg, by = "rating") |>
    mutate(rating = str_c(rating,
                          ": c=", round(c_avg,2),
                          " t=", round(t_avg,2),
                          " p=", round(p_avg,2)))

  # Merge observed and simulated frequencies
  my_plot <- pdf |>
    select(c, observed, modeled, rating, SE) |>
    gather(key = "type", value = "value", -c,-rating, -SE) |>
    ggplot(aes(x = c, y = value, color = type)) +
    geom_errorbar(aes(ymin = ifelse(type == "observed",
                                    value - 2*SE, NA),
                      ymax = ifelse(type == "observed",
                                    value + 2*SE, NA)),
                  width = 0.02, alpha = 0.5) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c("orange","steelblue")) +
    theme_bw()  +
    labs(title = my_title,
         x = "Fraction of Rating Type",
         y = "Frequency")  +
    facet_wrap(~ rating)

  return(my_plot)

}
