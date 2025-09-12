#' iterative attempt to fit model from counts
#' @description Fit the t-a-p model to counts. Start with the the middle of the
#' parameter space in looking #' for a solution. If it's degenerate it looks
#' around the edges of the space for a non-degenerate solution.
#' @param counts a data frame with columns N_r, N_c, and n,  representing number
#' of raters and counts of raters who rated class 1 with multiplicities n. This
#' can be created from `as_counts(ratings)`
#' @param method the method to use for optimization, defaults to "EM" for
#' expectation-maximization, or use "optim" for a direct optimization
#' @return a data frame with columns for parameters: t, a, p, and log likelihood
#' of the solution, and a flag for degeneracy
#' @details
#' The log likelihood returned is the expected value given the parameters, using
#' `expected_bits_per_rating()`. If you want the empirical value given the ratings
#' use `ratings |> as_rating_params(params) |> estimate_ti() |> bits_per_rating()`,
#' which upscales the parameters to a rating_params data frame with a_j = a and
#' p_j = p, and then estimates t_i from that before calculating log likelihood.
#' @export
fit_counts <- function(counts, method = "EM"){
  # start with the middle case
  params <- find_solution(counts, c(.5,.5,.5), method)

  if(is_degenerate(params)) {
    fence_params <- picket_fence(counts)[1:3]
    params <- find_solution(counts, fence_params)
  }

  # return the per-rating LL
  bpr <- bits_per_count(counts, params)

  return(data.frame( t = params[1],
                     a = params[2],
                     p = params[3],
                     ll = bpr,
                     degenerate = is_degenerate(params)))

}


#' t-a-p coefficients over an ordinal scale
#' @param ordinal_ratings a data frame in long format with columns subject_id and rating
#' where the ratings are for a single dimension, with multiple ordinal values,
#' for example water quality on a scale of 1 = poor, 2 = fair, 3 = good,
#' 4 = excellent. If the ratings are character strings, they will be sorted
#' alphabetically, so "11" would sort before "2." If the ratings are numeric,
#' they will be sorted numerically.
#' @return a data frame with columns for each cut point between ratings,
#' parameters: t, a, p, and ll for the t-a-p model and the Fleiss kappa, as
#' well as a column to flag degenerate cases, where one of the parameters is
#' zero or one.
#' @export
fit_ordinal_tap <- function(ordinal_ratings){
  rating_values <- sort(unique(ordinal_ratings$rating))
  n_values <- length(rating_values)

  output <- data.frame()
  for(i in 1:(n_values - 1)){
    lower_rating <- rating_values[i]
    upper_rating <- rating_values[i+1]

    ratings <- ordinal_ratings |>
      # convert to binary
      mutate(rating = as.integer(rating <= lower_rating))

    counts <- ratings |>
      as_counts()

    tap_params <- fit_counts(counts)

    tap_params <- tap_params |>
      mutate(CutPoint = str_c(lower_rating,"|", upper_rating),
             type = "t-a-p") |>
      relocate(type, CutPoint)

    fleiss_params <- fleiss_kappa(counts)

    fleiss_params <- fleiss_params |>
      mutate(CutPoint = str_c(lower_rating,"|", upper_rating),
             type = "Fleiss") |>
      relocate(type, CutPoint)

    output <- output |>
      rbind(tap_params, fleiss_params)

  }

  return(output)
}

#' Truth probabilities by Class 1 rate
#' @description Given a data frame of (N_r, N_c) pairs and t-a-p parameters
#' add a column to the dataframe with the probability of Class 1 for each
#' @param counts A data frame with columns N_r, and N_c. This might come from
#' `count_ratings()` with or without summarize = TRUE.
#' @param params A named list with t, a, and p estimates or a data frame with
#' a single row and columns named t, a, and, p. The function can also acommodate
#' parameter lists with a0,a1 and/or p0,p1 instead of a and p, respectively.
#' @return a data frame with the original columns plus columns C0 and C1
#' for the asymptotic rates of ratings of those classes, and a column  t_u for
#' the estimate of the true rate of Class 1 given counts.
#' @details
#' In the model, some ratings of Class 1 are generated from true Class 1 cases
#' and the rest from true Class 0 cases. Given the parametes we can find the
#' distributions of these two rating patterns, from which we can estimate the
#' fraction of each rating sum (e.g. 3 of 5 raters assigning class 1) that is
#' due to true Class 1 cases. We use that as an estimate for the probability of
#' the true class being 1.
#'
#' @export
estimate_tu <- function(counts, params){
  # was prob_t
  t <- params$t
  # check the format of params

  a <- params$a
  p <- params$p

  pr_c0 <- (1-a)*p
  pr_c1 <- a + (1-a)*p

  # conceptually we're doing this:
  #distr <- counts |>
  #  rowwise() |>
  #  mutate(C0 = (1-t)*dbinom(N_c, N_r, pr_c0),
  #         C1 = t*dbinom(N_c, N_r, pr_c1),
  #         t_u = C1/(C0 + C1)) |>  # estimate Pr[Class = 1|N_r, N_c, t, a, p]
  #  ungroup()

  # but it's numerically better to skip the binomial coefs
  distr <- counts |>
    rowwise() |>
    mutate(C0 = log(1-t) + N_c*log(pr_c0) + (N_r - N_c)*log(1-pr_c0), #dbinom(N_c, N_r, pr_c0),
           C1 = log(t) + N_c*log(pr_c1) + (N_r - N_c)*log(1-pr_c1), #dbinom(N_c, N_r, pr_c1),
           log_t_u = C1 - LSE_R(c(C0, C1)),
           t_u = exp(log_t_u)) |>  # estimate Pr[Class = 1|N_r, N_c, t, a, p]
    ungroup()


  return(distr)

}

#' Truth probabilities by Class 1 rate for each subject
#' @description Given a rating_params data frame, which contains the t, a, and p
#' parameters for each rating, as well as the rating, recalculate the t_i
#' for each subject using the general (multinomial) calculation.
#' @param rating_params A data frame from `fit_ratings()` or `fit_ratings_mcmc()`
#' @return a data frame with subject_id, t_i, C0, and C1 columns. The latter two
#' are the probability estimates of the classes for each subject, where
#' t_i = Pr(Class 1) = C1/(C0 + C1).
#' @details
#' In the model, some ratings of Class 1 are generated from true Class 1 cases
#' and the rest from true Class 0 cases. The derivation mimics that found
#' in the MCMC model t_i-a_j-p_j.stan. The function `estimate_tu()` does the
#' same thing for the counts of ratings, where we assume fixed values for
#' each of the parameters (fully pooled model).
#'
#' @export
estimate_ti <- function(rating_params){

    eps <- 1e-5

    # create subject log likelihood sums
    subject_prob <- rating_params |>
      # create the pi statistics. See the hierarchical chapter for details.
      # this follows Dawid & Skene (1979) notation.
      mutate(lpi_00 = log(1 - (1-a)*p + eps),
             lpi_01 = log((1-a)*p+ eps),
             lpi_10 = log((1-a)*(1-p)+ eps),
             lpi_11 = log(a + (1-a)*p)+ eps) |>
      # now do the subject aggregation
      group_by(subject_id) |>
      summarize(#t = first(t), # same value t_i for single subject
                C0 = exp(sum( (rating == 0)*lpi_00 + (rating == 1)*lpi_01)),
                C1 = exp(sum( (rating == 0)*lpi_10 + (rating == 1)*lpi_11))) |>
      mutate(t = C1/(C0 + C1)) |>
      select(subject_id, t)

    rating_params <- rating_params |>
      select(-t) |>
      left_join(subject_prob, by = "subject_id") |>
      select(subject_id, rating, rater_id, t, a, p)

    return(rating_params)
}

#' generate rater parameters
#' @param rating_params A rating_params dataframe
#' rating, and rater_id (a `ratings` data frame). See the `as_binary_ratings()` function.
#' @return a rating_params data frame with one row per rater giving t,a,p statistics for each
#' @export
estimate_aj_pj <- function(rating_params){
  # was rater_stats
  #

  # generate the individual rater parameters. The formulas come from the
  # confusion matrix in chapter 1, table 9. Here, tpr = true positive
  # rate, top left in the matrix. The idea is to use the information
  # to solve for a_j and p_j, where the truth class 1 probability t_i
  # has been induced from the three parameter estimate. I use primes
  # instead of bars for the complement for ease of notation, e.g.
  # t' = 1-t, a' = 1-a, p' = 1-p.


  rater_stats <- rating_params |>
    group_by(rater_id) |>
    summarize(avg_t = mean(t), # avg over ratings
              #  c1 = mean(rating),
              tpr  = mean(t*rating),         # ta + ta'p
              #  tnr  = mean((1-t)*(1-rating)), # t'a + t'a'p'
              fpr  = mean((1-t)*rating),     # t'a'p
              # fnr  = mean(t*(1-rating)),     # ta'p'
              a   = tpr/avg_t - fpr/(1-avg_t), # solve for a
              p   = fpr/(1-avg_t)/(1-a)) |>
    # make sure params are in [0,1]
    mutate(a = if_else(a < 0, 0, if_else(a > 1, 1, a)),
           p = if_else(p < 0, 0, if_else(p > 1, 1, p)))

  rating_params <- rating_params |>
    select(-a, -p) |>
    left_join(rater_stats, by = "rater_id") |>
    select(subject_id, rating, rater_id, t, a, p)

  return(rating_params)

}


#' log likelihood for each subject
#' @description Given a rating_param data frame from `fit_ratings` or `fit_ratings_mcmc`,
#' return the log likelihood for each subject using a t_i-a_j-p_j model.
#' @param rating_params A data frame with binary subject_id, rating, and a
#' t, a, and p parameter for the rating.
#' @return The rating_params data frame with column ll = log likelihood added.
#' The log base is e, so if you want bits divide by -log(2)
#' @details There are several functions that return bits per rating. Here's a
#' guide. `bits_per_rating(rating_params)` is the hierarchical version, using
#' the fitted model from `fit_ratings()` or `fit_ratings_mcmc()`. For the average
#' three-parameter t-a-p model use `bits_per_count(counts, params)`, which is
#' the same as upscaling the counts to rating_params and then using `bits_per_rating()`,
#' e.g. `counts |> as_rating_params(params) |> bits_per_rating()`, but it's a little
#' faster to do it directly. There's also a calculation for the expected value
#' of the bits per rating for a parameter set, using `params |> expected_bits_per_rating()`.
#' The `bits_per_rating()` function uses an intermediate function `ll_per_subject(rating_params)`,
#' which gives a subject-specific result in log likelihood, base e. This can be
#' useful for other purposes. Finally, there is a `rater_entropy(rating_params)`
#' function that returns the bits per rating with t set to .5, to account
#' only for rater characteristics.
#' @export
ll_per_subject <- function(rating_params){

  eps <- 1e-5

  N <- nrow(rating_params)

  ll_stats <- rating_params |>
    mutate(  lpi_00 = log(a+(1-a)*(1-p) + eps),
             lpi_10 = log((1-a)*(1-p) + eps),
             lpi_01 = log((1-a)*p + eps),
             lpi_11 = log(a + (1-a)*p+ eps) ) |>
    group_by(subject_id) |>
    summarize(sub_00 = sum((rating == 0)*lpi_00),
              sub_10 = sum((rating == 0)*lpi_10),
              sub_01 = sum((rating == 1)*lpi_01),
              sub_11 = sum((rating == 1)*lpi_11),
              lt = log(first(t) + eps),
              ltbar = log(1 - first(t) + eps)) |>
    rowwise() |>
    mutate(l1 = lt + sub_11 + sub_10,
           l0 = ltbar + sub_01 + sub_00,
           ll= LSE_R(c(l1,l0))) |>
    ungroup()

  return(ll_stats)
}

#' log likelihood by rating
#' @description Given a rating_param data frame from `fit_ratings` or `fit_ratings_mcmc`,
#' return the average bits per rating of log likelihood using a t_i-a_j-p_j model.
#' @param rating_params A data frame with binary subject_id, rating, and a
#' t, a, and p parameter for the rating.
#' @details There are several functions that return bits per rating. Here's a
#' guide. `bits_per_rating(rating_params)` is the hierarchical version, using
#' the fitted model from `fit_ratings()` or `fit_ratings_mcmc()`. For the average
#' three-parameter t-a-p model use `bits_per_count(counts, params)`, which is
#' the same as upscaling the counts to rating_params and then using `bits_per_rating()`,
#' e.g. `counts |> as_rating_params(params) |> bits_per_rating()`, but it's a little
#' faster to do it directly. There's also a calculation for the expected value
#' of the bits per rating for a parameter set, using `params |> expected_bits_per_rating()`.
#' The `bits_per_rating()` function uses an intermediate function `ll_per_subject(rating_params)`,
#' which gives a subject-specific result in log likelihood, base e. This can be
#' useful for other purposes. Finally, there is a `rater_entropy(rating_params)`
#' function that returns the bits per rating with t set to .5, to account
#' only for rater characteristics.
#' @return The average bits per rating of log likelihood
#' @export
bits_per_rating <- function(rating_params){

  N <- nrow(rating_params)

  avg_bits <- ll_per_subject(rating_params)  |>
    summarize(-sum(ll)/N/log(2)) |>
    pull()

  return(avg_bits)
}

#' log likelihood by rating
#' @description Given `counts` from `ratings |> as_counts()` and `params` from
#' `counts |> fit_counts()`, returns the average bits per rating.
#' return the average bits per rating of log likelihood using a t_i-a_j-p_j model.
#' @param rating_params A data frame with binary subject_id, rating, and a
#' t, a, and p parameter for the rating.
#' @return The average bits per rating of log likelihood
#' @details There are several functions that return bits per rating. Here's a
#' guide. `bits_per_rating(rating_params)` is the hierarchical version, using
#' the fitted model from `fit_ratings()` or `fit_ratings_mcmc()`. For the average
#' three-parameter t-a-p model use `bits_per_count(counts, params)`, which is
#' the same as upscaling the counts to rating_params and then using `bits_per_rating()`,
#' e.g. `counts |> as_rating_params(params) |> bits_per_rating()`, but it's a little
#' faster to do it directly. There's also a calculation for the expected value
#' of the bits per rating for a parameter set, using `params |> expected_bits_per_rating()`.
#' The `bits_per_rating()` function uses an intermediate function `ll_per_subject(rating_params)`,
#' which gives a subject-specific result in log likelihood, base e. This can be
#' useful for other purposes. Finally, there is a `rater_entropy(rating_params)`
#' function that returns the bits per rating with t set to .5, to account
#' only for rater characteristics.
#' @export
bits_per_count <- function(counts, params) {
  verify_counts(counts)
  verify_params(params)
  eps <- 1e-15

  N <- sum(counts$n*counts$N_r)

  t <- params$t
  a <- params$a
  p <- params$p

  counts |>
    mutate(  lpi_00 = log(a+(1-a)*(1-p) + eps),
             lpi_10 = log((1-a)*(1-p) + eps),
             lpi_01 = log((1-a)*p + eps),
             lpi_11 = log(a + (1-a)*p+ eps),
             sub_00 = (N_r - N_c)*lpi_00,
             sub_10 = (N_r - N_c)*lpi_10,
             sub_01 = N_c*lpi_01,
             sub_11 = N_c*lpi_11) |>
    estimate_tu(params) |>
    rowwise() |>
    mutate(l1 = log_t_u + sub_11 + sub_10,
           l0 = log(1 - t_u + eps) + sub_01 + sub_00,
           ll= LSE_R(c(l1,l0))) |>
    ungroup() |>
    summarize( -sum(ll*n)/N/log(2)) |>
    pull()

}

#' expected bits per rating from avg params
#' @param params A params vector with just t, a, and p
#' @return a scalar, the log likelihood in bits
#' @details There are several functions that return bits per rating. Here's a
#' guide. `bits_per_rating(rating_params)` is the hierarchical version, using
#' the fitted model from `fit_ratings()` or `fit_ratings_mcmc()`. For the average
#' three-parameter t-a-p model use `bits_per_count(counts, params)`, which is
#' the same as upscaling the counts to rating_params and then using `bits_per_rating()`,
#' e.g. `counts |> as_rating_params(params) |> bits_per_rating()`, but it's a little
#' faster to do it directly. There's also a calculation for the expected value
#' of the bits per rating for a parameter set, using `params |> expected_bits_per_rating()`.
#' The `bits_per_rating()` function uses an intermediate function `ll_per_subject(rating_params)`,
#' which gives a subject-specific result in log likelihood, base e. This can be
#' useful for other purposes. Finally, there is a `rater_entropy(rating_params)`
#' function that returns the bits per rating with t set to .5, to account
#' only for rater characteristics.
#' @export
expected_bits_per_rating <- function(params){
  verify_params(params)

  xlogx <- function(x){
    if_else(x == 0, 0, -x*log(x)/log(2))
  }

  t = params$t
  a = params$a
  p = params$p
  t_ = 1-t
  a_ = 1-a
  p_ = 1-p

  return(t*( xlogx(a_*p_) + xlogx(a+a_*p) ) +
         t_*( xlogx(a + a_*p_) + xlogx(a_*p)))
}

#' fit ratings to generate t_i, a_j, p_j for each rating
#' @param ratings a data frame with columns subject_id, rating, and rater_id
#' @param params optional average t-a-p parameters to use for the fit, or (if NULL)
#' automatically generated
#' @param max_iterations The maximum number of iterations to run, defaults to 10
#' @param ll_threshold The threshold for log likelihood convergence, defaults to .01
#' @param verbose report out iterations?
#' @return A data frame with the original ratings data augmented with
#' parameters for each subject, rater, and rating
#' @export
fit_ratings <- function(ratings, params = NULL,
                        max_iterations = 10,
                        ll_threshold = 0.01,
                        verbose = FALSE) {

  verify_ratings(ratings)

  # Initialize parameters
  if (is.null(params)) {
    params <- ratings |>
      tapModel::as_counts() |>
      tapModel::fit_counts()
  }

  rating_params <- ratings |>
    tapModel::as_rating_params(params)

  ll_old <- bits_per_rating(rating_params)

  for (i in seq_len(max_iterations)) {
    # E-step and M-step
    rating_params <- estimate_ti(rating_params)
    rating_params <- estimate_aj_pj(rating_params)

    ll_new <- bits_per_rating(rating_params)

    if (!is.finite(ll_new) || is.na(ll_new)) {
      warning("Non-finite or missing log-likelihood encountered. Stopping.")
      break
    }

    if (ll_new > ll_old) {
      warning(sprintf("Bits per rating increased from %.4f to %.4f; stopping.", ll_old, ll_new))
      break
    }

    if (verbose) {
      message(sprintf("Iter %d: LL = %.4f", i, ll_new))
    }

    if (abs(ll_new - ll_old) < ll_threshold) {
      if (verbose) message("Converged.")
      break
    }

    ll_old <- ll_new
  }

  return(rating_params)
}


#' Add log likelihood columns to a rating_params data frame
#' @param rating_params A data frame with columns subject_id, rating, rater_id,
#' and at least parameters t, a, and p for each row
#' @return The original data frame with columns C0 and C1 added
#' @details This function assumes the (t_i)-a_j-p_j model, whereby raters can
#' have different opinions about the same subject's probability of Class 1.
#' DEPRECATED
add_rating_param_bits <- function(rating_params){

  my_params <- rating_params |>
    summarize(t = mean(t), a = mean(a), p = mean(p))

  rating_param_bits  <- tapModel::as_count_index(rating_params) |>
    left_join(tapModel::estimate_tu(tapModel::as_counts(rating_params), my_params), by = join_by(N_r, N_c)) |>
    select(subject_id, t  = t_u) |>
    left_join(rating_params |> select(-t), by = "subject_id") |>
    mutate(C1 = -log2(t*a + (1-a)*p),     # log(Pr[Class 1 rating|params])
           C0 = -log2( 1- t*a - (1-a)*p))

}

#' pull the t-a-p parameters from a rating_params data frame
#' @param rating_params a data frame with columns subject_id, rating, rater_id,
#' and at least parameters t, a, and p for each row
#' @description
#' This function is useful for taking the parameters from a fitted model and
#' then simulating a rating set from it using `generate_ti_aj_pj_ratings()`
#' @return a list with two data frames, one for subject t_i, and another with
#' rater parameters a_j, and p_j
#' @export
pull_rating_params <- function(rating_params){

  subjects <- rating_params |>
    select(subject_id, t) |>
    distinct() |>
    arrange(subject_id)

  raters <- rating_params |>
    select(rater_id, a, p) |>
    distinct(rater_id, .keep_all = TRUE) |>
    arrange(rater_id)

  return(list(subjects = subjects, raters = raters))
}


######################### Exact accuracy calculations #########

#' get count probabilities (private)
#' @description
#' Given the number of raters and t,a,p parameters, generate the probability
#' distribution for the number of counts of Class 1 ratings per subject.
#' @param N_r the number of raters
#' @param t the true rate of Class 1
#' @param a rater accuracy
#' @param p random assigment rate for inaccurate ratings
#' @return a vector of probabilities for each count n0, n1, ... that has
#' been reflected and summed to account for the symmetry of the distribution
#' @details The functions that start with "exact_" are used for closed-form
#' calculation of accuracy.
exact_count_probabilites <- function(N_r,t,a,p){
  c0_probs <- dbinom(0:N_r, N_r, prob = (1-a)*p)
  c1_probs <- dbinom(0:N_r, N_r, prob = a + (1-a)*p)
  (1-t)*c0_probs + t*c1_probs
}

#' Apply symmetry to halve the length of count probabilities (private)
#' @param count_prob a vector of count probabilities from `exact_count_probabilites()`
#' @return a vector of count probabilities with the second half reflected and
#' summed with the first half, accounting for even/odd cases.
#' @details This function is not exported, since it's only used with
#' `exact_count_probabilites()` and `exact_count_range()`. The functions that
#' start with "exact_" are used for closed-form calculation of accuracy.
exact_combine_counts <- function(count_prob){
  l = length(count_prob)
  m = l/2 + .5 # middle element
  x <- count_prob + rev(count_prob)

  if(l %% 2 == 0){  # even length
    x <- x[1:(l/2)]
  } else { # odd length
    x[m] <- x[m]/2 # the middle one doesn't get doubled
    x <- x[1:m]
  }
  return(x)
}

#' Generate inputs for the regression to find coefficients for closed-form (private)
#' @description
#' Generates a sequence of accuracy values from 0 to 1 in increments of .001 and
#' associated exact distributions of the count probabilities from the t-a-p
#' model with the provided number of raters N_r, t, and p.
#' @param N_r the number of raters
#' @param t the true rate of Class 1
#' @param p random assigment rate for inaccurate ratings
#' @return a list with two elements: a vector of accuracy values, which is the
#' dependent variable for the regression, and the matrix of predictors comprising
#' the exact count probabilities for each accuracy value.
#' @details The functions that start with "exact_" are used for closed-form
#' calculation of accuracy.
exact_count_range <- function(N_r,t,p){

  l = floor( (N_r + 1)/ 2 + .5) # length of condensed counts

  a_seq = seq(.2, .9, .001) # y vector for regression
  X = matrix(nrow = length(a_seq), ncol = l) # X matrix of inputs

  for(i in 1:length(a_seq))
    X[i,] <-  exact_count_probabilites(N_r,t,a_seq[i],p)

  return(list(a = a_seq, X = X))
}

#' Fill in zeros for missing ratings counts (private)
#' @description
#' To use the coefficients generated from `exact_accuracy_coefs()` we need a
#' complete set of count proportions. For real data sets, especially with
#' larger numbers of raters, we might have counts that don't appear in the results.
#' This function fills in those blanks by inserting zeros.
#' @param counts A data frame resulting from `as_counts(ratings)`
#' @return A data frame with all possible counts from 0 to N_r, with zeros added
#' where necessary.
exact_fill_missing_vals <- function(counts){
  N_r = first(counts$N_r)

  missing <- base::setdiff(0:N_r,counts$N_c)
  if(length(missing) == 0) return(counts)
  bind_rows(counts, tibble(N_r = N_r, N_c = missing, n = 0)) |>
    arrange(N_c)
}

#' Estimate the coefficients for the closed-form accuracy model
#' @description
#' Given the number of raters N_r, the true rate of Class 1 t, and the random
#' assignment rate p, estimate the coefficients for the closed-form model of
#' accuracy as a function of the count proportions observed in data
#' @param N_r the number of raters (a constant)
#' @param tp the t and p parameters, which are assumed equal (unbiased raters)
#' @return a data frame with the coefficients for the closed-form model of
#' accuracy as a function of the count proportions observed in data
#' @details The functions that start with "exact_" are used for closed-form
#' calculation of accuracy. You'll get a warning if there are more than
#' thirty raters, because colinearity makes the regression unstable above that
#' point.
#' @export
exact_accuracy_coefs <- function(N_r,tp){
    k <- 0:N_r
    # beta <- ( k^2 - N_r^2*tp + N_r*(N_r-1)*tp*(1-tp) )/ ( N_r*(N_r-1)*(1-tp)*tp )
    beta <- ( k^2 ) / ( N_r*(N_r-1)*(1-tp)*tp ) - N_r/((N_r-1)*(1-tp)) + 1
    return(beta)
}

#' Estimate the accuracy from the count proportions and t, p parameters
#' @description
#' Given counts of class 1 ratings and the t, p parameters, estimate the accuracy
#' using the closed form. You can either provide the closed-form coefficients or
#' have it calculate them.
#' @param counts A data frame with columns N_r, N_c, and n, representing the number
#' of class 1 ratings. This can be created from `as_counts(ratings)`. The number
#' of raters N_r must be the same for all rows.
#' @param tp the t and p parameters, which are assumed equal (unbiased raters).
#' This parameter is only required if count_coefs is NULL, since it is used
#' to calculate those coefficients with `exact_accuracy_coefs()`. It defaults to
#' .5.
#' @return the accuracy parameter as a scalar
#' @details Either tp or count_coefs must be provided. These correspond to the
#' choices to have this function calculate the coefficients for you, in which
#' case tp must be specified, or provide the pre-calculated coefficients, e.g. if
#' you're doing a lot of calculations with them in a simulation. The functions
#' that start with "exact_" are used for closed-form
#' calculation of accuracy. The use_fleiss = FALSE case will attempt to use the
#' exact coefficients, but this is numerically unstable. It's mostly here for
#' theory development. See the code in the Appendix for a good use for this.
#' @export
exact_accuracy <- function(counts, tp = .5){

  # are all the N_r the same?
  if(any(counts$N_r != first(counts$N_r))) stop("All N_r must be the same")

  count_coefs <- exact_accuracy_coefs(counts$N_r, tp)

  count_probs <- counts |>
    mutate(n = n/sum(n)) |>
    exact_fill_missing_vals()

  a_squared <- sum(count_probs*count_coefs)

  if(a_squared <= 0) a = 0 else a = sqrt(a_squared)

  return(a)
}



