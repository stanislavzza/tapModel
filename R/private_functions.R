#' Fit the t-a-p model to binary ratings
#' @param counts a summary data frame with columns N_r, N_c, representing replications
#' of unique pairs of (N_r, N_c). This is the output of `count_ratings` with
#' `summarize = TRUE`
#' raters and counts of raters who rated class 1.
#' @param init_params initial values for t, a, and p
#' @param method optimization method, defaults to "EM" for expectation maximization,
#' or "optim" for a more general optimization method using R's method
#' @param max_steps maximum number of iterations for the EM optimization, defaults to 100
#' @param tolerance convergence tolerance for the EM optimization, defaults to 1e-4
#' @return a data frame with columns for parameters: t, a, p, and log likelihood of the solution
find_solution <- function(counts, init_params, method = "EM",  max_steps = 100, tolerance = 1e-4){

  if(method == "EM"){
    return(find_solution_em(counts, max_steps, tolerance))
  }

  params <- optim(par = init_params,
                  fn = log_likelihood,
                  counts = counts,
                  lower = c(0,0,0),
                  upper = c(1,1,1),
                  method = "L-BFGS-B")$par

  return(c( params, log_likelihood(params, counts)))
}

# non-exported functions
#' Ensure proper [0,1] domain for a vector
#' @param x numerical vector
#' @return numerical vector with values in [0,1], rounded where necessary
#'
fix_domain <- function(x){
  x[x < 0] <- 0
  x[x > 1] <- 1
  return(x)
}

#' negative log likelihood for binary ratings using t-a-p
#' @description Use the implied binomial mixture to calculate the negative log likelihood. It's
#' negated because the optimizer minimizes rather than maximizes.
#' @param params parameters t, a, and p in a vector
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters and counts of raters who rated class 1.
#' @return the log likelihood of the t-a-p model, or 1e6 if the parameters are out of range
log_likelihood <- function(params, counts){
  t <- params[1]
  a <- params[2]
  p <- params[3]

  counts  %>%
    rowwise() %>% # because log_prob isn't vectorized
    # note that the optimizer MINIMIZES
    summarize(ll = n*negative_log_prob(N_r, N_c, t, a, p)) %>%
    mutate(ll = if_else(is.infinite(ll), 1e6, ll)) %>%
    pull() |>
    sum()
}

#' Negative log likelihood of binomial mixture
#' @description given, t-a-p parameters, return -LL for the implied binomial mixture. It's
#' negated because the optimizer minimizes rather than maximizes. This function works on
#' scalars because of \code{dbinom}, and is used by the \code{\link{log_likelihood}} function.
#' @param N_r number of raters as a scalar
#' @param N_c number of raters who rated T_i = 1 as a scalar
#' @param t  mixture parameter in the t-a-p model
#' @param a  accuracy in the t-a-p model
#' @param p  guess distribution in the t-a-p model
#' @return negative log likelihood of the binomial mixture as a scalar
negative_log_prob <- function(N_r, N_c, t, a, p){

  t <- fix_domain(t)
  a <- fix_domain(a)
  p <- fix_domain(p)

  prob_true <- dbinom(N_c, N_r, prob = a + (1-a)*p)
  prob_false <- dbinom(N_c, N_r, prob = (1-a)*p) # T = 0 case

  # despite the limits, the optimizer sometimes submits out-of-range parameters
  if(sum( is.nan(prob_true) + is.nan(prob_false)) > 0) return(1e6)

  # scale by t and (1-t) to get the mixture
  prob <- t*prob_true + (1-t)*prob_false
  prob[prob == 0] <- .00001

  return(-sum(log(prob), na.rm= TRUE)) # negated because the optimizer minimizes
}

#' Find points along a line
#' @description given two t-a-p parameter vectors, return a sequence of points
#' along the line between them
#' @param start vector of starting points
#' @param stop vector of ending points
#' @param steps number of points to return
#' @return tibble of points along the line
vector_line <- function(start, stop, steps){
  # create a line of points between two vectors
  return(tibble(t = seq(start[1], stop[1], length.out = steps),
                a = seq(start[2], stop[2], length.out = steps),
                p = seq(start[3], stop[3], length.out = steps)) |>
           filter(row_number() > 1, row_number() < steps))

}
#' Find likelihood along a grid
#' @description  create a grid of points in parameter space
# and return the one with minimum likelihood for a given set of counts
#' @param counts data frame of counts, with N_r and N_c columns
#' @return vector of t-a-p parameters with maximum likelihood
picket_fence <- function(counts){

  t <- c(.05, .1, .5, .9, .95)
  a <- c(.05, .1, .2)
  p <- c(.05, .1, .5, .9, .95)

  fence <- expand.grid(t = t, a = a, p = p)

  # apply the function log_likelihood to each row
  fence <- fence %>%
    rowwise() %>%
    mutate(ll = log_likelihood(c(t, a, p), counts)) |>
    ungroup() |>
    slice_min(ll, n = 1) |>
    as.numeric()

  return(fence)
}

#' Round while preserving sum
#' @description round a vector of numbers while preserving the sum
#' @param x vector of numbers
#' @param digits number of digits to round to
# cf https://www.r-bloggers.com/2016/07/round-values-while-preserve-their-rounded-sum-in-r/
#' @return vector of rounded numbers
round_preserve_sum <- function(x, digits = 0) {
  up = 10 ^ digits
  x = x * up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y / up
}


#' faceted density plots for mcmc draws
#' @param x a numerical vector of parameter draws
#' @return a data frame with x, y, and region columns
get_density <- function(x){
  d <- density(x, from = 0, to = 1, bw = .02)
  q <- quantile(x, c(0.05, 0.25, 0.45, .55, 0.75, 0.95)) |> as.list()

  out <- data.frame(x = d$x, y = d$y) |>
    mutate(y = y/max(y), # scale to 0,1
           region = case_when(x < q$`5%` ~ 0,
                              x < q$`25%` ~ 1,
                              x < q$`45%` ~ 2,
                              x < q$`55%` ~ 3,
                              x < q$`75%` ~ 4,
                              x < q$`95%`~ 5,
                              TRUE ~ 6),
           region = as.factor(region))

  return(out)
}

#' Generate t-a-p statistics from ratings -- NOT USED CURRENTLY
#' @description
#' This option is for models that assign individual truth and/or accuracy
#' parameters, so called "random effects" models or "hierarchical" models.
#' The extra detail requires that the individual ratings, rather than counts
#' per subject, must be provided. #'
#' @param ratings a data frame that must include the columns SubjectID, RaterID,
#' and rating that are necessary for the model. For example, if running the
#' t_i-a-p model, the data frame must include the columns SubjectID and rating.
#' @param compiled_model a compiled stan model suitable for this data
#' @param output Specify the output as "model" for the complete stan output object,
#' or "draws" for the draws from the model (the default), or "params" to get the
#' paramter draws in a data frame.
#' @param lambda A non-negative integer to specify the steepness of the logistic
#' function that creates a soft step. Defaults to 30. Not used in all models.
#' @return Depending on the output parameter, either the whole stan model output,
#' a posterior stan \code{draws} object, or a data frame with the parameter draws.
#' @details If you have counts instead of raw ratings, use `fit_counts_mcmc`. This
#' function will work with stan models that accumulate log likelihood from
#' each rating. This requires more indexing, and hence the
#' requirement for the Rater and/or Subject IDs. See `t_i-a-p.stan` for an example.
#' was fit_random_tap_model
fit_ratings_mcmc_deprecated <- function(ratings, compiled_model, output = "draws", lambda = 30) {

  # create indices for subjects and raters

  # make sure at least one of the indices exists
  if(is.null(ratings$SubjectID) & is.null(ratings$RaterID)) stop("SubjectID or RaterID must be present")

  # does SubjectID exist?
  if(is.null(ratings$SubjectID)) ratings$SubjectID <- 1 # unused
  if(is.null(ratings$RaterID)) ratings$RaterID <- 1 # unused

  # remove blanks
  ratings <- ratings |>
    na.omit()

  # convert IDs to integers 1:N for MCMC performance
  ratings <- ratings |>
    mutate(SubjectID = as.integer(factor(SubjectID)),
           RaterID = as.integer(factor(RaterID)))

  N <- nrow(ratings)
  S <- n_distinct(ratings$SubjectID) # number of cases/subjects
  R <- n_distinct(ratings$RaterID)   # ratings for each subject

  # using library(cmdstanr)
  fitted_model <- compiled_model$sample(
    data = list(
      N = N,
      S = S,
      R = R,
      lambda = lambda,
      rating = ratings$rating,
      subject_index = ratings$SubjectID,
      rater_index = ratings$RaterID),
    seed = 123,
    chains = 3,
    parallel_chains = 3,
    refresh = 1000)

  if(output == "model") return(fitted_model)
  draws = fitted_model$draws()

  if(output == "params") return(extract_params_mcmc(draws))

  # the default option
  return(draws)

}

#' creating value maps from ratings
create_rater_map <- function(ratings){
  if(!"rater_id" %in% colnames(ratings)) return(NULL)

  ratings |>
    select(value = rater_id) |>
    distinct() |>
    # the following idiom is used to convert any sortable type to integers in order
    mutate(rater_id = as.integer(as.factor(value))) |>
    select(rater_id, value) |>
    arrange(rater_id)
}

create_subject_map <- function(ratings){
  ratings |>
    select(value = subject_id) |>
    distinct() |>
    mutate(subject_id = as.integer(as.factor(value))) |>
    select(subject_id, value) |>
    arrange(subject_id)
}

create_rating_map <- function(ratings, class_1_values){
  ratings |>
    select(value = rating) |>
    distinct() |>
    mutate(rating = as.integer(value %in% class_1_values)) |>
    select(rating, value) |>
    arrange(value)
}

#################################### E-M Algorithm ############################

#' Log likelihood for a,p and confusion matrix
#' @param params a vector of parameters a and p
#' @param confusion a data frame with columns TP, FP, TN, FN
#' @return the log likelihood of the confusion matrix given the parameters
ap_log_likelihood <- function(params, confusion) {
  a <- params[1]
  p <- params[2]

  a_ <- 1 - a
  p_ <- 1 - p

  ll = confusion$TP*log(a + a_*p ) +
    confusion$FP*log(a_*p) +
    confusion$TN*log(a + a_*p_) +
    confusion$FN*log(a_*p_)

  if(is.nan(ll) | is.infinite(ll)) ll <- -1e6


  return(-ll/log(2))  # Return negative for minimization
}

#' Fit the t-a-p model to counts using EM algorithm
#' @param counts a data frame with columns N_r, N_c, and n
#' @param max_steps the maximum number of iterations to run, defaults to 100
#' @param tolerance the convergence tolerance, defaults to 1e-4
#' @return a data frame with columns t, a, p, ll, and degenerate
find_solution_em <- function(counts, max_steps = 100, tolerance = 1e-4) {

  # number of subjects
  N_s <- sum(counts$n)

  # set initial values
  old_t <- .5
  old_a <- .5
  old_p <- .5
  old_ll <- 1

  for(i in 1:max_steps) {
    e_step <- estimate_tu(counts, params = list(t = old_t, a = old_a, p = old_p))

    # update t
    # averaged over subjects, which are enumerated by n
    t <- e_step |>
      summarize(sum(t_u*n)/N_s) |>
      pull()

    # confusion matrix of means
    confusion <- e_step |>
      summarize(TP = sum(N_c/N_r*t_u*n)/N_s,
                FP = sum(N_c/N_r*(1-t_u)*n)/N_s,
                TN = sum((1- N_c/N_r)*(1-t_u)*n)/N_s,
                FN = sum((1 - N_c/N_r)*t_u*n)/N_s)

    # m_step
    a <- confusion$TP/t - confusion$FP/(1-t)
    p <- confusion$FP/(1-t)/(1-a) # solve for p
    ll <- ap_log_likelihood(params = c(a, p), confusion)

    # if likelihood got worse, return the previous result
    if(ll > old_ll){
      t <- old_t
      a <- old_a
      p <- old_p
      ll <- old_ll
      warning(sprintf("Bits per rating increased from %.4f to %.4f; stopping.", old_ll, ll))
      break
    }

    # check for convergence
    if(abs(t - old_t) < tolerance &&
       abs(a - old_a) < tolerance &&
       abs(p - old_p) < tolerance) {
      break
    }

    old_t  <- t
    old_a  <- a
    old_p  <- p
    old_ll <- ll
  }


  degenerate <- (a == 0 | a == 1 | p == 0 | p == 1 | t == 0 | t == 1)

  return(data.frame(t = t, a = a, p = p, ll = ll, degenerate = degenerate))
}
