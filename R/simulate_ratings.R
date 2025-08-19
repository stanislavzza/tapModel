#' get ratings by sampling a known distribution
#' @param N_s number of subjects
#' @param N_r number of raters per subject on average
#' @param details logical defaulting to FALSE. If TRUE, return the full set
#' of parameters to include the T_i, P_ij, and A_ij values.
#' @return a data frame with ratings and parameters, including the input t-a-p averages, the
#' random effect parameters p_i and a_j and the discrete random variables T_i, P_ij, and A_ij.
#' @export
generate_sample_ratings <- function(N_s = 100, N_r = 5,
                                    params = list(t = .5,
                                                  a = .7,
                                                  p = .5),
                                    details = FALSE) {

  # complete param list and put into environment
  params <- verify_params(params, expand = TRUE)
  list2env(params, envir = environment())

  subject_params <- tibble(subject_id = 1:N_s,
                           T_i = sample(0:1,
                                        N_s,
                                        replace = TRUE,
                                        prob = c(1 - t, t)))


  rater_params <- tibble(rater = 1:N_r)


  param_grid <- subject_params %>%
    cross_join(rater_params)

  n_ratings <- nrow(param_grid)

  # generate ratings based on t_i-a_j-p_i model
  param_grid <- param_grid %>%
    mutate(a_j = if_else(T_i == 1, a1, a0),
           p_i = if_else(T_i == 1, p1, p0),
           A_ij = bernoulli_trial(a_j),
           P_ij = bernoulli_trial(p_i),
           C_ij = if_else(A_ij == 1,T_i,P_ij))

    if(details) {
      param_grid <- param_grid %>%
        select(subject_id, rating = C_ij, rater_id = rater, T_i, A_ij, P_ij)
    } else {
      param_grid <- param_grid %>%
        select(subject_id, rating = C_ij, rater_id = rater)
    }

  return(param_grid)
}


#' Generate ratings that have a given distribution
#' @param n_subjects number of subjects
#' @param n_raters number of raters per subject on average
#' @param params t, a, and p, possibly including a0,a1,p0,p1 in a named list
#' @return a data frame with ratings and parameters, including the input t-a-p averages, the
#' random effect parameters p_i and a_j and the discrete random variables T_i, P_ij, and A_ij.
#' @details This function attempts to exactly replicate the density function
#' specified by the parameters. This can be useful in tests where you want to
#' avoid sampling effects.
#' @export
generate_exact_ratings <- function(N_s = 100, N_r = 5,
                                   params = list(t = .5,
                                                 a = .7,
                                                 p = .5)) {
  # complete param list and put into environment
  params <- verify_params(params, expand = TRUE)
  list2env(params, envir = environment())

  # how many values are in the distro?

  # cumulative probabilities as cut points, with leading zero
  c0_probs <- dbinom(0:N_r, N_r, prob = (1-a0)*p0)
  c1_probs <- dbinom(0:N_r, N_r, prob = a1 + (1-a1)*p1)

  tap_prob <- c0_probs*(1-t) + c1_probs*t

  rating_sum_counts <- tapModel:::round_preserve_sum(tap_prob*N_s)

  # now that we have counts, produce the ratings

  param_grid <- expand.grid(rater = 1:N_r,subject_id = 1:N_s )

  ratings <- c()

  for(i in 0:(length(rating_sum_counts)-1)) {

    # make a single rating of this type
    r <- c(rep(1, i), rep(0, N_r - i))

    # repeat that rating for the number of subjects
    r <- rep(r, rating_sum_counts[i + 1])

    #add it to the list
    ratings <- c(ratings, r)
  }

  param_grid <- param_grid |>
    mutate(rating = as.integer(ratings)) |>
    select(-rater)

  return(param_grid)
}

#' Random ratings with individual parameters
#' @param rating_params The output of `fit_ratings()` or NULL (defult)
#' @param param_list A list with probability vectors t, a, and p, or NULL.
#' Vectors a and p must be the same length. Defaults to NULL.
#' @param use_avg_t If TRUE, averages the t_i values for the ratings and uses that
#' instead of individual subject t_i parameters to generate ratings. This is a
#' more robust kind of simulation.
#' @return A dataframe with columns subject_id, rating, and rater_id in addition
#' to the simulated T_i, A_ij, and P_ij values.
#' @details Exactly one of the two arguments should be provided. This is intended
#' to make it easy to simulate a data set with either the same parameters as a
#' model under study  or to specify a set of parameters from scratch. If the
#' param_list is provided, each rater will rate each subject.
#' @export
generate_ti_aj_pj_ratings <- function(rating_params = NULL, param_list = NULL, use_avg_t = TRUE){
  if(is.null(rating_params) & is.null(param_list)){
    stop("Must provide exactly one of rating_params or param_list")
  }

  if(!is.null(param_list)){
    t <- param_list$t
    a <- param_list$a
    p <- param_list$p

    if(length(a) != length(p)){
      stop("a and p must have the same length")
    }

    if(any(t < 0) | any(t > 1)){
      stop("t must be in [0,1]")
    }

    if(any(a < 0) | any(a > 1)){
      stop("a must be in [0,1]")
    }

    if(any(p < 0) | any(p > 1)){
      stop("p must be in [0,1]")
    }

    # since the subject is rated multiple times, we need to use the same
    # T_i value for all of those
    subjects <- data.frame(subject_id = 1:length(t),
                           t = t)

    # each rater rating is a single event, so we don't need to simulate
    # the A_j and P_j values for each rating
    raters <- data.frame(rater_id = 1:length(a),
                         a = a,
                         p = p)

    rm(t,a,p) # don't want these as globals now

    # each rater rates each subject
    ratings <- cross_join(subjects, raters)
  } else {
    ratings <- rating_params
  }

  if(use_avg_t == TRUE){
    ratings$t = mean(ratings$t)
  }

  # generate the t_i values, which are the same within a subject
  t_i <- ratings |>
         distinct(subject_id, t) |>
         mutate(T_i = bernoulli_trial(t)) |> # true class?
         select(subject_id, T_i)

  ratings <- ratings %>%
    left_join(t_i, by = "subject_id") |>
    mutate(A_ij = bernoulli_trial(a), # is it an accurate rating?
           P_ij = bernoulli_trial(p), # if not, what's the guess?
           rating = T_i*A_ij + (1-A_ij)*P_ij) |>
    select(subject_id, rating, rater_id, t, a, p, T_i, A_ij, P_ij)

  return(ratings)
}

#' Extract true parameters from a simulation
#' @param rating_params from `generate_ti_aj_pj_ratings()` which must contain
#' the binary T_i, A_ij, and P_ij values.
#' @return A rating_params data frame with the true t, a, and p values, calculated
#' from the averages of the T_i, A_ij, and P_ij values.T_i will be binary because
#' it's the same for each subject.
#' @export
true_params_from_sim <- function(rating_params){
  # empirical averages for true params
  true_subjects <- rating_params|>
    group_by(subject_id) |>
    summarize(t = mean(T_i)) # should be 0 or 1

  true_raters <- rating_params |>
    group_by(rater_id) |>
    summarize(a = mean(A_ij),
              p = mean(if_else(A_ij == 1, NA, P_ij), na.rm = TRUE))

  # join the true parameters to the ratings
  rating_params |>
    select(subject_id, rating, rater_id) |>
    left_join(true_subjects, by = "subject_id") |>
    left_join(true_raters, by = "rater_id")

}
