#' get ratings by sampling a known distribution
#' @param N_s number of subjects
#' @param N_r number of raters per subject on average
#' @param t truth average
#' @param a accuracy average
#' @param p guess rate average
#' @return a data frame with ratings and parameters, including the input t-a-p averages, the
#' random effect parameters p_i and a_j and the discrete random variables T_i, P_ij, and A_ij.
#' @details This function generates ratings for a set of subjects based on a t-a-p
#' model. It uses t to determine the 0 or 1 truth values for each subject,
#' creates random effects p_i and a_j for each subject and rater, respectively, using a beta distribution.
#' @export
generate_sample_ratings <- function(N_s = 100, N_r = 5,
                                    t = .5, a0 = .7, a1 = .7,
                                    p0 = .5, p1 = .5) {

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
           A_ij = as.integer(a_j > runif(n_ratings)),
           P_ij = as.integer(p_i > runif(n_ratings)),
           C_ij = if_else(A_ij == 1,T_i,P_ij)) |>
    # reorder output
    select(subject_id, rater_id = rater, rating = C_ij) # random variables

  return(param_grid)
}


#' get ratings that have a given distribution
#' @param n_subjects number of subjects
#' @param n_raters number of raters per subject on average
#' @param t truth average
#' @param a accuracy average
#' @param p guess rate average
#' @return a data frame with ratings and parameters, including the input t-a-p averages, the
#' random effect parameters p_i and a_j and the discrete random variables T_i, P_ij, and A_ij.
#' @details This function generates ratings for a set of subjects based on a t-a-p
#' model. It uses t to determine the 0 or 1 truth values for each subject,
#' creates random effects p_i and a_j for each subject and rater, respectively, using a beta distribution.
#' @export
generate_exact_ratings <- function(N_s, N_r, t, a0, a1, p0, p1) {
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
