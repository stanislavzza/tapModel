#' Fleiss kappa
#' @description Function to calculate Fleiss' kappa for binary data
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters for each subject and number of raters who rated class 1, and n for the
#' number of these (N_r, N_c) pairs. Optionally the row counts can be used, and the
#' function will convert
#' @return a data frame with one row and columns for parameters: t, a, p, log likelihood: ll,
#' and a true/false flag for degenerate solutions.
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
  ll    <- -log_likelihood(c(stats$c, sqrt(kappa), stats$c), counts)
  degenerate <- is_degenerate(c(stats$c, sqrt(kappa), stats$c)) || kappa <= 0

  return(data.frame(t = stats$c, a = sqrt(kappa), p = stats$c, ll = ll, degenerate = degenerate))
}

#' Information bits from a binary vector
#' @param x a probability
#' @return bits of information using Shannon entropy
#' @export
information_bits <- function(x){
 if_else(x == 0 | x== 1, 0, -x*log2(x) - (1-x)*log2(1-x))
}

#' Signal and noise
#' @param params the three average t-a-p parameters in a named list. If vectors
#' are provided instead, they will be averaged.
#' @return a data frame with columns for information from sources signal, noise,
#' and the total log likelihood per rating
#' @export
est_signal_noise <- function(params){

  t <- mean(params$t)
  a <- mean(params$a)
  p <- mean(params$p)

  tibble(I_signal = a*information_bits(t),
         I_noise =  (1-a)*information_bits(p),
         I_sum = I_signal + I_noise,
         ll = information_bits(a*t + (1-a)*p))
}


#' rater entropy
#' @description Calculates rater-specific entropy s-dot, which can be averaged
#' to estimate the average rater entropy.
#' @param rater_params Either a dataframe, typically from
#' `pull_rating_params(rating_params)$raters` with columns for rater_id, a_j, p_j,
#' OR the results of `fit_counts(counts)` with the average t-a-p parameters.
#' @return The same dataframe with an additional column s_dot for the
#' rater entropy.
#' @export
rater_entropy <- function(rater_params){

    rater_params |>
    mutate(s_dot = -.5*( (1-(1-a)*p)*log2(1-(1-a)*p) +
                          (1-a)*p *log2((1-a)*p ) +
                          (1-a)*(1-p)*log2((1-a)*(1-p)) +
                          (a + (1-a)*p)*log2(a + (1-a)*p)))
}

#' bernoulli trial
#' @description Simulate a Bernoulli trial with a given probability
#' @param p a vector of probabilities of success
#' @return a vector of 0s and 1s
#' @export
bernoulli_trial <- function(p){
  as.integer(runif(length(p)) < p)
}
