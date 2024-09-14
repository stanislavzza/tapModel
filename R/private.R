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

#' Is the t-a-p model degenerate?
#' @description given t-a-p parameters, return TRUE if the model is degenerate
#' @param params vector of t-a-p parameters
#' @param threshold threshold for degeneracy, defaults to x < 1e-6
#' @return TRUE if the model is degenerate
is_degenerate <- function(params, threshold = 1e-6){
  eps1 <- params[1]*params[2]*params[3]
  eps2 <- (1-params[1])*(1-params[2])*(1-params[3])

  if(eps1 > threshold & eps2 > threshold) {return(FALSE)}
  return(TRUE)
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
