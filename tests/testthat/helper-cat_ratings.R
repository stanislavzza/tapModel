library(dplyr)
# tests/testthat/helper-cat_ratings.R
# Utility functions for constructing toy cat_ratings objects in tests

# Minimal cat_ratings constructor
set_cat_params <- function(cat_ratings, t_arg, a_arg, p_arg) {

  t_arg <- t_arg / sum(t_arg)
  p_arg <- p_arg / sum(p_arg)

  cat_ratings$subjects <- cat_ratings$subjects |>
    dplyr::mutate(t = list(t_arg))

  cat_ratings$raters <- cat_ratings$raters |>
    dplyr::mutate(a = a_arg, p = list(p_arg))

  return(cat_ratings)
}

