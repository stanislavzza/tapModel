# include files for the shiny app

# wrapper function to cache results
get_mcmc_results <- function(counts){
  hash_name <- rlang::hash(counts)
  # get filenames in the /cache folder
  cache_files <- list.files("cache/mcmc") |>
    str_remove("\\.rds")

  if(hash_name %in% cache_files){
    return(read_rds(paste0("cache/mcmc/",hash_name,".rds")))
  } else {
    results <- mcmc_stats(counts)
    write_rds(results, paste0("cache/mcmc/",hash_name,".rds"))
    return(results)
  }

}

# MCMC for the t-a0,a1-p model
# wrapper function to cache results
get_mcmc_results2 <- function(counts, p0p1){
  hash_name <- rlang::hash(counts)
  # get filenames in the /cache folder
  if(isTRUE(p0p1)){
    folder <- "cache/mcmc3/"
  } else {
    folder <- "cache/mcmc2/"
  }

  cache_files <- list.files(folder) |>
    str_remove("\\.rds")

  if(hash_name %in% cache_files){
    return(read_rds(paste0(folder,hash_name,".rds")))
  } else {
    results <- mcmc_stats2(counts, p0p1)
    write_rds(results, paste0(folder,hash_name,".rds"))
    return(results)
  }

}

#' Fit the t-a-p model using MCMC with stan
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters and counts of raters who rated class 1.
#' @return a list with two elements: stats and draws. The stats vector has the
#' t,a,p estimates and the draws data frame has draws, e.g. for plotting.
#' @details
#' This function uses the stan model \code{t-a-p.stan} to fit the model to the data.
#' It assumes flat priors over the [0,1] domains of the parameter space. This
#' function requires that stan has been installed, and the \code{cmdstanr} package is available.
#'
mcmc_stats <- function(counts){
  library(cmdstanr)
  #  library(posterior)
  #  library(LaplacesDemon)

  compiled_model <- cmdstan_model("stan/t-a-p.stan")
  model_draws <- fit_tap_model(counts, compiled_model)
  tap_stats <- get_tap_stats(model_draws) |>
    mutate(var = substr(var,1,1), # "accuracy" -> "a"
           Degenerate = !is.na(mode2))

  draws <- extract_vars(model_draws) |>
    rename(a = accuracy)

  return(list(stats = tap_stats, draws = draws))
}

#' Fit the t-a0a1-p0p1 model using MCMC with stan
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters and counts of raters who rated class 1.
#' @return a list with two elements: stats and draws. The stats vector has the
#' t,a,p estimates and the draws data frame has draws, e.g. for plotting.
#' @details
#' This function uses the stan model \code{stan/t-a0a1-p0p1.stan} to fit the model to the data.
#' It assumes flat priors over the [0,1] domains of the parameter space. This
#' function requires that stan has been installed, and the \code{cmdstanr} package is available.
#'
mcmc_stats2 <- function(counts, p0p1){
  library(cmdstanr)

  if(isTRUE(p0p1)){
    stan_model <- "stan/t-a0a1-p0p1.stan"
  } else {
    stan_model <- "stan/t-a0a1-p.stan"
  }

  compiled_model <- cmdstan_model(stan_model)
  model_draws <- fit_tap_model(counts, compiled_model)
  tap_stats <- get_tap_stats(model_draws) |>
    mutate(Degenerate = !is.na(mode2))

  draws <- extract_vars(model_draws)

  return(list(stats = tap_stats, draws = draws))
}
