#' Generate t-a-p statistics from counts
#' @param counts a data frame with *summarized* ratings counts, one row per
#' pair (N_r, N_c) with a column n for the count of occurrences. It
#' must include columns N_r, N_c, and n. This is the output of `as_counts(ratings)`.
#' @param stan_model a stan model suitable for count data. One of "t-a-p" (default)
#' or "t-a0a1-p" for separate accuracies for the two classes, or "t-a0a1-p0p1"
#' to also include separate bias parameters. You can retrieve these models, e.g.
#' with  `system.file("inst/stan", "t-a-p.stan", package = "tapModel")`
#' @param quiet If TRUE, discards the normal status updates. This is useful
#' if generating a qmd file, for example. Defaults to FALSE.
#' @param mcmc_chains number of MCMC chains to run, defaulting to 3
#' @param mcmc_iter number of MCMC iterations per chain, defaulting to 1000
#' @param mcmc_warmup number of MCMC burn-in iterations, defaulting to 300
#' @param mcmc_cores number of cores to use for MCMC, defaulting to 3
#' @return a list with params = estimated parameters, and fitted_model = the
#' fitted model. You can use `shinystan::launch_shinystan(fitted_model)` to
#' explore the model fit.
#' @details Requires `library(cmdstanr)` and `library(LaplacesDeamon)`. If you're
#' looking for models that assign individual truth and/or accuracy parameters,
#' first estimate these with `rating_params = fit_ratings(ratings)`, then use
#'  `fit_ratings_mcmc(rating_params)` instead of this function.
#' @export
fit_counts_mcmc<- function(counts,
                           stan_model = "t-a-p",
                           quiet = FALSE,
                           mcmc_chains = 3,
                           mcmc_iter = 1000,
                           mcmc_warmup = 300,
                           mcmc_cores = 3) {

  stan_model <- system.file("stan", str_c(stan_model,".stan"), package = "tapModel")

  if (.Platform$OS.type == "windows") {
    null_device <- "nul"
  } else {
    null_device <- "/dev/null"
  }

  if(quiet) sink(null_device)

  compiled_model <- cmdstanr::cmdstan_model(stan_model)
  # was fit_tap_model
  # using
  fitted_model <- compiled_model$sample(
    data = list(
      N = nrow(counts),
      N_r = counts$N_r,
      N_c = counts$N_c,
      n   = counts$n),
    seed = 123,
    chains = 3,
    parallel_chains = 3,
    refresh = 1000)

  draws = fitted_model$draws()

  if(quiet) sink(NULL) # reset output

  return(list(params = tapModel::estimate_params_mcmc(draws),
         fitted_model = fitted_model))
}

#' Get t-a-p coefficients from MCMC draws
#' @param model_draws Use `fit_counts_mcmc` or `fit_ratings_mcmc` to get the
#' fitted model, then use the `draws()` method to get the draws, e.g.
#' `fitted_model$draws()`. Don't forget the parentheses--this is a function
#' call.
#' @return a data frame with var as the parameter name, with columns for
#' mean (avg), 5th percentile (p05), 25th percentile (p25), median, 75th
#' percentile (p75), 95th percentile (p95), mode1, mode2, and standard deviation.
#' @export
estimate_params_mcmc <- function(model_draws){

  model_means <- data.frame(position = 1:dim(model_draws)[3],
                            var = attr(model_draws, "dimnames")$variable,
                            avg = NA, # mean
                            p05 = NA, # 5th percentile
                            p25 = NA, # 25th percentile
                            median = NA, # median
                            p75 = NA, # 75th percentile
                            p95 = NA, # 95th percentile
                            mode1 = NA,
                            mode2 = NA,
                            sd = NA)
  #filter(str_detect(var,"^t|^a|^p"))

  # get parameter averages
  for(i in 1:nrow(model_means)){
    j <- model_means$position[i]

    model_means$avg[i] <- model_draws[,,j] %>% mean()
    model_means$median[i] <- model_draws[,,j] %>% median()
    model_means$p05[i] <- quantile(model_draws[,,j], 0.05)
    model_means$p25[i] <- quantile(model_draws[,,j], 0.25)
    model_means$p75[i] <- quantile(model_draws[,,j], 0.75)
    model_means$p95[i] <- quantile(model_draws[,,j], 0.95)
    model_means$sd[i] <- sd(model_draws[,,j])

    # get modes
    model_means$mode1[i] <- LaplacesDemon::Modes(model_draws[,,j])$modes[1]

    if(length(Modes(model_draws[,,j])$modes) > 1){
      model_means$mode2[i] <- Modes(model_draws[,,j])$modes[2]
    }
  }

  return(model_means)
}

#' Extract the parametersfrom a stan draws object
#' @description Create a data frame from a draws object, with one row per draw
#' and one column per parameter
#' @param model_draws a stan draws object
#' @return a data frame with one row per draw and one column per parameter
#' @details This is a useful utility for combining the chains to assemble a
#' data frame with parameters as column names and all the draws underneath.
#' Was called extract_vars() in a prior version of the package.
#' @export
extract_param_draws_mcmc <- function(model_draws){

  var_positions <- data.frame(position = 1:dim(model_draws)[3],
                              var = attr(model_draws, "dimnames")$variable)

  # initialize a dataframe of the correct length by
  # storing the log probability column

  draws_by_var <- tibble(likelihood = model_draws[,,1] |> as.vector() )

  for(i in 2:nrow(var_positions)){
    j <- var_positions$position[i]
    var_name <- var_positions$var[i]

    draws_by_var <- draws_by_var %>%
      mutate(!!var_name := (model_draws[,,j] |> as.vector()))
  }

  return(draws_by_var)
}

#' plot mcmc draw densities
#' @param model_draws Use `fit_counts_mcmc` or `fit_ratings_mcmc` to get the
#' fitted model, then use the `draws()` method to get the draws, e.g.
#' `fitted_model$draws()`. Don't forget the parentheses--this is a function
#' call.
#' @return a ggplot object with densities for each parameter
#' @export
plot_densities_mcmc <- function(model_draws){
  # convert the draws matrix to a data frame
  model_draws <- tapModel::extract_param_draws_mcmc(model_draws)

  # prepare for plotting
  pdf <- model_draws |>
    gather(var, value, -likelihood)


  param_density <- pdf |>
    group_by(var) %>%
    do(tapModel:::get_density(.$value))

  # create likelihood densities
  ll_density <- pdf |>
    group_by(var) |>
    mutate(scaled = round(value*2, 1) * .5) |>
    group_by(var, scaled) |>
    summarise(likelihood = mean(likelihood),
              value = mean(value)) |>
    group_by(var) |>
    mutate(likelihood = (likelihood - min(likelihood))/(max(likelihood) - min(likelihood)) ) |>
    ungroup() |>
    select(-scaled)

  ll_means <- pdf |>
    group_by(var) |>
    summarise(value = mean(value))

  param_density |>
    ggplot(aes(x  = x, y = y)) +
    geom_line()  +
    geom_ribbon( aes( fill = region, ymax = y, group = region),
                 ymin = 0,
                 position = position_identity()) +
    scale_fill_manual(values = c("#EEEEFF33",
                                 "#BBBBFF55",
                                 "#4444FF88",
                                 "#2222AA",
                                 "#4444FF88",
                                 "#BBBBFF55",
                                 "#EEEEFF33"),
                      drop = TRUE,  # omit unused factors
                      limits = factor(0:6)) +
    geom_line(data = ll_density, aes(x = value, y = likelihood),
              linetype = "dotted", color = "#666666") +
    geom_vline(data = ll_means, aes(xintercept = value), color = "darkorange") +
    geom_label(data = ll_means,
               aes(x = value, label = round(value,2)),
               y = .5, color = "orange",
               label.padding = unit(0.15, "lines")) +
    theme_bw() +
    theme(text=element_text(size=15),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none") +
    xlim(0,1)  +
    ylab("probability density") +
    xlab("") +
    facet_grid(var ~ ., scales = "free_y")
}

#' fit ratings with Bayesian MCMC
#' @param ratings a ratings data frame OR a one augmented with parameter estimates,
#' the output of `fit_ratings()`. If the former, initial estimates will be obtained
#' by first running `fit_counts_mcmc` on the counts of ratings, and then
#' `fit_ratings()` used to get starting estimates for the
#' individual parameters.
#' @param model the model to fit, one of "t_i-a_j-p_j" (default) or "t-a_j-p_j"
#' @param quiet If TRUE, discards the normal status updates. This is useful
#' if generating a qmd file, for example. Defaults to FALSE.
#' @param mcmc_chains number of MCMC chains to run, defaulting to 3
#' @param mcmc_iter number of MCMC iterations per chain, defaulting to 1000
#' @param mcmc_warmup number of MCMC burn-in iterations, defaulting to 300
#' @param mcmc_cores number of cores to use for MCMC, defaulting to 3
#' @param mcmc_prior_sd use the priors from the t-a-p estimates as means for the
#' parameters and the standard deviations specified here as a 3-vector in order
#' t, a, p, defaulting to c(1,1,1). Use c(.01,.05,.05) for a very tight constraint.
#' If any of these is set to 1 or more, the prior for that parameter is set to
#' a uniform distribution on [0,1].
#' @details This function takes the estimates from the three parameter t-a-p
#' as initial values, and applies priors that are specified by the user with
#' the mcmc_prior_sd parameter.
#' @return a list with rating_params =data frame with the original ratings
#' data augmented with parameter estimates using the mode1 of the posterior
#' distribution, and fitted_model = the fitted model. Note that if you
#' want more detailed summary information about the parameters, you can use
#' `tapModel::estimate_params_mcmc(fitted_model$draws())` where the
#' fitted_model is the output of this function.
#' @export
fit_ratings_mcmc <- function(ratings,
                             stan_model = "t_i-a_j-p_j",
                             quiet = FALSE,
                             mcmc_chains = 3,
                             mcmc_iter = 1000,
                             mcmc_warmup = 300,
                             mcmc_cores = 3,
                             mcmc_prior_sd = c(1,1,1)){

  # check if the input is ratings or rating_params
  verify_ratings(ratings) # basic properties

  if(!("t" %in% names(ratings))){  # ratings, need to add params

    # get the counts of ratings
    counts <- as_counts(ratings)

    # fit the model to the counts
    fitted_model <- fit_counts_mcmc(counts, stan_model = "t-a-p",
                                    quiet = quiet,
                                    mcmc_chains = mcmc_chains,
                                    mcmc_iter = mcmc_iter,
                                    mcmc_warmup = mcmc_warmup,
                                    mcmc_cores = mcmc_cores)

    # get the parameter estimates
    avg_params <- fitted_model$params |>
                  filter(var %in% c("t", "a", "p")) |>
                  select(var, avg) |>
                  spread(var, avg)

    # check for degeneracy
    if(is_degenerate(avg_params)){
      stop("Degenerate solution: t, a, or p is 0 or 1")
    }

    # find the E-M starter parameter if requested
    rating_params <- fit_ratings(ratings, avg_params)
  } else {
    rating_params <- ratings # already have estimated params
  }


  # if a value is too close to zero or one, it causes problems with init
  fudge <- function(x){
    x <- if_else(x < 1e-4, 1e-4, x)
    x <-if_else( x > 1-1e-4, 1-1e-4, x)
    return(x)
  }

  init_values <- function() {
    list(
      t = fudge(param_list$subjects$t),  # Vector of t_i estimates
      a = fudge(param_list$raters$a),  # Vector of a_j estimates
      p = fudge(param_list$raters$p)  # Vector of p_j estimates
    )
  }

  # The first version uses what we know about the individual parameters to
  # inform the mcmc analysis by providing initial values and setting strong
  # priors around the estimates.
  stan_file <- system.file("stan", str_c(stan_model,".stan"), package = "tapModel")

  if (.Platform$OS.type == "windows") {
    null_device <- "nul"
  } else {
    null_device <- "/dev/null"
  }

  if(quiet) sink(null_device)

  compiled_model <- cmdstanr::cmdstan_model(stan_file)

  # get a list of the parameters for subjects and raters
  param_list <- pull_rating_params(rating_params)

  # fix for single-parameter t
  if(stan_model == "t-a_j-p_j"){
    param_list$subjects <- param_list$subjects |>
      summarize(t = mean(t))
  }

  # set the uniform distribution flag
  uniform_prior <- ifelse(mcmc_prior_sd >= 1, 1, 0)

  fitted_model <- compiled_model$sample(
    data = list(
        N = nrow(rating_params),
        S = n_distinct(rating_params$subject_id),
        R = n_distinct(rating_params$rater_id),
        rating = rating_params$rating,
        subject_index = rating_params$subject_id,
        rater_index = rating_params$rater_id,
        t_est = param_list$subjects$t,
        a_est = param_list$raters$a,
        p_est = param_list$raters$p,
        t_prior_sd = mcmc_prior_sd[1],
        a_prior_sd = mcmc_prior_sd[2],
        p_prior_sd = mcmc_prior_sd[3],
        use_uniform_t= uniform_prior[1],
        use_uniform_a= uniform_prior[2],
        use_uniform_p= uniform_prior[3]),
    init = init_values, # must be a function that returns a list
    seed = 123,
    chains = mcmc_chains,
    parallel_chains = mcmc_cores,
    iter_warmup = mcmc_warmup,
    refresh = 1000)

  mcmc_means <- tapModel::estimate_params_mcmc(fitted_model$draws()) |>
    filter(var != "lp__") |>
    select(var, mode1) |> # using mode instead of average
    separate(var, into = c("var", "id"), sep = "\\[|\\]") |>
    spread(var, mode1) |>
    mutate(id = as.integer(id)) |>
    arrange(id)

  if(stan_model == "t-a_j-p_j"){
    rating_params <- ratings |>
      select(subject_id, rater_id, rating) |>
      # add the a, p stats per rater
      left_join(mcmc_means |> select(id, a, p),  by = c("rater_id" = "id")) |>
      select(subject_id, rating, rater_id, a , p)

    # add the constant t to the rating_params
    t <- mcmc_means |> select(t) |> na.omit() |> pull()

    rating_params$t  <- t

    # now induce the subject t from this
    t_i <- estimate_ti(rating_params)

    # replace the constant version
    rating_params <- rating_params |>
      select(-t) |>
      left_join(t_i, by = "subject_id") |>
      select(subject_id, rating, rater_id, t, a , p)

  } else {
  rating_params <- ratings |>
    select(subject_id, rater_id, rating) |>
    # add the t stats per subject
    left_join(mcmc_means |> select(id, t), by = c("subject_id" = "id")) |>
    # add the a, p stats per rater
    left_join(mcmc_means |> select(id, a, p),  by = c("rater_id" = "id")) |>
    select(subject_id, rating, rater_id, t, a , p)
  }

  if(quiet) sink(NULL) # reset output

  return(list(rating_params = rating_params, fitted_model = fitted_model))
}

#' fit ratings with Bayesian MCMC using latent parameters
#' @param rating_params a ratings data frame augmented with parameter estimates,
#' the output of `fit_ratings()`
#' @param quiet If TRUE, discards the normal status updates. This is useful
#' if generating a qmd file, for example. Defaults to FALSE.
#' @param mcmc_chains number of MCMC chains to run, defaulting to 3
#' @param mcmc_iter number of MCMC iterations per chain, defaulting to 1000
#' @param mcmc_warmup number of MCMC burn-in iterations, defaulting to 300
#' @param mcmc_cores number of cores to use for MCMC, defaulting to 3
#' @details This function takes the estimates from the three parameter t-a-p
#' and uses them as initial values to estimate all the paramters on latent
#' scales with regularization. Instead of parameters in [0,1], they are
#' on the real line, and mapped to [0,1] with the logistic function. Each of
#' t, a, and p have an average parameter with offsets for the individual
#' parameters of the form tau_i = tau_avg + sd_tau*tau_i, where tau_avg, tau_i,
#' and sd_tau have normal(0,1) priors. The idea is that the model can optimize
#' between average and freely roaming individual parameters via shrinkage,
#' to optimize likelihood without overfitting the data.
#' @return a list with rating_params =data frame with the original ratings
#' data augmented with parameter estimates, and fitted_model = the fitted model.
#' @export
fit_ratings_mcmc_latent <- function(rating_params,
                                    quiet = FALSE,
                                    mcmc_chains = 3,
                                    mcmc_iter = 1000,
                                    mcmc_warmup = 300,
                                    mcmc_cores = 3,
                                    mcmc_prior_sd = c(.1,.05,.05)){

  # if a value is too close to zero or one, it causes problems with init
  fudge <- function(x){
    x <- if_else(x < 1e-4, 1e-4, x)
    x <-if_else( x > 1-1e-4, 1-1e-4, x)
    return(x)
  }

  init_values <- function() {
    # Input data
    t = fudge(param_list$subjects$t)  # Vector of t_i estimates
    a = fudge(param_list$raters$a)  # Vector of a_j estimates
    p = fudge(param_list$raters$p)  # Vector of p_j estimates

    # Global means
    mu_t <- mean(t)
    mu_a <- mean(a)
    mu_p <- mean(p)

    # Shrinkage parameter
    sigma <- 0.8  # Standard deviation for offsets

    # Logit function
    logit <- function(x) log(x / (1 - x))

    # Calculate latent offsets (z values)
    z_t <- (logit(t) - logit(mu_t)) / sigma
    z_a <- (logit(a) - logit(mu_a)) / sigma
    z_p <- (logit(p) - logit(mu_p)) / sigma

    # Prepare initial values for Stan
    init_list <- list(
      mu_t = logit(mu_t),
      mu_a = logit(mu_a),
      mu_p = logit(mu_p),
      sigma_t = sigma,
      sigma_a = sigma,
      sigma_p = sigma,
      z_t = z_t,
      z_a = z_a,
      z_p = z_p
    )

    return(init_list)
  }

  # The first version uses what we know about the individual parameters to
  # inform the mcmc analysis by providing initial values and setting strong
  # priors around the estimates.
  stan_model <- system.file("stan", "t_i-a_j-p_j latent.stan", package = "tapModel")
  #stan_model <- "inst/stan/t_i-a_j-p_j latent.stan"

  if (.Platform$OS.type == "windows") {
    null_device <- "nul"
  } else {
    null_device <- "/dev/null"
  }

  if(quiet) sink(null_device)

  compiled_model <- cmdstanr::cmdstan_model(stan_model)

  # get a list of the parameters for subjects and raters
  param_list <- pull_rating_params(rating_params)

  # set the uniform distribution flag
  uniform_prior <- ifelse(mcmc_prior_sd >= 1, 1, 0)

  fitted_model <- compiled_model$sample(
    data = list(
      N = nrow(rating_params),
      S = n_distinct(rating_params$subject_id),
      R = n_distinct(rating_params$rater_id),
      rating = rating_params$rating,
      subject_index = rating_params$subject_id,
      rater_index = rating_params$rater_id),
    init = init_values, # must be a function that returns a list
    seed = 123,
    chains = mcmc_chains,
    parallel_chains = mcmc_cores,
    iter_warmup = mcmc_warmup,
    refresh = 1000)

  mcmc_means <- tapModel::estimate_params_mcmc(fitted_model$draws()) |>
    filter(var != "lp__") |>
    select(var, avg) |> # using mode instead of average
    separate(var, into = c("var", "id"), sep = "\\[|\\]") |>
    spread(var, avg) |>
    mutate(id = as.integer(id)) |>
    arrange(id)

  rating_params <- ratings |>
    select(subject_id, rater_id, rating) |>
    # add the t stats per subject
    left_join(mcmc_means |> select(id, t), by = c("subject_id" = "id")) |>
    # add the a, p stats per rater
    left_join(mcmc_means |> select(id, a, p),  by = c("rater_id" = "id")) |>
    select(subject_id, rating, rater_id, t, a , p) |>
    # add columns for log likelihood in bits
    mutate(C1 = -log2(t*a + (1-a)*p),     # log(Pr[Class 1 rating|params])
           C0 = -log2( 1- t*a - (1-a)*p)) # log(Pr[Class 0 rating|params])


  if(quiet) sink(NULL) # reset output

  return(list(rating_params = rating_params, fitted_model = fitted_model))
}

#' Pull average rating parameters as an array
#' @param the output of `fit_counts_mcmc()`
#' @return a named array with the average t, a, and p parameters
#' @export
pull_avg_params <- function(params){

 params |>
    filter(str_sub(var,1,1) %in% c("t","a","p")) |> # possibly includes a0, a1, etc
    select(var, avg) |>
    spread(var, avg)
}
