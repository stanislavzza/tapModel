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
  counts <- verify_counts(counts)

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

#' negative log likelihood for binary ratings using t-a-p
#' @description Use the implied binomial mixture to calculate the negative log likelihood. It's
#' negated because the optimizer minimizes rather than maximizes.
#' @param params parameters t, a, and p in a vector
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters and counts of raters who rated class 1.
#' @return the log likelihood of the t-a-p model, or 1e6 if the parameters are out of range
#' @export
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

#' Fit the t-a-p model to binary ratings
#' @param counts a summary data frame with columns N_r, N_c, representing replications
#' of unique pairs of (N_r, N_c). This is the output of `count_ratings` with
#' `summarize = TRUE`
#' raters and counts of raters who rated class 1.
#' @param init_params initial values for t, a, and p
#' @return a data frame with columns for parameters: t, a, p, and log likelihood of the solution
#' @export
find_solution <- function(counts, init_params){

  params <- optim(par = init_params,
                  fn = log_likelihood,
                  counts = counts,
                  lower = c(0,0,0),
                  upper = c(1,1,1),
                  method = "L-BFGS-B")$par

  return(c( params, log_likelihood(params, counts)))
}

#' Generate t-a-p statistics from counts
#' @param counts a data frame with *summarized* ratings counts, one row per
#' pair (N_r, N_c) with a column n for the count of occurrences. It
#' must include columns N_r, N_c, and n. This is the output of `count_ratings`
#' with `summarize = TRUE`.
#' @param compiled_model a compiled stan model suitable for this data, usually
#' "t-a-p optim.stan"
#' @param output Specify the output as "model" for the complete stan output object,
#' or "draws" for the draws from the model (the default), or "params" to get the
#' paramter draws in a data frame.
#' @return Depending on the output parameter, either the whole stan model output,
#' a posterior stan \code{draws} object, or a data frame with the parameter draws.
#' @details The compiled stan model must be able to accommodate varying numbers
#'  of raters per #' subject. The basic model is in the file t-a-p.stan, but
#'  t-a0a1-p.stan and t-a0a1-p0p1.stan are also supported. For models that
#'  assign individual truth and/or accuracy parameters use `fit_random_tap_model`.
#' @export
fit_tap_model <- function(counts, compiled_model, output = "draws") {

  # using library(cmdstanr)
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

  if(output == "model") return(fitted_model)
  draws = fitted_model$draws()

  if(output == "params") return(extract_vars(draws))

  return(draws)

}

#' Generate t-a-p statistics from ratings
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
#' @details If you have counts instead of raw ratings, use `fit_tap_model`. This
#' function will work with stan models that accumulate log likelihood from
#' each rating. This requires more indexing, and hence the
#' requirement for the Rater and/or Subject IDs. See `t_i-a-p.stan` for an example.
#' @export
fit_random_tap_model <- function(ratings, compiled_model, output = "draws", lambda = 30) {

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

  if(output == "params") return(extract_vars(draws))

  # the default option
  return(draws)

}

#' Extract the draws from a stan draws object
#' @description Create a data frame from a draws object, with one row per draw
#' and one column per parameter
#' @param model_draws a stan draws object
#' @return a data frame with one row per draw and one column per parameter
#' @export
extract_vars <- function(model_draws){

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

#' Get t-a-p coefficients
#' @param model_draws The default output (output = "draws") of the `fit_tap_model`
#'  or `fit_random_tap_model` function
#' @return a list with the t-a-p coefficients. If there are two modes for a
#' these are returned as a0 and a1
#' @export
get_tap_stats <- function(model_draws){

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

#' iterative optimization
#' @description Start with the the middle of the parameter space in looking
#' for a solution. If it's degenerate it looks around the edges of the space
#' for a non-degenerate solution.
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters and counts of raters who rated class 1. This can be created from ratings |> as_counts()
#' @return a data frame with columns for parameters: t, a, p, and log likelihood
#' of the solution, and a flag for degeneracy
#' @export
iterative_optim <- function(counts){
  # start with the middle case
  params <- find_solution(counts, c(.5,.5,.5))

  if(is_degenerate(params)) {
    fence_params <- picket_fence(counts)[1:3]
    params <- find_solution(counts, fence_params)
  }

  return(data.frame( t = params[1],
                     a = params[2],
                     p = params[3],
                     ll = -params[4],
                     degenerate = is_degenerate(params)))

}

#' faceted density plots for mcmc draws
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

#' plot mcmc draw densities
#' @param draws a data frame with one row per draw and one column per parameter
#' @return a ggplot object with densities for each parameter
#' @export

plot_draw_densities <- function(draws){

  # prepare for plotting
  pdf <- draws |>
    gather(var, value, -likelihood)


  param_density <- pdf |>
    group_by(var) %>%
    do(get_density(.$value))

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

#' tap optim over ordinal scale
#' @param ratings a data frame in long format with columns subject_id and rating
#' where the ratings are for a single dimension, with multiple ordinal values,
#' for example water quality on a scale of 1 = poor, 2 = fair, 3 = good,
#' 4 = excellent. If the ratings are character strings, the will be sorted
#' alphabetically. If the ratings are numeric, they will be sorted numerically.
#' @return a data frame with columns for each cut point between ratings,
#' parameters: t, a, p, for the t-a-p model and the Fleiss kappa.
#' @export
get_ordinal_tap <- function(ratings){
  rating_values <- sort(unique(ratings$rating))
  n_values <- length(rating_values)

  output <- data.frame()
  for(i in 1:(n_values - 1)){
    lower_rating <- rating_values[i]
    upper_rating <- rating_values[i+1]

    counts <- ratings |>
      group_by(subject_id) |>
      summarize(N_r = n(),
                N_c = sum(rating <= lower_rating)) |>
      filter(N_r >= 2)

    tap_params <- iterative_optim(counts) |>
      mutate(CutPoint = str_c(lower_rating,"|", upper_rating),
             type = "t-a-p")

    fleiss_params <- fleiss_kappa(counts) |>
      mutate(CutPoint = str_c(lower_rating,"|", upper_rating),
             type = "Fleiss")

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
#' a single row and columns named t, a, and, p.
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
prob_t <- function(counts, params){

  t <- params$t
  a <- params$a
  p <- params$p

  pr_c0 <- (1-a)*p
  pr_c1 <- a + (1-a)*p

  distr <- counts |>
    rowwise() |>
    mutate(C0 = (1-t)*dbinom(N_c, N_r, pr_c0),
           C1 = t*dbinom(N_c, N_r, pr_c1),
           t_u = C1/(C0 + C1)) # estimate Pr[Class = 1|N_r, N_c, t, a, p]

  return(distr)

}

#' generate rater parameters
#' @param ratings Long form data frame with subject_id, binary
#' rating, and rater_id (a `ratings` data frame). See the `as_ratings()` function.
#' @param params A named list with t, a, and p estimates
#' @return a data frame with one row per rater giving t,a,p statistics for each
#' @export
rater_stats <- function(ratings, params){

  # check the format
  ratings <- verify_ratings(ratings)

  # associate each subject with rating counts N_r (number of raters) and
  # N_c (number of raters who rated class 1)
  count_index <- as_count_index(ratings)

  # summarize this by omiting the subject ID and counting up how many times
  # each combination of (N_r, N_c) occurs
  # the function as_counts(ratings) does this directly, but the code below
  # avoids extra computation
  counts <- count_index |>
    count(N_r, N_c, name = "n")

  # get the probability of Class 1 for each subject given the parameters
  prob_t_i <- prob_t(counts, params)

  # generate the individual rater parameters. The formulas come from the
  # confusion matrix in chapter 1, table 9. Here, tpr = true positive
  # rate, top left in the matrix. The idea is to use the information
  # to solve for a_j and p_j, where the truth class 1 probability t_i
  # has been induced from the three parameter estimate. I use primes
  # instead of bars for the complement for ease of notation, e.g.
  # t' = 1-t, a' = 1-a, p' = 1-p.

  rater_stats <- count_index |>
    left_join(prob_t_i |> select(N_r, N_c, t_i = t_u)) |>
    select(subject_id, t_i) |> # we now have t_i for each subject
    left_join(ratings) |>  # add that to each row of the long data
    # mutate(T_i = round(t + .5)) |> # this makes a_j = C1
    group_by(rater_id) |>
    summarize(c1 = mean(rating),
              tpr  = mean(t_i*rating),         # ta + ta'p
              tnr  = mean((1-t_i)*(1-rating)), # t'a + t'a'p'
              fpr  = mean((1-t_i)*rating),     # t'a'p
              fnr  = mean(t_i*(1-rating)),     # ta'p'
              a_j   = tpr/params$t - fpr/(1-params$t), # solve for a
              p_j   = fpr/(1-params$t)/(1-a_j),        # solve for p
              #a0   = tnr/(1-params$t) - fnr/params$t, # gives same answer
              #t_avg = mean(t_i),
              bias = p_j - params$t) |>
    # make sure params are in [0,1]
    mutate(a_j = if_else(a_j < 0, 0, if_else(a_j > 1, 1, a_j)),
           p_j = if_else(p_j < 0, 0, if_else(p_j > 1, 1, p_j)))

  return(rater_stats)

}

#' log likelihood by rating
#' @description Given a data frame of binary ratings and t-a-p parameters that
#' may include individual rater and subject parameters, calculate the
#' ordered log likelihood for the rating set.
#' @param param_ratings A data frame with binary subject_id, rating, and a t, a, and p
#' parameter for the rating.
#' @param bits A logical, defaulting to TRUE, which indicates whether to use
#' log base 1/2 to produce informational bits as the units for the log likelihood.
#' @param summarize A logical, defaulting to TRUE, which indicates whether to
#' return the sum of the log likelihood using the ratings provided
#' @return The ratings dataframe with two new columns, C1 and C0, for the
#' log likelihood of a 1 rating or 0 rating, respectively.
#' @export
rating_log_likelihood <- function(param_ratings,  bits = TRUE, summarize = FALSE){

  param_ratings <- param_ratings |>
    mutate(C1 = log(t*(a+(1-a)*p) + (1-t)*(1-a)*p),
           C0 = log((1-t)*(a +(1-a)*(1-p)) + t*(1-a)*(1-p)) )

  if(bits){
    param_ratings <- param_ratings |>
      mutate(C1 = -C1/log(2),
             C0 = -C0/log(2))
  }

  if(summarize){
    return(bits_per_rating(param_ratings))
  }

  return(param_ratings)
}

#' bits per rating
#' @param ll_ratings a ratings data frame augmented with C0 and C1, the
#' likelihood of 0 and 1 ratings. This is the output of `rating_log_likelihood()`
#' @return the average bits per rating over the data set
#' @export
bits_per_rating <- function(ll_ratings){
  ll_ratings %>%
    summarize( sum(C1*rating + C0*(1-rating)) / nrow(.) ) |>
    pull()
}

#' fit a hierarchical t-a-p model to ratings data
#' @param ratings data frame with columns subject_id, (binary) rating, and rater_id
#' @param n_sim number of simulations to run, defaulting to 1000
#' @param params initial values for the t-a-p parameters, or NULL (default) to generate automatacally.
#' Either a named list or a data frame with columns t, a, and p with a single row, or a vector of length 3
#' in that order.
#' @param mcmc logical, whether to use MCMC to estimate the parameters,
#' defaulting to FALSE
#' @param mcmc_chains number of MCMC chains to run, defaulting to 3
#' @param mcmc_iter number of MCMC iterations per chain, defaulting to 1000
#' @param mcmc_warmup number of MCMC burn-in iterations, defaulting to 300
#' @param mcmc_cores number of cores to use for MCMC, defaulting to 3
#' @param mcmc_strong_priors use the priors from the t-a-p estimates, defaulting
#' to TRUE
#' @return A list with the following components: a data frame param_ratings,
#' with each row
#' containing likelihood information and estimates for t, a, and p.
#' @export

hierarchical_tap_model <- function(ratings,
                                   n_sim = 1000,
                                   params = NULL,
                                   mcmc = FALSE,
                                   mcmc_chains = 3,
                                   mcmc_iter = 1000,
                                   mcmc_warmup = 300,
                                   mcmc_cores = 3,
                                   mcmc_strong_priors = TRUE){

  #' *check data and set things up*  ###########################################
  ratings <- tapModel::verify_ratings(ratings)

  N_ratings <- nrow(ratings)

  # summary of ratings by combination (R, k)
  counts <- tapModel::as_counts(ratings)

  #' *closed-form analysis*  ###################################################
  # get average t-a-p parameter estimates
  if(is.null(params)){
    params <- tapModel::iterative_optim(counts)
  }

  # subject index with truth values
  subject_index <- tapModel::as_count_index(ratings) |>
    left_join(tapModel::prob_t(counts, params))

  # compute the rater stats a_j and p_j
  a_j <- tapModel::rater_stats(ratings, params)

  # put all this into one data frame
  param_ratings <- ratings |>
    left_join(subject_index) |>
    left_join(a_j) |>
    select(subject_id, rating, rater_id, t = t_u, a = a_j, p = p_j)

  # compute ll for avg params using the probabilistic t_i and average a, p
  param_tap_ratings <- param_ratings |>
    mutate(a = params$a, p = params$p)

  ll_tap_ratings     <- tapModel::rating_log_likelihood(param_tap_ratings)
  tap_ratings_bits   <- tapModel::bits_per_rating(ll_tap_ratings) # bits per rating from data, using avg params

  # compute ll for the hierarchical ratings (i.e. random effects)
  ll_re_ratings <- tapModel::rating_log_likelihood(param_ratings)
  re_ratings_bits <- tapModel::bits_per_rating(ll_re_ratings) # bits per rating from data, using hierarchical params

  #' *MCMC analysis*  ###########################################################
  if(mcmc){
    library(cmdstanr)

    # The first version uses what we know about the individual parameters to
    # inform the mcmc analysis by providing initial values and setting strong
    # priors around the estimates.
    if(mcmc_strong_priors){
      compiled_model <- cmdstanr::cmdstan_model("code/t_i-a_j-p_j init.stan")

      init_values <- function() {
        list(
          t = subject_index$t_u,  # Vector of t_i estimates
          a = a_j$a_j,  # Vector of a_j estimates
          p = a_j$p_j  # Vector of p_j estimates
        )
      }

      fitted_model <- compiled_model$sample(
        data = list(
          N = nrow(param_ratings),
          S = n_distinct(param_ratings$subject_id),
          R = n_distinct(param_ratings$rater_id),
          rating = param_ratings$rating,
          subject_index = param_ratings$subject_id,
          rater_index = param_ratings$rater_id,
          t_est = subject_index$t_u,
          a_est = a_j$a_j,
          p_est = a_j$p_j),
        init = init_values, # must be a function that returns a list
        seed = 123,
        chains = mcmc_chains,
        parallel_chains = mcmc_cores,
        iter_warmup = mcmc_warmup,
        refresh = 1000)

      mcmc_means <- tapModel::get_tap_stats(fitted_model$draws()) |>
        filter(var != "lp__") |>
        select(var, mode1) |> # could use mode instead of average
        separate(var, into = c("var", "id"), sep = "\\[|\\]") |>
        spread(var, mode1) |>
        mutate(id = as.integer(id)) |>
        arrange(id)

      param_ratings_mcmc <- ratings |>
        select(subject_id, rater_id, rating) |>
        # add the t stats per subject
        left_join(mcmc_means |> select(id, t), by = c("subject_id" = "id")) |>
        # add the a, p stats per rater
        left_join(mcmc_means |> select(id, a, p),  by = c("rater_id" = "id")) |>
        select(subject_id, rating, rater_id, t, a , p)

    } else {
      compiled_model <- cmdstanr::cmdstan_model("code/@t_i-a_j-p_j.stan")

      init_values <- function() {
        list(
          a = a_j$a_j,  # Vector of a_j estimates
          p = a_j$p_j  # Vector of p_j estimates
        )
      }

      fitted_model <- compiled_model$sample(
        data = list(
          N = nrow(param_ratings),
          S = n_distinct(param_ratings$subject_id),
          R = n_distinct(param_ratings$rater_id),
          rating = param_ratings$rating,
          subject_index = param_ratings$subject_id,
          rater_index = param_ratings$rater_id,
          t = param_ratings$t),
        init = init_values, # must be a function that returns a list
        seed = 123,
        chains = mcmc_chains,
        parallel_chains = mcmc_cores,
        iter_warmup = mcmc_warmup,
        refresh = 1000)

      mcmc_means <- tapModel::get_tap_stats(fitted_model$draws()) |>
        filter(var != "lp__") |>
        select(var, avg) |> # could use mode instead of average
        separate(var, into = c("var", "id"), sep = "\\[|\\]") |>
        spread(var, avg) |>
        mutate(id = as.integer(id)) |>
        arrange(id)

      param_ratings_mcmc <- ratings |>
        select(subject_id, rater_id, rating) |>
        # add the t stats per subject
        left_join(subject_index |> select(subject_id, t = t_u), by = c("subject_id")) |>
        # add the a, p stats per rater
        left_join(mcmc_means |> select(id, a, p),  by = c("rater_id" = "id")) |>
        select(subject_id, rating, rater_id, t, a , p)
    }


    ll_mcmc_ratings <- tapModel::rating_log_likelihood(param_ratings_mcmc)
    mcmc_ratings_bits <- tapModel::bits_per_rating(ll_mcmc_ratings)
  }

  #' *simulation*  ###########################################################
  # create the plot
  results <- data.frame(sim = 1:N, ll_re = rep(NA, N), ll_tap = rep(NA,N))

  for(sim in 1:n_sim){
    # simulate ratins for the average tap model and the hierarchical model
    sim_ratings <- param_ratings  |>
      mutate(rating = rbern(N_ratings, prob = t*a + (1-a)*p), # using random effects
             tap_rating = rbern(N_ratings, prob = t*params$a + (1-params$a)*params$p)) # avg params

    # this is the bits_per_rating() function modified to use the simulated ratings
    results$ll_re[sim] <- sum(ll_re_ratings$C1*sim_ratings$rating +
                                ll_re_ratings$C0*(1-sim_ratings$rating))/N_ratings

    results$ll_tap[sim] <- sum(ll_tap_ratings$C1*sim_ratings$tap_rating +
                                 ll_tap_ratings$C0*(1-sim_ratings$tap_rating))/N_ratings
  }

  # simulate the mcmc if it's here
  if(mcmc){
    mcmc_results <- data.frame(sim = 1:N, ll_mcmc = rep(NA, N))

    for(sim in 1:n_sim){
      # simulate ratins for the average tap model and the hierarchical model
      sim_ratings <- ll_mcmc_ratings  |>
        mutate(rating = rbern(N_ratings, prob = t*a + (1-a)*p))

      # this is the bits_per_rating() function modified to use the simulated ratings
      mcmc_results$ll_mcmc[sim] <- bits_per_rating(sim_ratings)

    }

    results <- results |>
      left_join( mcmc_results)
  }

  #' *outputs*  ###########################################################
  my_plot <- results |>
    gather(key = "type", value = "ll", -sim) |>
    ggplot(aes(x=ll, fill = type, color = type, group = type)) +
    geom_density(alpha = .5) +
    geom_vline(xintercept = tap_ratings_bits, color = "blue", alpha = .5, linetype = "dashed") +
    geom_vline(xintercept = re_ratings_bits, color = "green", alpha = .5, linetype = "dashed") +
    theme_bw() +
    xlim(0,1) +
    xlab("Log Likelihood (bits/rating)")

  my_list <- list(param_ratings = param_ratings,
                  ll_re = ll_re_ratings,
                  ll_tap = ll_tap_ratings,
                  mcmc_model = NULL,
                  ll_mcmc = NULL,
                  plot = NULL)

  if(mcmc) {
    my_plot <- my_plot + geom_vline(xintercept = mcmc_ratings_bits, color = "red", alpha = .5, linetype = "dashed")

    my_list$mcmc_model <- fitted_model
    my_list$ll_mcmc <- ll_mcmc_ratings
  }

  my_list$plot <- my_plot

  return(my_list)
}

