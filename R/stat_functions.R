#' Fleiss kappa
#' @description Function to calculate Fleiss' kappa for binary data
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters for each subject and number of raters who rated class 1.
#' @return a data frame with one row and columns for parameters: t, a, p, log likelihood: ll,
#' and a true/false flag for degenerate solutions.
#' @export
fleiss_kappa <- function(counts) {

  matches <- function(N_c, N_r) {
    N_c_c  = N_r - N_c
    N_c*(N_c-1)/2 + N_c_c*(N_c_c-1)/2
  }

  # Number of cases (rows)
  n <- nrow(counts)
  # Number of raters (columns)
  k <- ncol(data)

  # Calculate the proportion of raters that assigned each category (0 or 1) to each case
  stats <- counts |>
    mutate(p_ij = N_c / N_r,
           q_ij = 1 - p_ij,
           m = matches(N_c, N_r),
           p = m / matches(N_r,N_r)) |>
    summarize(P_bar= mean(p),
              c = sum(N_c)/sum(N_r),
              P_e_bar= c^2 + (1-c)^2)

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
    #rowwise() %>% # because log_prob isn't vectorized
    # note that the optimizer MINIMIZES
    summarize(ll = negative_log_prob(N_r, N_c, t, a, p)) %>%
    mutate(ll = if_else(is.infinite(ll), 1e6, ll)) %>%
    pull()
}

#' Fit the t-a-p model to binary ratings
#' @param counts a data frame with columns N_r and N_c, representing number of
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

#' Iterative optimization for the t-a-p model
#' @description Start with the middle case and iterate to find the best solution
#' @param counts a data frame with columns N_r and N_c, representing number of
#' raters and counts of raters who rated class 1.
#' @param iterations number of iterations to run
#' @return a data frame with columns for parameters: t, a, p, and log likelihood of the solution
#' @export
iterative_optim <- function(counts, iterations = 1){
  # start with the middle case
  params <- find_solution(counts, c(.5,.5,.5))

  if(is_degenerate(params)) {
    fence_params <- picket_fence(counts)
    params <- find_solution(counts, fence_params)
  }

  return(data.frame( t = params[1], a = params[2], p = params[3], ll = params[4]))

}


#' Generate t-a-p statistics from counts
#' @param ratings a data frame that must include columns N_r and N_c, the number
#' of raters and ratings of class 1 for each subject
#' @param compiled_model a compiled stan model suitable for this data
#' @return a posterior stan \code{draws} object
#' @details The model must be able to accommodate varying numbers of raters per
#' subject
#' @export
fit_tap_model <- function(counts, compiled_model) {

  N <- nrow(counts) # number of cases/subjects
  R <- counts$N_r   # ratings for each subject
  count <- counts$N_c # number of positive ratings

  fitted_model <- compiled_model$sample(
    data = list(
      N = N,
      R = R,
      count = count),
    seed = 123,
    chains = 3,
    parallel_chains = 3,
    refresh = 1000)

  return(fitted_model$draws())

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
#' @param compiled_model The stan model for t-a0,a1-p_i
#' @param model_data A dataframe with columns: ID n_ratings, and sum_ratings.
#' ID is the subject identifier, n_ratings is the number of binary ratings,
#' and sum_ratings is the sum of the binary ratings.
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
                            mode2 = NA) |>
    filter(str_detect(var,"^t|^a|^p"))

  # get parameter averages
  for(i in 1:nrow(model_means)){
    j <- model_means$position[i]

    model_means$avg[i] <- model_draws[,,j] %>% mean()
    model_means$median[i] <- model_draws[,,j] %>% median()
    model_means$p05[i] <- quantile(model_draws[,,j], 0.05)
    model_means$p25[i] <- quantile(model_draws[,,j], 0.25)
    model_means$p75[i] <- quantile(model_draws[,,j], 0.75)
    model_means$p95[i] <- quantile(model_draws[,,j], 0.95)

    # get modes
    model_means$mode1[i] <- Modes(model_draws[,,j])$modes[1]

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
#' raters and counts of raters who rated class 1.
#' @param iterations number of iterations to run. Currently this doesn't do
#' anything. It's there for future development.
#' @return a data frame with columns for parameters: t, a, p, and log likelihood
#' of the solution, and a flag for degeneracy
#' @export
iterative_optim <- function(counts, iterations = 1){
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
#' @param ratings a data frame in long format with columns SubjectID__ and rating
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
      group_by(SubjectID__) |>
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


