#' Subject Calibration Plot
#' @description For each subject compute the fraction of Class 1 ratings and
#' compare this to the the expected fraction from the t-a-p model parameters
#' @param rating_params A rating_params dataframe
#' @param n_sims Number of simulations to run to estimate the modeled
#' distribution from the probabilities. Defaults to 30.
#' @return A ggplot object with the calibration plot and error statistics
#' @details
#' The probabilities for each rating can be averaged to find the *expected* number
#' of Class 1 ratings per subject, but it's better to get a distribution of
#' that variation to use in the plot. That's what the n_sim parameter does.
#' @export
subject_calibration <- function(rating_params, n_sims = 30){

  # verify the input
  verify_ratings(rating_params)

  # get rid of extra columns
  rating_params <- rating_params |>
    select(subject_id, rating, rater_id, t, a, p)

  # simulate counts for each subject
  sim_counts_list <- vector("list", n_sims)

  for (i in seq_len(n_sims)) {
    sim_ratings <- tapModel::generate_ti_aj_pj_ratings(rating_params, use_avg_t = TRUE)
    sim_counts_list[[i]] <- as_counts(sim_ratings)
  }

  # Combine all into a single data frame
  sim_counts <- bind_rows(sim_counts_list)

  # Count up unique cases of N_r and N_c in the simulated data
  sim_counts <- sim_counts |>
    filter(!is.na(N_r)) |>
    group_by(N_r, N_c) |>
    summarize(modeled = sum(n)) |>
    ungroup()

  # Count up unique cases of N_r and N_c for the observed data
  observed_counts <- rating_params |>
    as_counts() |>
    rename(observed = n)

  # match up cases and insert zeros for missing data
  comparison <- sim_counts |>
    full_join(observed_counts) |>
    replace_na(list(observed = 0, modeled = 0)) |>
    mutate(c = round(N_c/N_r, 2)) |>
    group_by(c) |>
    summarize(observed = sum(observed),
              modeled = sum(modeled)) |>
    arrange(c) |>
    mutate(modeled = modeled/sum(modeled),
           observed = observed/sum(observed))

  # find sum abs error
  MAE <- sum(abs(
    (comparison$modeled - comparison$observed) *
      (comparison$c - lag(comparison$c))), na.rm = TRUE)

  RMSE <- sqrt(sum((
    (comparison$modeled - comparison$observed)^2 *
      (comparison$c - lag(comparison$c))), na.rm = TRUE))

  BPR = bits_per_rating(rating_params)

  my_title <- str_c("(t,a,p)=(", round(mean(rating_params$t),2),
                    ",", round(mean(rating_params$a),2),
                    ",", round(mean(rating_params$p),2),")")

  my_subtitle <- str_c("MAE=",round(MAE, 3),
                       "  RMSE=", round(RMSE, 3),
                       "  LL=", round(BPR, 3))


  # markers for means
  mean_1 <- with(rating_params, {mean(a + (1-a)*p )})
  mean_0 <- with(rating_params, {mean((1-a)*p   )})

  # Merge observed and simulated frequencies
  my_plot <- comparison |>
    select(c, observed, modeled) |>
    gather(key = "type", value = "value", -c) |>
    ggplot(aes(x = c, y = value, color = type)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c("orange","steelblue")) +
    theme_bw()  +
    labs(title = my_title,
         subtitle = my_subtitle,
         x = "Fraction of Class 1 Ratings",
         y = "Frequency")  +
    geom_vline(xintercept = mean_1, color = "gold", linetype = "dashed") +
    geom_vline(xintercept = mean_0, color = "gold", linetype = "dashed")

  return(my_plot)

}

#' Rater Calibration Plot
#' @description For each rater compute the fraction of Class 1 ratings and
#' compare this to the the expected fraction from the t-a-p model parameters
#' @param rating_params A rating_params dataframe
#' @param bins Number of bins to use for the calibration plot, or zero for no binning
#' @param labels For bins = 0, plot the rater_id labels instead of points. This might
#' be useful for identifying outliers. Defaults to FALSE.
#' @details The predicted class 1 probabilities are ta + (1 - a)p, and these
#' are optionally binned into equal-sized groups. Then the average predicted value and
#' average observed value are calculated, and plotted. The observed values
#' are on the x-axis, so it's easy to interpret dots above the 1-1 line as
#' over-prediction, and dots below the 1-1 line as under-prediction. If no binning
#' is requested, each rater is a point on the plot, or if labels = TRUE,
#' the rater_id.
#' @return A ggplot object with the calibration plot and error statistics
#' @export
rater_calibration <- function(rating_params, bins = 10, labels = FALSE){

  # verify the input
  verify_ratings(rating_params)

  # get rid of extra columns
  rating_params <- rating_params |>
    select(subject_id, rating, rater_id, t, a, p)

  # add a pr_c1 column
  rating_params <- rating_params |>
    mutate(pr_c1 = t*a + (1 - a) * p)

  comparison <- rating_params |>
    group_by(rater_id) |>
    summarize(modeled = mean(pr_c1),
              observed = mean(rating))

  if(bins > 0){
    comparison <- comparison |>
      mutate(bin = ntile(observed, bins)) |>
      group_by(bin) |>
      summarize(rater_id = first(rater_id), # this is a hack, not used in practice
                modeled = mean(modeled),
                observed = mean(observed))
  }

  # find sum abs error
  MAE <- with(comparison,
              { mean(abs(modeled - observed)) }
  )

  RMSE <- with(comparison,
               {sqrt( mean( (modeled - observed)^2)  )}
  )

  BPR = bits_per_rating(rating_params)

  my_title <- str_c("(t,a,p)=(", round(mean(rating_params$t),2),
                    ",", round(mean(rating_params$a),2),
                    ",", round(mean(rating_params$p),2),")")

  my_subtitle <- str_c("MAE=",round(MAE, 3),
                       "  RMSE=", round(RMSE, 3),
                       "  LL=", round(BPR, 3))


  # plot observed and simulated frequencies
  my_plot <- comparison |>
    select(observed, modeled, rater_id) |>
    ggplot(aes(y = modeled, x = observed, label = rater_id)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_bw() +
    labs(title = my_title,
         subtitle = my_subtitle,
         y = "Modeled Fraction of Class 1",
         x = "Observed Fraction of Class 1")

  if(bins > 0){
    my_plot <- my_plot +
      geom_line(linewidth = 0) +
      geom_point(color = "steelblue")
  } else{
    # add points to the plot or labels as requested
    if (labels == TRUE){
      my_plot <- my_plot +
        geom_text(color = "steelblue")
    } else {
      my_plot <- my_plot +
        geom_point(alpha = .5, color = "steelblue")
    }
  }

  return(my_plot)

}

#' Rating Calibration Plot
#' @description Ratings are sorted by pr_c1 and the cumulative sums of
#' observed vs modeled plotted with comparison to the 1-1 line.
#' @param rating_params A rating_params dataframe
#' @param bins Number of bins to use for the calibration plot
#' @details The predicted class 1 probabilities are ta + (1 - a)p, and these
#' are binned into equal-sized groups. Then the average predicted value and
#' average observed value are calculated, and plotted. The observed values
#' are on the x-axis, so it's easy to interpret dots above the 1-1 line as
#' over-prediction, and dots below the 1-1 line as under-prediction.
#' @return A ggplot object with the calibration plot and error statistics
#' @export
rating_calibration <- function(rating_params, bins = 10){

  # verify the input
  verify_ratings(rating_params)

  # get rid of extra columns
  rating_params <- rating_params |>
    select(subject_id, rating, rater_id, t, a, p)

  # add a pr_c1 column
  rating_params <- rating_params |>
    mutate(pr_c1 = t*a + (1 - a) * p,
           pr_bin = ntile(pr_c1, bins))

  comparison <- rating_params |>
    group_by(pr_bin) |>
    summarize(observed = mean(rating),
              modeled = mean(pr_c1))

  # find sum abs error
  MAE <- with(comparison,
              { mean(abs(modeled - observed)) }
  )

  RMSE <- with(comparison,
               {sqrt( mean( (modeled - observed)^2)  )}
  )

  BPR = bits_per_rating(rating_params)

  my_title <- str_c("(t,a,p)=(", round(mean(rating_params$t),2),
                    ",", round(mean(rating_params$a),2),
                    ",", round(mean(rating_params$p),2),")")

  my_subtitle <- str_c("MAE=",round(MAE, 3),
                       "  RMSE=", round(RMSE, 3),
                       "  LL=", round(BPR, 3))

  # Merge observed and simulated frequencies
  my_plot <- comparison |>
    select(observed, modeled) |>
    ggplot(aes(x = modeled, y = observed)) +
    geom_line(color = "steelblue", linewidth = 0) +
    geom_point(size = 1.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_bw() +
    labs(title = my_title,
         subtitle = my_subtitle,
         x = "Modeled Fraction of Class 1",
         y = "Observed Fraction of Class 1")

  return(my_plot)

}

#' Rater covariance
#' @param rating_params A rating_params data frame
#' @param max_raters The max number of unique raters to sample, defaulting to 100
#' @return A list with individual `raters` results, including actual and predicted correlation
#' @details
#' This function uses a formula similar to that in the Appendix to estimate the
#' covariance between ratings based on individual parameters as a comparison point to the
#' actual correlations of ratings over subjects.
#'
#' With too many raters, this calculation will bog down. That's why the default
#' is to sample raters if there are more than 100.
#'
#' @export
rater_cov <- function(rating_params, max_raters = 100){

  unique_raters <- unique(rating_params$rater_id)

  if(length(unique_raters) > max_raters) {
    unique_raters <- sample(unique_raters, max_raters)
    rating_params <- rating_params |> dplyr::filter(rater_id %in% unique_raters)
    message("Too many raters, so sampling.")
  }

  rating_params |>
    select(subject_id, rater_id, rating, t, a, p) %>%
    inner_join(., ., by = "subject_id", suffix = c("1","2")) |>
    filter(rater_id1 < rater_id2) |>
    group_by(rater_id1, rater_id2) |>
    summarize(
      n_overlap = n(),
      t = mean(t1),
      a1 = first(a1),
      a2 = first(a2),
      c1 = mean(rating1),
      c2 = mean(rating2),
      # empirical centered dot and covariance
      dot_centered = sum( (rating1 - c1) * (rating2 - c2) ),
      cov_emp = dot_centered / n_overlap,
      # model prediction for covariance
      cov_pred = a1 * a2 * t * (1 - t),
      .groups = "drop"
    ) |>
    filter(n_overlap > 0) |>
    transmute(
      rater1 = rater_id1, rater2 = rater_id2,
      cov_emp, cov_pred,
      diff = cov_emp - cov_pred
    )
}

#' quantiles for expected covariance between raters
#' @param rating_params A rating_params data frame, usually from a fitted model
#' @param n_sim How many simulations to run, defaulting to 10
#' @param quantiles What quantiles to return, defaulting to c(.02, .98)
#' @return A named vector with the quantiles on the difference between expected
#' and actual correlation.
#' @export
rater_cov_quantile <- function(rating_params, n_sim = 10, quantiles = c(.02,.98)){

  out <- data.frame()

  for(i in 1:n_sim){
    rnd_cov <- generate_ti_aj_pj_ratings(rating_params) |>
      rater_cov()

    out <- rbind(out, rnd_cov)
  }

  result <- quantile(out$diff, quantiles)

  return(result)
}

#' Plot rater correlation histogram
#' @param rater_cov The output from `rater_cov()`
#' @param range The output from `rater_cov_quantile()`
#' @param bins how many bins for the histogram, defaulting to 50
#' @return a ggplot
#' @export
plot_rater_cov <- function(rater_cov, range, bins = 50){

  my_plot <- rater_cov |>
    ggplot(aes(x = diff)) +
    geom_histogram(color = "white", fill = "steelblue", bins = bins) +
    theme_bw() +


    xlab("Actual - Predicted Covariance") +
    geom_vline(xintercept = range[1], color = "red", linetype = "dashed") +
    geom_vline(xintercept = range[2], color = "red", linetype = "dashed")

  return(my_plot)
}

########################### Old Stuff ##################

#' fit a hierarchical t-a-p model to ratings data
#' @param rating_params_1 output of either `fit_ratings()` or `fit_ratings_mcmc()`
#' @param rating_params_2 output of either `fit_ratings()` or `fit_ratings_mcmc()`,
#' or NULL to use the average t-a-p model
#' @param model_names a character vector of length 2 with the names of the models
#' @param n_sim number of simulations to run, defaulting to 1000
#' @param vars Some subset of "t", "a", "p", "ll" to include in the output plot
#' @return A list with the simulation data (sim_data) and a plot (plot)
#' comparing model fit using log likelihood in bits
#' @export
model_fit_comparison <- function(rating_params_1, rating_params_2 = NULL,
                                 model_names = c("model 1", "model 2"),
                                 n_sim = 200,
                                 vars = c("t","a", "p","c","ll")){

  # utility function to create parameter averages
  tap_avg <- function(rating_params){
    # do we have binary values?
    if("T_i" %in% colnames(rating_params)){
      t <- rating_params |>
        distinct(subject_id, T_i) |>
        summarize(t = mean(T_i)) |>
        pull()
      a <- mean(rating_params$A_ij)
      p <- mean(rating_params$P_ij)
      c <- mean(rating_params$rating)
      return(c(t = t, a = a, p = p, c = c))
    }
    # else we have only the avg parameters t_i etc
    a <- mean(rating_params$a)
    p <- mean(rating_params$p)
    t <- rating_params |>
      distinct(subject_id, t) |>
      summarize(t = mean(t)) |>
      pull()
    c <- mean(rating_params$rating)
    return(c(t = t, a = a, p = p, c = c))
  }

  # create the second model from average parameters if required
  if(is.null(rating_params_2)){
    rating_params_2 <- rating_params_1 |>
      select(subject_id, rating, rater_id, t, a, p) |>
      mutate(#t = mean(t),
             a = mean(a),
             p = mean(p))
  }

  # original parameter values for markers on the plot
  original_1 <- data.frame(model = model_names[1],ll=NA,t=NA,a=NA,p=NA,c=NA)
  original_1$ll   <- tapModel::bits_per_rating(rating_params_1)
  original_1[1,3:6] <- tap_avg(rating_params_1)

  original_2 <- data.frame(model = model_names[2],ll=NA,t=NA,a=NA,p=NA,c=NA)
  original_2$ll   <- tapModel::bits_per_rating(rating_params_2)
  original_2[1,3:6] <- tap_avg(rating_params_2)

  original <- rbind(original_1, original_2) |>
    gather(var, value, -model)

  # simulation  ###########################################################
  results_1 <- data.frame(model = model_names[1],
                          sim = 1:n_sim,
                          ll = rep(NA, n_sim),
                          t = rep(NA, n_sim),
                          a = rep(NA, n_sim),
                          p = rep(NA, n_sim),
                          c = rep(NA, n_sim))

  results_2 <- data.frame(model = model_names[2],
                          sim = 1:n_sim,
                          ll = rep(NA, n_sim),
                          t = rep(NA, n_sim),
                          a = rep(NA, n_sim),
                          p = rep(NA, n_sim),
                          c = rep(NA, n_sim))

  for(sim in 1:n_sim){

    ratings_1 <- tapModel::generate_ti_aj_pj_ratings(rating_params = rating_params_1, use_avg_t = FALSE)

    ratings_2 <- tapModel::generate_ti_aj_pj_ratings(rating_params = rating_params_2, use_avg_t = FALSE)

    results_1[sim, 4:7] <- tap_avg(ratings_1)
    results_2[sim, 4:7] <- tap_avg(ratings_2)

    results_1$ll[sim] <- tapModel::bits_per_rating(ratings_1)
    results_2$ll[sim] <- tapModel::bits_per_rating(ratings_2)
  }

  # change the names
  results <- rbind(results_1, results_2) |>
             gather(var, value, -model, -sim)

  rm(results_1, results_2)

  ## xplot  ###########################################################

  my_title <- str_c(model_names[1]," = ", round(original_1$ll,2), " ",
                    model_names[2]," = ", round(original_2$ll,2))


  my_plot <- results |>
    filter(var %in% vars) |>
    ggplot(aes(x=value, fill = model, color = model, group = model)) +
    geom_density(alpha = .5) +
    geom_vline(aes(xintercept = value,color = model), linetype = "dashed",
               data = original |> filter(var %in% vars)) +
    theme_bw() +
    xlim(0,1) +
    xlab("Log Likelihood (bits/rating)") +
    ggtitle(my_title) +
    facet_wrap(~var, scales = "free")

  return(list(sim_data = results, plot = my_plot))
}


#' simulate data to compare to observed
#' @param n_iter number of iterations to simulate, defaults to 100
#' @param observed_data data frame of observed data, which must have a fixed number of raters,
#' and columns RaterID__ (integer index) and rating (0 or 1)
#' @param params list of parameters for the simulation: n_subjects, n_raters, t, a0, a1, p0, p1
#' @param output If "data" a dataframe with the comparison data, or if "plot" a ggplot object (default)
#' @return A dataframe with the observed and simulated frequencies of ratings
#' @export
compare_simulated_to_observed <- function(n_iter = 100, observed_data, params, output = "plot") {
  # Simulate multiple datasets
  simulated_data <- map_dfr(1:n_iter, ~ {
    generate_sample_ratings(
      n_subjects = params$n_subjects,
      n_raters = params$n_raters,
      t = params$t,
      a0 = params$a0,
      a1 = params$a1,
      p0 = params$p0,
      p1 = params$p1
    ) |>
      group_by(subject_id) |>
      summarize(N_c = sum(rating), .groups = "drop") %>%
      count(N_c, name = "n") |>
      mutate(iteration = .x)
  })

  # Aggregate simulated frequencies
  simulated_frequencies <- simulated_data %>%
    group_by(N_c) %>%
    summarise(
      mean_sim_freq = mean(n),
      iqr_lower = quantile(n, 0.25),  # 25% quantile
      iqr_upper = quantile(n, 0.75),  # 75% quantile
      ci_lower = quantile(n, 0.025),  # 2.5% quantile
      ci_upper = quantile(n, 0.975),  # 97.5% quantile
      .groups = "drop"
    )

  # Aggregate observed frequencies
  observed_frequencies <- observed_data %>%
    group_by(subject_id) |>
    summarize(N_c = sum(rating), .groups = "drop") |>
    count(N_c, name = "observed")

  # find average mean error
  mean_error <- simulated_data |>
                left_join(observed_frequencies) |>
                summarize(mean_error = mean(abs(n - observed)/observed))


# Merge observed and simulated frequencies
  comparison <- left_join(observed_frequencies, simulated_frequencies, by = "N_c") %>%
    replace_na(list(mean_sim_freq = 0)) # Handle cases where frequencies don't align

  # output type -- return plot if requested
  if(output == "plot"){
    myplot <- comparison %>%
      ggplot(aes(x = N_c)) +
      geom_bar(aes(y = observed), stat = "identity", fill = "steelblue", alpha = 0.8) +
      geom_point(aes(y = mean_sim_freq), color = "black", size = 3) +
      geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper, y = mean_sim_freq),
        color = "black",
        width = 0
      ) +
      geom_errorbar(
        aes(ymin = iqr_lower, ymax = iqr_upper, y = mean_sim_freq),
        color = "black",
        width = 0,
        linewidth = 1.5
      ) +
      labs(
        title = "Observed vs. Simulated Frequencies with IQR and 95% coverage.",
        subtitle = str_c("Mean absolute relative error: ", round(mean_error, 2)),
        x = "Class 1 Count per Subject (N_c)",
        y = "Frequency"
      ) +
      theme_bw()

    return(myplot)
  }

  # otherwise return the data
  return(comparison)
}

#' compare model to observed
#' @param model_params The parameters of the model, a list with t, a0, a1, p0, p1,
#' OR a rating_params data frame with columns subject_id, rating, rater_id, t, a, p
#' @param observed_data frame of binary ratings or counts, which must either (1) have
#' columns SubjectID__and rating (0 or 1) in long format, or (2) have columns N_r,
#' N_c, and n (the count) to give the count distribution over subjects, respectively.
#' If rating_params is included as the model, then ratings can be omitted
#' @param n_sims How many sims to run to generate the modeled distribution? This
#' only gets used if rating_params are provided. For counts, exact calculations
#' are used.
#' @return A dataframe with model fit statistics
#' @details This function compares the model to observed data by aggregating the observed and
#' modeled distributions of class 1 counts by subject. It cannot accomodate individual-level
#' parameters like t_i, a_j, or p_j. Therefore, no RaterID__ is required.
#' @export
compare_model_to_observed <- function(model_params, observed_data = NULL, n_sims = 30) {

  # is the the rater_params from a hierarchical model?
  if("subject_id" %in% names(model_params)) {
    # compute Class 1 rate
    class_1 <- model_params |>
      as_counts() |>
      rename(observed = n)

    # we need to estimate the distribution of C1 ratings per subject. Since we have
    # multiple, possibly unique, raters, this is a complex calculation directly
    # from the probabilities. It's easier just to simulate it.
    sim_counts <- data.frame(N_r = NA, N_c = NA, n = NA)
    for(i in 1:n_sims){
      sim_ratings <- tapModel::generate_ti_aj_pj_ratings(rating_params = model_params, use_avg_t = TRUE)
      sim_counts <- rbind(sim_counts, as_counts(sim_ratings))

    }

    sim_counts <- sim_counts |>
      filter(!is.na(N_r)) |>
      group_by(N_r, N_c) |>
      summarize(modeled = sum(n)) |>
      ungroup()

    comparison <- sim_counts |>
      full_join(class_1) |>
      replace_na(list(observed = 0, modeled = 0)) |>
      mutate(c = round(N_c/N_r, 2)) |>
      group_by(c) |>
      summarize(observed = sum(observed),
                modeled = sum(modeled)) |>
      arrange(c) |>
      mutate(modeled = cumsum(modeled/sum(modeled)),
             observed = cumsum(observed/sum(observed)))

  } else { # it's average t-a-p params, not hierarchical

    # expand to full t-a0a1-p0p1 format and compute probabilities
    expand_params(model_params) |>
      list2env(envir = environment())

    prob1 <- a1 + (1-a1)*p1 # Pr[C = 1 | T = 1]
    prob0 <- (1-a0)*p0 # Pr[C = 1 | T = 0]


    if ("subject_id" %in% names(observed_data)) {
      # Aggregate observed frequencies if not already done
      counts <- observed_data %>%
        group_by(subject_id) |>
        summarize(N_r = n(),
                  N_c = sum(rating),
                  .groups = "drop") |>
        count(N_r, N_c, name = "observed")
    } else { # we already have the counts
      counts <- observed_data |>
        mutate(observed = n)
    }

    # convert counts to ratios
    counts <- counts |>
      group_by(N_r) |>
      mutate(observed = observed/sum(observed)) |>
      ungroup() |>
      mutate(c = N_c/N_r)

    # Aggregate simulated frequencies
      modeled_rates <- counts |>
      select(N_r, N_c) |>
      rowwise() |>
      mutate(modeled = t*dbinom(N_c, N_r, prob1) + (1-t)*dbinom(N_c, N_r, prob0) )


    # Merge observed and simulated frequencies
    comparison <- counts |>
      left_join(modeled_rates) |>
      arrange(c) |>
      mutate( modeled = modeled/sum(modeled),
              observed = observed/sum(observed),
              modeled = cumsum(modeled),
              observed = cumsum(observed) )
  }

  # find sum abs error
  MAE <- sum(abs(
    (comparison$modeled - comparison$observed) *
      (comparison$c - lag(comparison$c))), na.rm = TRUE)

  RMSE <- sqrt(sum((
    (comparison$modeled - comparison$observed)^2 *
      (comparison$c - lag(comparison$c))), na.rm = TRUE))

 # BPR = bits_per_rating(model_params)

  my_title <- str_c("MAE=",round(MAE, 3),
                    "  RMSE=", round(RMSE, 3))
                    #"  S=", round(BPR, 3))

  # Merge observed and simulated frequencies
  my_plot <- comparison |>
    select(c, observed, modeled) |>
    gather(key = "type", value = "value", -c) |>
    ggplot(aes(x = c, y = value, color = type)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c("orange","steelblue")) +
    theme_bw() +
    xlab("Fraction of Class 1") +
    ylab("Cumulative Frequency/Probability") +
    ggtitle(my_title)

  return(list(data = comparison, plot = my_plot))
}
#' recover rater parameters from simulated data
#' @param rating_params The ratings data with subject_id, rating, and rater_id
#' columns in addition to the parameter estimates. This normally comes from  a
#' solver like `fit_ratings(ratings)` or `fit_ratings_mcmc(ratings)$rating_params`
#' @param n_sim The number of simulations to run, defaulting to 10, since it can
#' be time-consuming.
#' @param method One of "model" or "sample". If the former, data sets are simulated
#' from the parameters. If the latter, data sets are sampled without replacement
#' from the original ratings at the specified rate, defaulting to .5.
#' @param sample_rate Only used when method = "sample". Specifies the sample rate
#' from the original data, defaulting to .5.
#' @return a list with plot_a = box plot of the accuracy estimates, plot_p for
#' the p_j parameters, and rater_params = a dataframe with simulation values for
#' each a_j, p_j parameter.
#' @export
estimate_rater_parameter_error <- function(rating_params, n_sim = 10, method = "model", sample_rate = .5){
  N_ratings <- nrow(rating_params)

  # save the initial params
  model_rater_params <- tapModel::pull_rating_params(rating_params)$raters

  # initialize a place to store results
  rater_results <- list()

  #### Model-based variation
  if(method == "model"){

    for(sim in 1:n_sim){

      # simulate ratings for the hierarchical model
      sim_ratings <- generate_ti_aj_pj_ratings(rating_params = rating_params)

      # estimate the t-a-p parameters and get a, p
      sim_params <- fit_ratings(sim_ratings)

      a_j <- tapModel::pull_rating_params(sim_params)$raters |>
        select(rater_id, a_sim = a, p_sim = p)

      # append a_j to the results list
      rater_results[[sim]] <- a_j
    }
  } else if(method == "sample") {

    for(sim in 1:n_sim){

      # simulate ratings for the hierarchical model
      sim_ratings <-  rating_params |>
        slice_sample(prop = sample_rate)

      # estimate the t-a-p parameters and get a, p
      sim_params <- fit_ratings(sim_ratings)

      a_j <- tapModel::pull_rating_params(sim_params)$raters |>
        select(rater_id, a_sim = a, p_sim = p)

      # append a_j to the results list
      rater_results[[sim]] <- a_j
    }

  } else {
    stop("The method must be one of 'model' or 'sample'")
  }

  # convert the list into a dataframe with column for sim number
  rater_results <- bind_rows(rater_results, .id = "sim") |>
    left_join(model_rater_params, by = "rater_id") |>
    mutate(a_error = a_sim - a,
           p_error = p_sim - p)

  my_plot_a <-  rater_results |>
    mutate(rater_id = reorder(rater_id,a)) |>
    ggplot(aes(x = a_sim, y = rater_id, group = rater_id)) +
    geom_boxplot() +
    geom_point(aes(x = a), color = "red", shape = 18, size = 3) +
    theme_bw() +
    xlab("Simulated a_j with true value marked in red") +
    xlim(0,1)

  my_plot_p <-  rater_results |>
    mutate(rater_id = reorder(rater_id,p)) |>
    ggplot(aes(x = p_sim, y = rater_id, group = rater_id)) +
    geom_boxplot() +
    geom_point(aes(x = p), color = "red", shape = 18, size = 3) +
    theme_bw() +
    xlab("Simulated p_j with true value marked in red") +
    xlim(0,1)

  return(list(plot_a = my_plot_a, plot_p = my_plot_p, rater_params = rater_results))
}

#' Generate rating samples and model fit
#' @description Range of accuracy estimates for ideal data
#' @param N_s number of subjects
#' @param N_r average number of raters per subject
#' @param params list of t-a-p parameters for the simulation
#' @param n_sim number of simulations to run, defaulting to 100
#' @param output If "data" (default) a dataframe with the comparison data, or if
#'  "plot" a ggplot object with facet histograms for each parameter
#'  @details
#'  This function uses the solver `fit_counts(counts)` to estimate the t-a-p
#'  parameters, which will combine sampling error with estimation error. In
#'  special circumstances, we can eliminate the estimation error. Use
#'  `simulate_exact_fit()` for that.
#'
#' @return A data frame with the parameter estimates for each simulation or
#' a histogram summarizing the data for each parameter, including absolute
#' bias abs(p-t).
#' @export
simulate_tap_fit <- function(N_s, N_r, params, n_sim = 100, output = "data") {
  # initialize a place to store results
  results <- data.frame(sim = 1:n_sim, t = NA, a = NA, p = NA)

  for(sim in 1:n_sim){

    # simulate ratings for the hierarchical model
    sim_ratings <- tapModel::generate_sample_ratings(N_s, N_r, params)

    # estimate the average t-a-p parameters
    sim_params <- tapModel::fit_counts(as_counts(sim_ratings))

    # append a_j to the results list
    results$t[sim] <- sim_params$t
    results$a[sim] <- sim_params$a
    results$p[sim] <- sim_params$p
  }

  if(output == "plot"){
    params$abs_bias = abs(params$p - params$t)

    # plot the results if requested
    my_plot <- results %>%
      mutate(abs_bias = abs(p - t)) |>
      gather(param, value, -sim) |>
      ggplot(aes(x = value)) +
      geom_histogram(fill = "steelblue", color = "white") +
      geom_vline(aes(xintercept = value), color = "red", data = params |> gather(param, value)) +
      facet_wrap(~param) +
      xlim(0,1) +
      theme_minimal()

    return(my_plot)
  }

  return(results)
}

#' Dunning-Krueger Threshold
#' @description Estimates the Dunning-Kruger horizon as
#' a percentile of the estimate for accuracy when it is actually
#' zero.
#' @param N_s number of subjects
#' @param N_r average number of raters per subject
#' @param a The "null value" of a to test against, usually zero (default)
#' @param tp The value of t and p, which must be equal. This is used for the
#' simulations as well as the exact coefficient model when use_fleiss = FALSE.
#' @param use_fleiss If TRUE, use the Fleiss kappa to estimate accuracy, otherwise
#' the exact formula is used. Default is TRUE. There are limitations to the
#' exact formula.
#' @param n_sim number of simulations to run, defaulting to 100
#' @return A list with 50%, 90%, 95%, and 98% quantiles for a.
#' @details
#' The function uses the `simulate_exact_fit()` function to estimate the
#'
#' @export
dk_horizon <- function(N_s, N_r, a = 0, tp = .5, use_fleiss = TRUE, n_sim = 500){

  # initialize a place to store results
  results <- simulate_exact_fit(N_s, N_r, a_vals = a,
                                tp, use_fleiss, n_sim, output = "data")

  # return the quantiles
  return(   c(quantile(results$a_sim, .5),
              quantile(results$a_sim, .75),
              quantile(results$a_sim, .9),
              quantile(results$a_sim, .95),
              quantile(results$a_sim, .98)))
}

#' Simulate sampling error with exact formulas
#' @description Range of accuracy estimates for data simulated with various
#' accuracies and the other parameters fixed. This function uses the exact
#' formulas, but is limited to the unbiased rater case (t = p). This function
#' returns a either the data or a plot.
#' @param N_s_vals A vector of one or more values for N_s, the number of subjects
#' @param N_r_vals A vector of one or more values for N_r, the average number of raters per subject
#' @param a_vals A vector of values to use for accuracy, defaults to seq(0,1,.05)
#' @param tp The value of t and p, which must be equal. This is used for the
#' simulations as well as the exact coefficient model when use_fleiss = FALSE.
#' @param use_fleiss If TRUE, use the Fleiss kappa to estimate accuracy, otherwise
#' the exact formula is used. Default is TRUE. There are limitations to the
#' exact formula.
#' @param n_sim number of simulations to run on each combination of N_s and
#' N_r, defaulting to 100
#' @param output If "data" (default) a dataframe with the comparison data, or if
#' "plot" a ggplot object with facet histograms for each parameter
#' @details This function uses the exact formulas for the t-a-p parameters using
#' one of two methods. The default is to use the Fleiss kappa to calculate accuracy,
#' which assumes that t=p, but not a particular value. If you choose use_fliess
#'  it will use the Fleiss kappa. The second method is the linear
#' combination for a squared as described in Chapter 2: Exact Formulas.
#' @return A dataframe with the parameter estimates for each simulation or
#' a facet grid of boxplots along the a axis
#' @export
simulate_exact_fit <- function(N_s_vals = c(20,100,300), N_r_vals = c(2,5,10),
                               a_vals =  seq(0, 1, .05),
                               tp = .5, use_fleiss = TRUE,
                               n_sim = 100, output = "plot") {
  # initialize a place to store results
  param_grid <- expand.grid(
    sim = 1:n_sim,
    N_s = N_s_vals,
    a = a_vals,
    N_r = N_r_vals
  ) |>
    mutate(a_sim = NA_real_)

  N <- nrow(param_grid)
  # initialize the change flag for number of subjects
  N_r_old = 1

  for(i in 1:N){
    #print(i/N)

    N_r = param_grid$N_r[i]
    N_s = param_grid$N_s[i]
    my_a = param_grid$a[i]

   ratings <- tapModel::generate_sample_ratings(N_s, N_r,
                                                params = list(t = tp,
                                                              a = my_a,
                                                              p = tp))
    counts <- as_counts(ratings)

    if(use_fleiss){  # use fleiss kappa
      param_grid$a_sim[i] <- tapModel::fleiss_kappa(counts)$a
    } else {
      param_grid$a_sim[i] <- tapModel::exact_accuracy(counts, tp)
    }

  }
  if(output == "data") return(param_grid)

  # plot it
  g <- param_grid |>
    ggplot(aes(x = a, y = a_sim, group = a)) +
    geom_boxplot() +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 18,        # solid diamond
      size = 2,
      color = "red"
    ) +
    geom_abline() +
    facet_grid(N_s ~ N_r)

  return(g)
}

#' Compare model parameters
#' @description Given two sets of model parameters as `rating_params` data frames,
#' plot the t,a,p coefficients for each to make a visual comparison.
#' @param rating_params_1 output of either `fit_ratings()` or `fit_ratings_mcmc()`
#' @param rating_params_2 output of either `fit_ratings()` or `fit_ratings_mcmc()`
#' @param model_names a character vector of length 2 with the names of the models
#' @param vars Some subset of "t", "a", "p", "bias" to include in the output plot,
#' defaults to all of them.
#' @param markers One of "points" or "labels", defaulting to points
#' @param scales Either "free" or "fixed", defaulting to fixed. Fixed scales
#' show the whole [0,1] range for all variables, while free scales adapt to the
#' data.
#' @return A ggplot object with the comparison of the two models
#' @export
compare_model_params <- function(rating_params_1, rating_params_2,
                                 model_names = c("model 1", "model 2"),
                                 vars = c("t","a","p","bias"),
                                 markers = "points",
                                 scales = "fixed"){



  long_params_1 <- rating_params_1 |>
    select(subject_id, rater_id, t, a, p) |>
    mutate(bias = p - t) |>
    group_by(subject_id) |>
    mutate(t = if_else(row_number() == 1, t, NA_real_)) |>
    ungroup() |>
    group_by(rater_id) |>
    mutate(a = if_else(row_number() == 1, a, NA_real_),
           p = if_else(row_number() == 1, p, NA_real_),
           bias = if_else(row_number() == 1, bias, NA_real_)) |>
    ungroup() |>
    gather(var, !!model_names[1],  -subject_id, -rater_id)

  long_params_2 <- rating_params_2 |>
    select(subject_id, rater_id, t, a, p) |>
    mutate(bias = p - t) |>
    group_by(subject_id) |>
    mutate(t = if_else(row_number() == 1, t, NA_real_)) |>
    ungroup() |>
    group_by(rater_id) |>
    mutate(a = if_else(row_number() == 1, a, NA_real_),
           p = if_else(row_number() == 1, p, NA_real_),
           bias = if_else(row_number() == 1, bias, NA_real_)) |>
    ungroup() |>
    gather(var, !!model_names[2],  -subject_id, -rater_id)

  all_params <- long_params_1 |>
    left_join(long_params_2, by = c("subject_id", "rater_id", "var")) |>
    na.omit()

  my_plot <- all_params |>
    filter(var %in% vars) |>
    ggplot(aes(x = !!sym(model_names[1]), y = !!sym(model_names[2]),
               label = if_else(var == "t", subject_id, rater_id))) +
    geom_abline(linetype = "dashed", color = "red") +
    facet_wrap(~var, scales = "free") +
    theme_bw()

  if(markers == "points"){
    my_plot <- my_plot +
      geom_point()
  } else {
    my_plot <- my_plot +
      geom_text(size = 3)
  }

  if(scales == "fixed"){
    my_plot <- my_plot +
      xlim(0,1) + ylim(0,1)
  }

  return(my_plot)

}
