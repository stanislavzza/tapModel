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
      group_by(SubjectID__) |>
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
    group_by(SubjectID__) |>
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
#' @param observed_data data frame of observed data, which must either (1) have
#' columns SubjectID__and rating (0 or 1) in long format, or (2) have columns N_r,
#' N_c, and n (the count) to give the count distribution over subjects
#' @param params The parameters of the model, a list with t, a0, a1, p0, p1
#' @return A dataframe with model fit statistics
#' @details This function compares the model to observed data by aggregating the observed and
#' modeled distributions of class 1 counts by subject. It cannot accomodate individual-level
#' parameters like t_i, a_j, or p_j. Therefore, no RaterID__ is required.
#' @examples
#' comparison |>
#' summarize(mean_error = mean(abs(observed - modeled)/observed))
#'
#' @export
compare_model_to_observed <- function(observed_data, params) {

  # short hand for the params, as a convenience
  t <- params$t
  a0 <- params$a0
  a1 <- params$a1
  p0 <- params$p0
  p1 <- params$p1

  if (str_detect(names(observed_data), "SubjectID__")) {
     # Aggregate observed frequencies if not already done
    counts <- observed_data %>%
      group_by(SubjectID__) |>
      summarize(N_r = n(),
                N_c = sum(rating),
                .groups = "drop") |>
      count(N_r, N_c, name = "observed")
  } else { # we alrady have the counts
    counts <- observed_data |>
              mutate(observed = n)
  }

  # convert counts to ratios
  counts <- counts |>
            group_by(N_r) |>
            mutate(observed = observed/sum(observed)) |>
            ungroup()

  # Aggregate simulated frequencies
  prob1 <- a1 + (1-a1)*p1 # Pr[C = 1 | T = 1]
  prob0 <- (1-a0)*p0 # Pr[C = 1 | T = 0]

  modeled_rates <- counts |>
                   select(N_r, N_c) |>
                   rowwise() |>
                   mutate(modeled = t*dbinom(N_c, N_r, prob1) + (1-t)*dbinom(N_c, N_r, prob0) )

  # Merge observed and simulated frequencies
  comparison <- counts |>
                left_join(modeled_rates)

  return(comparison)
}


