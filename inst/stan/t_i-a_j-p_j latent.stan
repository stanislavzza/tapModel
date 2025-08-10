// hierarchical t_i-a_j-p_j model with latent variables and regularization
// initialized to the implied model

data {
  int<lower=0> N;   // Number of ratings
  int<lower=0> S;   // Number of subjects
  int<lower=0> R;   // Number of raters

  array[N] int<lower=0, upper=1> rating; // Binary ratings
  array[N] int<lower=1, upper=S> subject_index; // Subject index
  array[N] int<lower=1, upper=R> rater_index;   // Rater index
}

parameters {
  // Global means on the real line
  real mu_t;
  real mu_a;
  real mu_p;

  // Standard deviations for the offsets
  real<lower=0> sigma_t;
  real<lower=0> sigma_a;
  real<lower=0> sigma_p;

  // Latent offsets for individual parameters
  vector[S] z_t;  // Subject-specific offsets
  vector[R] z_a;  // Rater accuracy offsets
  vector[R] z_p;  // Rater random rate offsets
}

transformed parameters {
  // Map latent parameters to [0, 1]
  vector[S] t = inv_logit(mu_t + sigma_t * z_t);
  vector[R] a = inv_logit(mu_a + sigma_a * z_a);
  vector[R] p = inv_logit(mu_p + sigma_p * z_p);
}

model {
  // Priors on global means
  mu_t ~ normal(0, 1); // Weakly informative prior
  mu_a ~ normal(0, 1);
  mu_p ~ normal(0, 1);

  // Priors on standard deviations
  sigma_t ~ normal(0, 1);  // Shrinkage for offsets
  sigma_a ~ normal(0, 1);
  sigma_p ~ normal(0, 1);

  // Priors on latent offsets
  z_t ~ normal(0, 1);  // Standard normal
  z_a ~ normal(0, 1);
  z_p ~ normal(0, 1);

  // Likelihood
  for (i in 1:N) {
    real cond_prob;
    if (rating[i] == 0)
      cond_prob = a[rater_index[i]] * (1 - t[subject_index[i]]) +
                  (1.0 - a[rater_index[i]]) * (1.0 - p[rater_index[i]]);
    else
      cond_prob = a[rater_index[i]] * t[subject_index[i]] +
                  (1.0 - a[rater_index[i]]) * p[rater_index[i]];

    target += log(cond_prob);
  }
}

generated quantities {
  real avg_bits = 0; // Initialize total log2 likelihood


  for (i in 1:N) {
    real cond_prob;

    // Compute the conditional probability
    if (rating[i] == 0)
      cond_prob = a[rater_index[i]] * (1 - t[subject_index[i]]) +
                  (1.0 - a[rater_index[i]]) * (1.0 - p[rater_index[i]]);
    else
      cond_prob = a[rater_index[i]] * t[subject_index[i]] +
                  (1.0 - a[rater_index[i]]) * p[rater_index[i]];

    // Accumulate log2 of the conditional probabilities
    avg_bits += -log2(cond_prob);
  }

  // per rating
  avg_bits = avg_bits/N;
}

