//
//   A t-a_j-p_j model
//   The development is described in the hierarchical chapter. It collapses
//   T_i (binary) into t_i (probability) at the subject level. Compare this to
//   the model (t_i)-a_j-p_j.stan, which collapses right away, so that each
//   rater applies the probability independently instead of at the subject level.
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started


// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of ratings
  int<lower=0> S;   // number of subjects being rated
  int<lower=0> R;   // number of unique raters

  array[N] int rating;        // array of binary ratings
  array[N] int subject_index; // the ID of the subject for the ith rating. Should be in 1:S
  array[N] int rater_index; // the ID of the rater for the ith rating. Should be in 1:R

  array[R] real<lower = 0, upper = 1> p_est;
  array[R] real<lower = 0, upper = 1> a_est;  // rater random parameter
  real<lower = 0, upper = 1> t_est;  // rater random parameter

  real t_prior_sd;
  real a_prior_sd;
  real p_prior_sd;

  int<lower=0, upper=1> use_uniform_t; // 1 if uniform(0, 1) is used for t, 0 otherwise
  int<lower=0, upper=1> use_uniform_a; // 1 if uniform(0, 1) is used for a, 0 otherwise
  int<lower=0, upper=1> use_uniform_p; // 1 if uniform(0, 1) is used for p
}

// The parameters to estimate
parameters {
  // random effects
  array[R] real<lower = 0, upper = 1> p;
  array[R] real<lower = 0, upper = 1> a;  // rater random parameter
  real<lower = 0, upper = 1> t; // the estimated true class probability for subjects
}

// compute the log pis for each rater
transformed parameters {
  array[R] real lpi_00; // Pr(Assigned class = 0 | True class = 0)
  array[R] real lpi_01;
  array[R] real lpi_10;
  array[R] real lpi_11;

  // for efficiency precompute the logs
  for(i in 1:R){
      lpi_00[i] = log(1-(1-a[i])*p[i]);
      lpi_10[i] = log((1-a[i])*(1-p[i]));
      lpi_01[i] = log((1-a[i])*p[i]);
      lpi_11[i] = log(a[i] + (1-a[i])*p[i] );
  }

}

// The model to be estimated.
model {
  // accumulation of log likelihood by subject
  array[S] real sub_00 = rep_array(0.0, S);
  array[S] real sub_01 = rep_array(0.0, S);
  array[S] real sub_10 = rep_array(0.0, S);
  array[S] real sub_11 = rep_array(0.0, S);
  int rater;
  int subject;

 // use either uniform distribution or restricted normals
  if (use_uniform_t == 1) {
    t ~ uniform(0, 1);
  } else {
    t ~ normal(t_est, t_prior_sd);
  }

  if (use_uniform_a == 1) {
    a ~ uniform(0, 1);
  } else {
    a ~ normal(a_est, a_prior_sd);
  }

  if (use_uniform_p == 1) {
    p ~ uniform(0, 1);
  } else {
    p ~ normal(p_est, p_prior_sd);
  }

  // log likelihood contribution from raters
  for(i in 1:N) {
    rater = rater_index[i]; // rater who assigned this rating
    subject = subject_index[i];

    if(rating[i] == 0){ // assigned class 0
      sub_00[subject] += lpi_00[rater];
      sub_10[subject] += lpi_10[rater];
    } else {           // assigned class 1
      sub_01[subject] += lpi_01[rater];
      sub_11[subject] += lpi_11[rater];
    }
  }

  // accumulate by subject
  for(i in 1:S){
      target += log_sum_exp(log(t) + sub_11[i] + sub_10[i],
                            log(1-t) + sub_01[i] + sub_00[i]);
  }

}

