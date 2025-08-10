//
//   A t_i-a_j-p_j model
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started


// functions to make the code simpler below
functions {
  // rating = 0 or 1, for the class
  // a = accuracy for rater
  // t = probability of Class 1
  // p = random rate of Class 1 assignment for inaccurate ratings

  real log_lik(real rating, real a, real t, real p){
    real cond_prob;
    real ll; //log likelihood

     if(rating == 0) cond_prob = a*(1-t) + (1.0 - a)*(1.0-p);
    else
    // Conditional probability of class 1
    cond_prob = a*t+ (1.0 - a)*p;

    // return log likelihood with a penalty for tau being too far from 0 or 1
    ll = log(cond_prob);

    return(ll);
  }
}

// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of ratings
  int<lower=0> S;   // number of subjects being rated
  int<lower=0> R;   // number of unique raters

  array[N] int rating;        // array of binary ratings
  array[N] int subject_index; // the ID of the subject for the ith rating. Should be in 1:S
  array[N] int rater_index; // the ID of the rater for the ith rating.

  array[R] real<lower = 0, upper = 1> p_est;
  array[R] real<lower = 0, upper = 1> a_est;  // rater random parameter
  array[S] real<lower = 0, upper = 1> t_est;  // rater random parameter

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
  array[R]real<lower = 0, upper = 1> p;
  array[R] real<lower = 0, upper = 1> a;  // rater random parameter
  array[S] real<lower = 0, upper = 1> t;  // rater random parameter

}


// The model to be estimated.
model {

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

  // Likelihood model, incremented for each rating
  for(i in 1:N) {
      target += log_lik(rating[i],  // rating value
                        a[rater_index[i]] ,         // average accuracy
                        t[subject_index[i]], // true class probability
                        p[rater_index[i]]);
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



