//
//   Stan model specification for fixed rater accuracy and no random effects
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// functions to make the code simpler below
functions {
  real p_true(real a, real s) {  // convenience function for binomial probability for 
    return a + (1.0-a)*s;        // subjects that are class 1 in reality
  }
  real p_false(real a, real s) {  // convenience function for binomial probability for
    return (1.0-a)*s;            // subjects that are class 2 in reality
  }
  
}


// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of subjects
  array[N] int<lower=0> R;   // number of raters fo a given subject
  array[N] int count;  // count of ratings of category 1 for subject i
}

// The parameter to estimate
parameters {
  real<lower=0, upper = 1> a1; // fixed for all raters
  real<lower=0, upper = 1> a0; // fixed for all raters
  real<lower=0, upper = 1> p0;    // guess rate for class 0 when inaccurate
  real<lower=0, upper = 1> p1;    // guess rate for class 1 when inaccurate
  real<lower=0, upper = 1> t;        // true class 1 rate
  
}

// The model to be estimated. We model the output
// count (of 1s) by the binomial mixture described
// in the paper. S is the fraction of 1-ratings in the whole data set
// The log_sum_exp function is useful for this--we take the log of each binomial 
// likelihood using built-in functions, and the log_sum_exp function exponentiates,
// adds, and then takes the log to get the actual likelihood we care about. 
// cf http://modernstatisticalworkflow.blogspot.com/2016/10/finite-mixture-models-in-stan.html
model {
  a1 ~ uniform(0,1);
  a0 ~ uniform(0,1);
  t ~ uniform(0,1);
  p0 ~ uniform(0,1);
  p1 ~ uniform(0,1);
  
  for(i in 1:N) {  // for each subject rated
    target += log_sum_exp(log(t)   + binomial_lpmf(count[i] | R[i], p_true(a1,p1)),
                          log(1-t) + binomial_lpmf(count[i] | R[i], p_false(a0,p0)));
  }
}

