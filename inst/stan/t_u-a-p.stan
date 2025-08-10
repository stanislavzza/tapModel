
//
//   Model specification for estimating t_u for each (N_r,N_c) type, with
//   average a and p parameters. 
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// functions to make the code simpler below
functions {
  real p_true(real a, real s) {  // binomial probability for
    return a + (1.0-a)*s;        // subjects that are class 1 in reality
  }
  real p_false(real a, real s) {  // binomial probability for
    return (1.0-a)*s;            // subjects that are class 0 in reality
  }

}

// The data provided from the ratings
data {
  int<lower=0> N;              // number of rows of data
  array[N] int<lower=0> N_r;   // number of raters for a count pair
  array[N] int N_c;            // count of ratings of category 1 for count pair
  array[N] int<lower=0> n;     // multiplicity of this (N_r, N_c) pair
}

// Parameters to estimate
parameters {
  real<lower=0, upper = 1> a;          // average accuracy 
  real<lower=0, upper = 1> p;          // rate of class 1 ratings when inaccurate
  array[N] real<lower=0, upper = 1> t; // estimated true class 1 rate for each 
                                       // (N_r, N_c) pair
}

// We model the count of 1s (Class 1 ratings) by the binomial mixture described
// in Chapter 5 at kappazoo.com. 

// cf http://modernstatisticalworkflow.blogspot.com/2016/10/finite-mixture-models-in-stan.html
model {
  a ~ uniform(0,1); // flat priors for all parameters, since we are on [0,1]
  t ~ uniform(0,1);
  p ~ uniform(0,1);

  for(i in 1:N) {  // for each subject rated
    target += n[i] * // multiplicity of the (N_r, N_c) pair
              log_sum_exp( 
                 log(t[i])  + binomial_lpmf(N_c[i] | N_r[i], p_true(a,p)),
                 log(1-t[i]) + binomial_lpmf(N_c[i] | N_r[i], p_false(a,p)));
  }
}

