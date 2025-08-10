//
//   A t_i-a_j-p_j model with a_, p_ provided as constants as part of EM
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

  array[N] real a; // rater accuracy
  array[N] real p; // rater random assignment

}

// The parameters to estimate
parameters {

  // random effects
  array[S]real<lower = 0, upper = 1> t;
}


// The model to be estimated.
model {

  // global means for accuracy and guess rate
  t ~ uniform(0,1); // an array

  // Likelihood model, incremented for each rating
  for(i in 1:N) {
      target += log_lik(rating[i],  // rating value
                        a[i] ,         // average accuracy
                        t[subject_index[i]], // true class probability
                        p[i]);
  }
}

