//
//   A t-a-p model with fixed a and p and random t, using soft step
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started


// functions to make the code simpler below
functions {
  //we need to push the truth value s toward 0 or 1 to recover accuracy
  // since those parameters generate data with zeros or ones

  real soft_step(real x, real lambda){
    return(1/(1+exp(-x*lambda)));
  }

  // rating = 0 or 1, for the class
  // a = accuracy for rater
  // t = probability of Class 1
  // p = random rate of Class 1 assignment for inaccurate ratings
  // lambda = scaling parameter for soft step

  real log_lik(real rating, real a, real tau, real p, real lambda){
    real cond_prob;
    real ll; //log likelihood

     if(rating == 0) cond_prob = a*(1-soft_step(tau,lambda)) + (1.0 - a)*(1.0-p);
    else
    // Conditional probability of class 1
    cond_prob = a*soft_step(tau,lambda) + (1.0 - a)*p;

    // return log likelihood
    ll = log(cond_prob);

    return(ll);
  }
}

// The ratings summary (number of 1-ratings per case) and descriptives
data {
  int<lower=0> N;   // number of ratings
  int<lower=0> S;   // number of subjects being rated
  real<lower=1> lambda;  // scaling parameter for soft step

  array[N] int rating;  // rating array
  array[N] int subject_index; // the ID of the subject for the ith rating. Should be in 1:S

}

// The parameters to estimate
parameters {
  // fixed effects
  real<lower = 0, upper = 1> p;  // average random rate for inaccurate ratings
  real<lower = 0, upper = 1> a;  // average accuracy

  // random effects
  array[S] real tau;  // latent scale parameter for each subject's true class probability

}

// create the estimated truth values as a convenience
transformed parameters {
  array[S] real t;  // Declare an array to store the transformed truth values

  for(i in 1:S) {
    t[i] = soft_step(tau[i], lambda);  // Apply the transformation using soft_step
  }
}

// The model to be estimated.
model {

  // global means for accuracy and guess rate
  a ~ uniform(0,1);
  p ~ uniform(0,1);

  // random effects for true class
  tau  ~ normal(0,3); // an array of random effects

  // Likelihood model
  for(i in 1:N) {
      target += log_lik(rating[i],  // rating value
                        a,         // average accuracy
                        tau[subject_index[i]], // true class probability
                        p,          // average guessing paramter
                        lambda);        // scaling for soft step
  }
}

