// chat-gpt generated model to regularize truth

data {
  int<lower=1> N;             // Number of subject-rater observations
  int<lower=1> J;             // Number of raters
  int<lower=1> I;             // Number of subjects
  array[N] int<lower=1, upper=I> subject; // Subject index for each observation
  array[N] int<lower=1, upper=J> rater;   // Rater index for each observation
  array[N] int<lower=0, upper=1> Y;       // Observed rating (0 or 1)
}
parameters {
  real<lower=0, upper=1> a;      // Average accuracy
  real<lower=0, upper=1> p; // Rater-specific guessing probabilities
  array[I] real<lower=0, upper=1> t; // Subject-specific truth probabilities
  real<lower=0, upper=1> mu_t;   // Population mean truth
  real<lower=0> sigma_t;         // Population variability in truth
}
model {
  // Priors
  a ~ beta(2, 2);                     // Weak prior for accuracy
  p ~ beta(2, 2);                     // Weak prior for guessing probabilities
  mu_t ~ beta(2, 2);                  // Weak prior for population mean truth
  sigma_t ~ cauchy(0, 1);             // Prior for population variability
  t ~ normal(mu_t, sigma_t);          // Hierarchical prior for truth probabilities

  // Likelihood
  for (n in 1:N) {
    int s = subject[n];
    int r = rater[n];
    real prob = t[s] * (a + (1 - a) * p) + (1 - t[s]) * (1 - a) * p;
    Y[n] ~ bernoulli(prob);
  }
}
