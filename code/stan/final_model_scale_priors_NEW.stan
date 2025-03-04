// Model Feb 5, 2025
// some of the priors are incorrect in the model statement of previous file (final_model_scale_priors.stan)
// switching sigma_g1, sigma_g2, sigma_1 to IG(1,1) prior
// switching mu_beta ~ N(0, sigma_1) by removing incorrect duplicated mu ~ N(0,1) statement


data {
  int<lower=0> N;                      // the number of games
  int<lower=0> J;                      // the number of focal players
  array[N] int<lower=0, upper=1> y;    // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating focal player involved
  vector[N] colour;                    // the colour of the focal player
  vector[N] elo;                       // diff in elo scores between players
  vector[N] win_prop;                  // current win ratio for focal
  
}


parameters {
  real mu_beta;                        // population average winner effect
  vector[2] nu;                        // location of beta[ , j]
  vector<lower=0>[2] tau;              // scale of beta[ , j], sd of effects
  cholesky_factor_corr[2] L_Omega;     // Cholesky of correlation of beta[ , j]
  matrix[2, J] beta_std;               // standard beta (beta - nu) / Sigma
  real<lower=0> sigma_1;               // sd of mu_beta
  real<lower=0> sigma_g1;              // sd of gamma1
  real<lower=0> sigma_g2;              // sd of gamma2
  real gamma1;                         // effect of colour
  real gamma2;                         // effect of elo difference
}
transformed parameters {
  matrix[2, J] beta = rep_matrix(nu, J)
                      + diag_pre_multiply(tau, L_Omega) * beta_std;
}

model {
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  sigma_1 ~ normal(0, 1);              // prior for sd of mu_beta
  sigma_g1 ~ normal(0, 1);             // prior for sd of gamma1
  sigma_g2 ~ normal(0, 1);             // prior for sd of gamma2

  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, sigma_g2);        // prior for gamma2

  nu[1] ~ normal(0, 1);
  nu[2] ~ normal(mu_beta, 1);          // standardized so sds here fixed
  
  tau ~ inv_gamma(1, 1);               // prior for sd of both random effects
  L_Omega ~ lkj_corr_cholesky(2);      // prior for correlation matrix
  to_vector(beta_std) ~ normal(0, 1);  // beta[ , j] ~ multi_normal(nu, Sigma)
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i];
  }
  y ~ bernoulli_logit(pred);
}

