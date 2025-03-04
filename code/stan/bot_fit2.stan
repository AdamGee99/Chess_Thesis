// Feb 5, 2025
// updating previous bot_fit to remove the player effect - it is bugging out in the previous fit
// its clearly just correlated with rating, but the rating diff effect should just capture this?


data {
  int<lower=0> N;                      // the number of games
  int<lower=0> J;                      // the number of focal players
  array[N] int<lower=0, upper=1> y;    // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating focal player involved
  vector[N] colour;                    // the colour of the focal player
  vector[N] elo;                       // diff in elo scores between players
  vector[N] win_prop;                  // current win ratio for focal player
  vector[N] open_win_rate;             // the win rate for white in current opening
}


parameters {
  array[J] real beta;                  // player-level experiential effect
  real mu_beta;                        // mean population-level experiential effect
  real <lower=0> tau;                  // sd of betas, player-level experential effect
  real <lower=0> sigma_1;              // sd of mu_beta
  
  real gamma1;                         // effect of colour
  real gamma2;                         // effect of elo difference
  real gamma3;                         // controlling for opening win rate

  real<lower=0> sigma_g1;              // sd of gamma1
  real<lower=0> sigma_g2;              // sd of gamma2
  real<lower=0> sigma_g3;              // sd of gamma3
}


model {
  sigma_1 ~ inv_gamma(1, 1);           // prior for sd of mu_beta
  tau ~ inv_gamma(1, 1);               // prior for sd of betas (player-level experiential effect)
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  beta ~ normal(mu_beta, tau);         // prior for betas (plyer-level experiential effect)

  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, sigma_g2);        // prior for gamma2
  gamma3 ~ normal(0, sigma_g3);        // prior for gamma3
  
  sigma_g1 ~ inv_gamma(1, 1);          // prior for sd of gamma1
  sigma_g2 ~ inv_gamma(1, 1);          // prior for sd of gamma2
  sigma_g3 ~ inv_gamma(1, 1);          // prior for sd of gamma3
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i] + gamma3 * open_win_rate[i];
  }
  y ~ bernoulli_logit(pred);
}

