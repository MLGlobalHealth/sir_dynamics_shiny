# This file compiles and plots an odin model 
library(odin)

run_child_vaccination = function(N_init, I_init, beta, sigma, theta, mu, u, max_t){
  # The file name of the model
  model <- "child_vaccination.R"
  
  # Compiles the model
  generator <- odin::odin(model)
  mod <- generator$new(user = list(sigma = sigma, beta = beta, theta = theta,
                                   mu = mu, u = u, N_init = N_init, I_init = I_init))

  
  # Set time variable
  tt <- seq(0, max_t, by = 0.5)
  
  # Runs the model
  y <- mod$run(tt)
  return (y)
}

run_emergency_vaccination = function(N, I_init, beta, sigma, u, max_t){
  # The file name of the model
  model <- "emergency_vaccination.R"
  
  # Compiles the model
  generator <- odin::odin(model)
  mod <- generator$new(user = list(sigma = sigma, beta = beta, u = u, 
                                   N = N, I_init = I_init))
  
  
  # Set time variable
  tt <- seq(0, max_t, by = 0.5)
  
  # Runs the model
  y <- mod$run(tt)
  return (y)
}

run_heterogeneity = function(N_A, N_B, I_init_A, I_init_B, beta_AA, beta_BA, 
                             beta_AB, beta_BB, sigma,  max_t){
  # The file name of the model
  model <- "heterogeneity.R"
  
  # Compiles the model
  generator <- odin::odin(model)
  mod <- generator$new(user = list(sigma = sigma, beta_AA = beta_AA, 
                                   beta_BA = beta_BA, beta_AB = beta_AB, 
                                   beta_BB = beta_BB, 
                                   N_A = N_A, N_B = N_B,
                                   I_init_A = I_init_A, I_init_B = I_init_B))
  
  
  # Set time variable
  tt <- seq(0, max_t, by = 0.5)
  
  # Runs the model
  y <- mod$run(tt)
  return (y)
}