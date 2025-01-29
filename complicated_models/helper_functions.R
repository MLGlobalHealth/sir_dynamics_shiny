# This file compiles and plots an odin model 
library(odin)

run_child_vaccination = function(N, I_init, beta, sigma, theta, mu, u, max_t){
  # The file name of the model
  model <- "child_vaccination.R"
  
  # Compiles the model
  generator <- odin::odin(model)
  mod <- generator$new(user = list(sigma = sigma, beta = beta, theta = theta,
                                   mu = mu, u = u, N = N, I_init = I_init))

  
  # Set time variable
  tt <- seq(0, max_t, by = 0.5)
  
  # Runs the model
  y <- mod$run(tt)
  return (y)
}

run_seir_model = function(N, I_init, beta, sigma, gamma, max_t){
  # The file name of the model
  model <- "seir_model.R"
  
  # Compiles the model
  generator <- odin::odin(model)
  mod <- generator$new(user = list(sigma = sigma, beta = beta, gamma = gamma, 
                                   N = N, I_init = I_init))
  
  
  # Set time variable
  tt <- seq(0, max_t, by = 0.5)
  
  # Runs the model
  y <- mod$run(tt)
  return (y)
}

run_seirs_model = function(N, I_init, beta, sigma, gamma, alpha, max_t){
  # The file name of the model
  model <- "seirs_model.R"
  
  # Compiles the model
  generator <- odin::odin(model)
  mod <- generator$new(user = list(sigma = sigma, beta = beta, gamma = gamma, 
                                   alpha = alpha, N = N, I_init = I_init))
  
  
  # Set time variable
  tt <- seq(0, max_t, by = 0.5)
  
  # Runs the model
  y <- mod$run(tt)
  return (y)
}