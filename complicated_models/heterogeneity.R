
# variables
deriv(S_A) <- -beta_AA * S * I_A / N_A 
deriv(I_A) <- beta * S * I / N - sigma * I
deriv(R_A) <- sigma * I   
deriv(S_B) <- -beta * S * I / N 
deriv(I_B) <- beta * S * I / N - sigma * I
deriv(R_B) <- sigma * I   

# initial conditions
initial(S) <- N - I_init
initial(E) <- 0
initial(I) <- I_init
initial(R) <- 0

# parameters
N_A <- user(1000)
N_B <- user(1000)
I_init <- user(1)
beta <- user(2)
gamma <- user(0.1)
sigma <- user(0.5)
