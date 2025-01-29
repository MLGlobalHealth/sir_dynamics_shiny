
# variables
deriv(S) <- - beta * S * I / N - u * S
deriv(V) <- u * S
deriv(I) <-  beta * S * I / N - sigma * I
deriv(R) <- sigma * I  

# initial conditions
initial(S) <- N - I_init
initial(V) <- 0
initial(I) <- I_init
initial(R) <- 0

# parameters
N <- user(1000)
I_init <- user(1)
beta <- user(2)
sigma <- user(0.5)
u <- user(0.1)
