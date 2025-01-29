
# variables
deriv(V) <- u * theta - mu * V
deriv(S) <- (1-u) * theta - beta * S * I / N - mu * S
deriv(I) <- beta * S * I / N - sigma * I - mu * I
deriv(R) <- sigma * I - mu * R

# initial conditions
initial(V) <- u * (N - I_init) 
initial(S) <- (1 - u) * (N - I_init)
initial(I) <- I_init
initial(R) <- 0

# parameters
N <- user(1000)
I_init <- user(1)
beta <- user(2)
sigma <- user(0.1)
theta <- user(10)
u <- user(0.5)
mu <- user(4)
