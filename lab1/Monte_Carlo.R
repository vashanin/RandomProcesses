# function that count integral using Monte Carlo method
count_integral <- function(amount_of_simulations, lower_bound, upper_bound, f) {
    X <- runif(amount_of_simulations, lower_bound, upper_bound)
    Y <- f(X) / (1 / (upper_bound - lower_bound))
    integral <- sum(Y) / amount_of_simulations
    
    return(integral)
}

amount_of_simulations = as.integer(readline(prompt = "Enter amount of simulations: "))

first_initial_function <- function(x) { x^7 + x^3 + x }
integral = count_integral(amount_of_simulations = amount_of_simulations,
                          lower_bound = 0,
                          upper_bound = 2,
                          f = first_initial_function)
print(paste("First integral value: ", integral))

second_initial_function <- function(x) { 5*cos(pi*x) }
integral = count_integral(amount_of_simulations = amount_of_simulations,
                          lower_bound = 0,
                          upper_bound = pi,
                          f = second_initial_function)
print(paste("Second integral value: ", integral))

third_initial_function <- function(x) { 1 / (sqrt(x) * (x + 1) * (x + 1)) }

# Зробимо заміну, щоб позбутися від нескінченності в межах інтегрування
improved_third_initial_function <- function(t) {third_initial_function(1/t- 1) * 1/(t*t)}
integral = count_integral(amount_of_simulations = amount_of_simulations,
                          lower_bound = 0,
                          upper_bound = 1,
                          f = improved_third_initial_function)
print(paste("Third integral value: ", integral))

X <- runif(amount_of_simulations, 0, 2)
Y <- runif(amount_of_simulations, 0, 2)

general_amount = length(X) * length(Y)
fitted_amount <- 0

for (i in 1:length(X))
    for (j in 1:length(Y)) {
        if (Y[j] <= 1 - X[i]*X[i])
            if ((X[i] - 1)^2 + (Y[j] - 1)^2 <= 1)      
                if (Y[j] >= (X[i] - 1)^2)
                    fitted_amount <- fitted_amount + 1
        
    }
square <- 4 * fitted_amount / general_amount
print(paste("Area: ", square))