# function that count integral using Monte Carlo method
count_integral <- function(amount_of_simulations, lower_bound, upper_bound, f) {
    if (upper_bound == Inf) {
        upper_bound <- 10^10
    }
    
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
integral = count_integral(amount_of_simulations = amount_of_simulations,
                          lower_bound = 0,
                          upper_bound = Inf,
                          f = third_initial_function)
print(paste("Third integral value: ", integral))