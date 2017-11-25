amount_of_simulations = as.integer(readline(prompt = "Enter amount of simulations: "))
lambda = as.integer(readline(prompt = "Enter lambda coefficient of Poisson distribution: "))

dataset <- rpois(amount_of_simulations, lambda)

hist(dataset)
plot(dataset)