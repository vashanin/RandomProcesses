dataset <- list()
amount_of_simulations <- 1000

for (i in 1:amount_of_simulations) {
    temp_dataset <- runif(amount_of_simulations)
    dataset <- append(dataset, max(temp_dataset))
}

dataset <- unlist(dataset)
hist(dataset)
