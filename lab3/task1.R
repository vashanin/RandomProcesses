X <- rpois(1000, 5)

is_pois <- function(dataset) {
    hist(dataset)
    
    size_of_dataset <- length(dataset)
    
    lambda = mean(dataset)
    
    unique_dataset <- c()
    
    for (item in dataset) {
        if (!(item %in% unique_dataset)) {
            unique_dataset <- append(unique_dataset, item)
        }
    }
    
    actual_distribution <- c()
    for (item in unique_dataset) {
        actual <- length(dataset[dataset == item])
        actual_distribution <- append(actual_distribution, actual)
    }
    
    P <- function(i) {(lambda^i) * exp(-lambda) / factorial(i)}
    
    theoretical_distribution <- c()
    for (item in unique_dataset) {
        theoretical <- size_of_dataset * P(item)
        theoretical_distribution <- append(theoretical_distribution, theoretical)
    }
    
    Chi_Square_actual <- 0
    for (i in 1:length(unique_dataset)) {
        th <- theoretical_distribution[[i]]
        act <- actual_distribution[[i]]
        
        Chi_Square_actual <- Chi_Square_actual + ((act - th) ^ 2) / th
    }
    
    Chi_Square_critical <- qchisq(p = 0.95, df = length(unique_dataset) - 2)
    
    return(Chi_Square_actual < Chi_Square_critical)
}

print(is_pois(X))