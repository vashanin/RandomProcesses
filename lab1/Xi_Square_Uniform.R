# check whether distribution is uniform
is_unifom = function(dataset) {
    hist(dataset)
    
    size_of_dataset = length(dataset)
    
    sturgess_formula = 1 + 3.322 * log10(size_of_dataset)
    amount_of_intervals = round(sturgess_formula) + 1
    
    max <- max(dataset)
    min <- min(dataset)
    
    step <- (max - min) / amount_of_intervals
    
    actual_distribution <- list()
    theoretical_distribution <- list()
    square_difference <- list()
    
    for (i in 0:(amount_of_intervals - 1)) {
        actual <- 0  
        if (i == amount_of_intervals - 1)
            actual <- length(dataset[dataset >= min + i*step & dataset <= min + (i + 1)*step])
        else
            actual <- length(dataset[dataset >= min + i*step & dataset < min + (i + 1)*step])
        
        theoretical <- size_of_dataset / amount_of_intervals
        
        actual_distribution <- append(actual_distribution, 
                                      actual)
        theoretical_distribution <- append(theoretical_distribution, 
                                           theoretical)
        square_difference <- append(square_difference, 
                             (actual - theoretical)^2 / theoretical)
    }
    
    Chi_Square_actual <- 0
    for (item in square_difference) {
        Chi_Square_actual <- Chi_Square_actual + item
    }
    
    Chi_Square_critical <- qchisq(p = 0.95, df = amount_of_intervals - 1)
    
    print(Chi_Square_actual)
    print(Chi_Square_critical)
    
    return(Chi_Square_actual < Chi_Square_critical)
}

dataset <- runif(10000)
test_result <- is_unifom(dataset)

if (test_result == TRUE) {
    print("Chi Square test decided that RUNIF is genetare uniformly distributed numbers")
} else {
    print("Chi Square test decided that RUNIF is NOT generate uniformly distributed numbers.")
}

lower <- as.numeric(readline(prompt = "Enter lower bound of interval: "))
upper <- as.numeric(readline(prompt = "Enter upper bount of interval: "))
amount <- as.numeric(readline(prompt = "Enter amount of simulations: "))

custom_dataset <- runif(amount, min = lower, max = upper)
hist(custom_dataset)