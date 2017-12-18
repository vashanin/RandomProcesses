# check whether distribution is exponential
is_exponent = function(dataset) {
    hist(dataset)
    
    size_of_dataset = length(dataset)
    average <- mean(dataset)
    lambda <- 1 / average
    
    f = function(x) {exp(-lambda * x)}
    
    sturgess_formula = 1 + 3.322 * log10(size_of_dataset)
    amount_of_intervals = round(sturgess_formula) + 1
    
    max <- max(dataset)
    min <- min(dataset)
    
    step <- (max - min) / amount_of_intervals
    
    actual_frequency <- list()
    theoretical_frequency <- list()
    Chi_Square_actual <- 0
    
    for (i in 0:(amount_of_intervals - 1)) {
        lower_bound <- min + i * step  
        upper_bound <- min + (i + 1) * step
      
        actual <- 0  
        if (i == amount_of_intervals - 1)
            actual <- length(dataset[dataset >= lower_bound & dataset <= upper_bound])
        else
            actual <- length(dataset[dataset >= lower_bound & dataset < upper_bound])
        
        actual_frequency <- append(actual_frequency, actual)
        
        hitting_probability <- f(lower_bound) - f(upper_bound)
        theoretical <- size_of_dataset * hitting_probability
        
        theoretical_frequency <- append(theoretical_frequency, theoretical)
        
        difference <- (actual - theoretical)^2 / theoretical
        Chi_Square_actual <- Chi_Square_actual + difference
    }
    
    Chi_Square_critical <- qchisq(p = 0.95, df = amount_of_intervals - 2)
    
    return(Chi_Square_actual < Chi_Square_critical)
}

dataset <- rexp(10000)
test_result <- is_exponent(dataset)

if (test_result == TRUE) {
    print("Chi Square test decided that REXP is genetare exponentially distributed numbers")
} else {
    print("Chi Square test decided that REXP is NOT generate exponentially distributed numbers.")
}

hist(dataset)