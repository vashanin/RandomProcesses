k = 5
n = 17

lambda <- 5

P <- function(n, t) {((lambda * t)^n) * exp(-lambda * t) / factorial(n)}

result <- c()
previous <- 0

for (t in 1:20) {
    current <- P(n, t)
    result <- append(result, current)
}
hist(result)