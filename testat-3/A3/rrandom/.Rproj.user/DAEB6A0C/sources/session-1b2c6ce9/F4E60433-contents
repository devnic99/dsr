#' The function 'choice' n item of the given list x randomly with the propability p
#' @param x list which should be chosen from
#' @param p optional list of propabilities for each value in x (default 1:length(x))
#' @param n optional number of chosen items (default: 1)
#' @return a list of length n containing random items from x
choice <- function(x, p = rep(1 / length(x), times = length(x)), n = 1) {
    if (length(x) != length(p)) {
        stop("x and p must have same length")
    }
    if (abs(sum(p) - 1) > 0.00001) {
        warning("Sum of propabilities is not 1. Rescaling...")
        p <- p / sum(p)
    }
    random_numbers <- runif(n)
    choices <- rep(NA, length = n)
    for (i in 1:n) {
        p_idx <- 1
        propability_sum <- p[1]
        while (propability_sum < random_numbers[i]) {
            p_idx <- p_idx + 1
            propability_sum <- propability_sum + p[p_idx]
        }
        choices[i] <- x[p_idx]
    }
    return(choices)
}

choices <- choice(c(0, 1), p = c(0.9, 0.1), n = 100)
print(choices)
