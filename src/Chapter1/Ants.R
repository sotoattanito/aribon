library(dplyr)

falling_ants <- function(L, n, x) {
  if (length(L) > 1) {
    print("L use first value.")
    L <- L[1]
  }
  if (length(n) > 1) {
    print("n use first value.")
    n <- n[1]
  }
  
  if (!is.numeric(L) | !is.numeric(n) | !is.numeric(x)) {
    print("Arguments are numeric.")
    return(NA)
  }
  if (is.na(L) | is.na(n) | any(is.na(x))) {
    print("Arguments can not include NaN.")
    return(NA)
  }
  if (length(L) == 0 | length(n) == 0 | n != length(x)) {
    print("length(L) = 1, length(n) = 1, length(x) = n")
    return(NA)
  }
  if (L < 1 | L > 10 ^ 6 | n < 1 | n > 10 ^ 6 | any(x < 0 | x > L)) {
    print("1 <= L <= 10^6, 1 <= n <= 10^6, 0 <= x <= L")
    return(NA)
  }
  
  calc_time <- function(x, L, f) {
    return(ifelse(x %in% c(0, L), 0, f(x, L - x)))
  }
  
  min_time <- sapply(x, calc_time, L, min) %>% max()
  max_time <- sapply(x, calc_time, L, max) %>% max()
  return(c(min = min_time, max = max_time))
}

falling_ants(10, 3, c(2, 6, 7))
