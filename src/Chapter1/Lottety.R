calc <- function(m, k) {
  x <- sapply(m, "-", k)
  return(as(x, "vector"))
}

lottety <- function(n, m, k) {
  # Check...
  if (any(is.na(n), is.na(m), is.na(k))) {
    stop("Arguments is not NA.")
  }
  if (!is(n, "vector") | length(n) != 1L | n < 1 | n > 50 | 
      !is(m, "vector") | length(m) != 1L | m < 1 | m > 10 ^ 8 |
      !is(k, "vector") | length(k) != n | any(k < 1 | k > 10 ^ 8)) {
    stop("bad arguments.")
  }
  
  # Logic...
  ans <- m;
  for (i in 1:4) {
    ans <- calc(ans, k);
  }
  return(ifelse(any(ans == 0), "Yes", "No"))
}

lottety(3, 10, c(1,3,5)) # Yes
lottety(3, 9, c(1,3,5))  # No
lottety(50, 123, 1:50) # Yes
#lottety(3, 4, c(1, NA, 5)) # Error,stoped.
