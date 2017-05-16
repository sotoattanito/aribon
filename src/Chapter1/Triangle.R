library(dplyr)

triangle <- function(x) {
  if (!is.numeric(x)) {
    warning("x is not numeric.")
    return(NA)
  }
  if (any(is.na(x))) {
    warning("x can not include NA.")
    return(NA)
  }
  if (any(length(x) < 3, length(x) > 100)) {
    warning("3 <= n <= 100")
    return(NA)
  }
  if (any(x < 1, x > 10 ^ 6)) {
    warning("1 <= x <= 10 ^ 6")
    return(NA)
  }
  
  df <- combn(x, 3) %>% t() %>% data.frame()
  colnames(df) <- paste0("X", 1:3)
  filtered <- 
    filter(df, X1 < X2 + X3, X2 < X1 + X3, X3 < X1 + X2) %>%
    mutate(Length = X1 + X2 + X3) %>%
    arrange(desc(Length))
  max_length <- ifelse(nrow(filtered) > 0, slice(filtered, 1)$Length, 0)

  return(c(Length = max_length))
}

x <- c(2, 3, 4, 5, 10)
triangle(x)
# Length 
#     12 

y <- c(1, 3, 5, NA_real_, 9)
triangle(y)
# [1] NA
# Warning message:
#   In triangle(y) : x can not include NA.

z <- 0:5
triangle(z)
# [1] NA
# Warning message:
#   In triangle(z) : 1 <= x <= 10 ^ 6

v <- c(1, 3, 7, 10 ^ 6 + 1, 5)
triangle(v)
# [1] NA
# Warning message:
#   In triangle(v) : 1 <= x <= 10 ^ 6

w <- 1:1000
triangle(w)
# [1] NA
# Warning message:
#   In triangle(w) : 3 <= n <= 100

