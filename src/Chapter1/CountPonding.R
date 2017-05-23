search.around <- function(index, mt) {
  n <- nrow(mt)
  m <- ncol(mt)
  i <- (index - 1) %% n + 1
  j <- (index - 1) %/% n + 1
  rowindex <- seq(i - 1, i + 1)
  rowindex <- rowindex[1 <= rowindex & rowindex <= n]
  colindex <- seq(j - 1, j + 1)
  colindex <- colindex[1 <= colindex & colindex <= m]
  target <- rep(rowindex, length(colindex)) +
            (rep(colindex, each = length(rowindex)) - 1) * n
  return(target[mt[target]])
}

count.ponding <- function(lake, N, M) {
  if (!is(N, "vector") | length(N) != 1L | any(N < 1, N > 100) |
      !is(M, "vector") | length(M) != 1L | any(M < 1, M > 100) |
      !is(lake, "matrix") | any(nrow(lake) != N, ncol(lake) != M)) {
    stop("bad arguments")
  }

  lakeTF <- lake == "W"
  index <- which(lakeTF)
  current <- integer(0)
  count <- 0

  while (length(index) > 0) {
    if (length(current) == 0) {
      current <- index[1]
      count <- count + 1
    } else {
      current <- unlist(lapply(current, search.around, lakeTF))
    }
    
    if (any(current %in% index)) {
      lakeTF[current] <- F
      index <- setdiff(index, current)
    } else {
      current <- integer(0)
    }
  }

  return(count)
}

lake1 <- matrix(c("W", ".", ".",
                  "W", "W", "W",
                  ".", ".", "W"),
                nrow = 3, ncol = 3, byrow = T) # ponding = 1
lake2 <- matrix(c("W", ".", ".", "W",
                  ".", ".", "W", "."),
                nrow = 2, ncol = 4, byrow = T)# ponding = 2
lake3 <- matrix(c("W", ".", ".", ".", ".", ".", ".", ".", ".", "W", "W", ".",
                  ".", "W", "W", "W", ".", ".", ".", ".", ".", "W", "W", "W",
                  ".", ".", ".", ".", "W", "W", ".", ".", ".", "W", "W", ".",
                  ".", ".", ".", ".", ".", ".", ".", ".", ".", "W", "W", ".",
                  ".", ".", ".", ".", ".", ".", ".", ".", ".", "W", ".", ".",
                  ".", ".", "W", ".", ".", ".", ".", ".", ".", "W", ".", ".",
                  ".", "W", ".", "W", ".", ".", ".", ".", ".", "W", "W", ".",
                  "W", ".", "W", ".", "W", ".", ".", ".", ".", ".", "W", ".",
                  ".", "W", ".", "W", ".", ".", ".", ".", ".", ".", "W", ".",
                  ".", ".", "W", ".", ".", ".", ".", ".", ".", ".", "W", "."),
                nrow = 10, ncol = 12, byrow = T) # ponding = 3


count.ponding(lake1, 3, 3) # OK
count.ponding(lake2, 2, 4) # OK
count.ponding(lake3, 10, 12) # OK

count.ponding(lake1, 3, 4) # NG
count.ponding(lake2, 3, 4) # NG
count.ponding(lake3, 0, 101) #NG
