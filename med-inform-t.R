d6 <- sample(x = 1:6, size = 10, replace = TRUE)

data_vector <- d6

population_mean <- data_vector |> mean()

# empty vector
sample_means <- vector(mode = "numeric",
                       length = 10)

# random sampling
for (i in seq_along(data_vector)) {
  sample_means[i] <- data_vector |>
    sample(replace = TRUE)
}

# generate tibble
sample_tbl <- sample_means |> tibble::as_tibble()

# from ani.lln
for (i in 1:n) {
  d = colMeans(matrix(replicate(np, FUN(i, mu)), i))
  m = c(m, d)
  x = rbind(x, range(d))
}