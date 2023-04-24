set.seed(135)
d6 <- sample(x = 1:6, size = 1000, replace = TRUE)

data_vector <- d6

population_mean <- data_vector |> mean()

# empty vector
sample_means <- vector(mode = "numeric",
                       length = 1000)

# random sampling
for (i in seq_along(data_vector)) {
  sample_means[i] <- data_vector |> 
    sample(size = i, replace = TRUE) |> 
    mean()
}

plot(sample_means)

# generate tibble
sample_tbl <- sample_means |> tibble::as_tibble()