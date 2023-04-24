library(animint2)

set.seed(120)
d6 <- sample(x = 1:6, size = 100, replace = TRUE)

data_vector <- d6

population_mean <- data_vector |> mean()

# empty vector
sample_means <- vector(mode = "numeric",
                       length = 100)

# random sampling
for (i in seq_along(data_vector)) {
  sample_means[i] <- data_vector |> 
    sample(size = i, replace = TRUE) |> 
    mean()
}

# generate dataframe
sample_df <- sample_means |> 
  data.frame() |>
  tibble::rowid_to_column(var = "n")

# generate list of dataframes
list_dfs

# plot construction
static_lln_plot <- sample_tbl |> 
  ggplot() +
  aes(x = n, y = sample_df$sample_means) + 
  labs(y = "sample mean") +
  geom_point(showSelected = "n", alpha = 0.5) +
  geom_hline(yintercept = population_mean, colour = "red")
static_lln_plot

# plot animation
ani_lln_plot <- animint(static_lln_plot)
ani_lln_plot$time <- list(variable = "n", ms = 200)
ani_lln_plot