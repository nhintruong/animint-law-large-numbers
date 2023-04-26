# Informal Testing for fn_lln


# Setup
set.seed(180)
library(animint2)
d6 <- sample(x = 1:6, size = 5, replace = TRUE)


# LLN function
fn_lln <- function(data_vector = d6, 
                   sampling = 500,
                   ylab = "sample mean"){
  
  # default data_vector
  d6 <- sample(x = 1:6, size = 500, replace = TRUE)
  
  # make sure dataset is a numeric vector
  validity_check <- data_vector |> 
    assertive::assert_is_numeric() |>
    assertive::is_non_empty() |>
    assertive::assert_all_are_not_na()
  
  # calculate population mean
  population_mean <- data_vector |> mean()
  
  # create empty vector to put sample means into
  sample_means <- vector(mode = "numeric",
                         length = sampling)
  
  # random sampling and mean generation
  for (i in 1:sampling) {
    sample_means[i] <- data_vector |> 
      sample(size = i, replace = TRUE) |> 
      mean()
  }
  
  # generate n and a df to use for animint
  sample_df <- sample_means |> 
    data.frame() |>
    tibble::rowid_to_column(var = "n")
  
  # make lln plot
  lln_plot <- sample_df |> 
    ggplot() +
    aes(x = n, y = sample_df$sample_means) + 
    labs(y = ylab) +
    geom_point(showSelected = "n", colour = "red") +
    geom_line(alpha = 0.5) +
    geom_hline(yintercept = population_mean, 
               colour = "red")
  
  # animate lln plot
  ani_lln_plot <- animint(lln_plot)
  ani_lln_plot$time <- list(variable = "n", ms = 75)
  ani_lln_plot
  
}