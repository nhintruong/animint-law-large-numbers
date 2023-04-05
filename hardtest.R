
## set up packages

library(knitr)        # tidy file conversion
library(dplyr)        # tidy file manipulation
library(readr)        # tidy file reader
library(tibble)       # tidy tables
library(animint2)     # animated ggplot2
library(testthat)     # testing



## data wrangling

# get rid of unneeded columns
b5 <- read_tsv("b5.tsv",
               show_col_types = FALSE)
go_away <- c("race", "age", "engnat", "gender", "hand", "source")
b5c <- b5 |> select(-any_of(go_away))

# leave only 6 countries
six_countries <- b5c |> filter(country %in% c("US", "GB", "IN", "AU", "CA", "PH"))

# add ids
plus_id <- rowid_to_column(six_countries, "id") |> rowwise(id)

# generate sumscores and remove columns containing individual items
plus_sumscores <- plus_id |> 
  mutate(extraversion = sum(across(starts_with("E"))),
         neuroticism = sum(across(starts_with("N"))),
         agreeableness = sum(across(starts_with("A"))),
         conscientiousness = sum(across(starts_with("C", ignore.case = FALSE))),
         openness = sum(across(starts_with("O"))))
sumscores <- plus_sumscores |> ungroup() |>
  select(id, country, extraversion:openness)



## generate animint()

correlations <- sumscores |> 
  ggplot(mapping = aes(x = conscientiousness,
                       y = extraversion,
                       color = country))
corr <- correlations + geom_jitter(alpha = 0.25)

ani_corr <- animint(corr)
ani_corr$first <- list(country = "US")





# testing

test_that("scatterplot generates error message", {
  expect_warning(ani_corr, "position=identity")
})
