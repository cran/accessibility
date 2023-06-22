## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(accessibility)

data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

access <- cumulative_cutoff(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  cutoff = 30
)
head(access)

## -----------------------------------------------------------------------------
palma <- palma_ratio(
  access,
  sociodemographic_data = land_use_data,
  opportunity = "jobs",
  population = "population",
  income = "income_per_capita"
)
palma

## -----------------------------------------------------------------------------
gini <- gini_index(
  access,
  sociodemographic_data = land_use_data,
  opportunity = "jobs",
  population = "population"
)
gini

## -----------------------------------------------------------------------------
poverty <- fgt_poverty(
  access,
  opportunity = "jobs",
  sociodemographic_data = land_use_data,
  population = "population",
  poverty_line = 95368
)
poverty

