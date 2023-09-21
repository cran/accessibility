## ----include = FALSE----------------------------------------------------------
Sys.setenv(OMP_THREAD_LIMIT = 2)

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
ci <- concentration_index(
  access,
  sociodemographic_data = land_use_data,
  opportunity = "jobs",
  population = "population",
  income = "income_per_capita",
  type = "corrected"
)
ci

## -----------------------------------------------------------------------------
theil_without_groups <- theil_t(
  access,
  sociodemographic_data = land_use_data,
  opportunity = "jobs",
  population = "population"
)
theil_without_groups

# some cells are classified as in the decile NA because their income per capita
# is NaN, as they don't have any population. we filter these cells from our
# accessibility data, otherwise the output would include NA values (note that
# subsetting the data like this doesn't affect the assumption that groups are
# completely exhaustive, because cells with NA income decile don't have any
# population)

na_decile_ids <- land_use_data[is.na(land_use_data$income_decile), ]$id
no_na_access <- access[! access$id %in% na_decile_ids, ]
sociodem_data <- land_use_data[! land_use_data$id %in% na_decile_ids, ]

theil_with_groups <- theil_t(
  no_na_access,
  sociodemographic_data = sociodem_data,
  opportunity = "jobs",
  population = "population",
  socioeconomic_groups = "income_decile"
)
theil_with_groups

## -----------------------------------------------------------------------------
poverty <- fgt_poverty(
  access,
  sociodemographic_data = land_use_data,
  opportunity = "jobs",
  population = "population",
  poverty_line = 95368
)
poverty

