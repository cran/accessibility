## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("accessibility")

## ---- eval = FALSE------------------------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("ipeaGIT/accessibility")

## ---- message = FALSE, warning = FALSE----------------------------------------
library(accessibility)
library(data.table)
library(ggplot2)
library(sf)

## -----------------------------------------------------------------------------
data_dir <- system.file("extdata", package = "accessibility")

travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
head(travel_matrix)

## -----------------------------------------------------------------------------
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
head(land_use_data)

## ---- message = FALSE---------------------------------------------------------
mtc <- cost_to_closest(
  travel_matrix,
  land_use_data,
  opportunity = "schools",
  travel_cost = "travel_time",
  n = 1
)
head(mtc)

## ---- message = FALSE---------------------------------------------------------
cum_cutoff <- cumulative_cutoff(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  cutoff = 30
)
head(cum_cutoff)

## ---- message = FALSE---------------------------------------------------------
passive_cum_cutoff <- cumulative_cutoff(
  travel_matrix,
  land_use_data,
  opportunity = "population",
  travel_cost = "travel_time",
  cutoff = 30,
  active = FALSE
)
head(passive_cum_cutoff)

## ---- message = FALSE---------------------------------------------------------
cum_interval <- cumulative_interval(
  travel_matrix = travel_matrix,
  land_use_data = land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  interval = c(40, 60)
)
head(cum_interval)

## ---- message = FALSE---------------------------------------------------------
negative_exp <- gravity(
  travel_matrix,
  land_use_data,
  opportunity = "schools",
  travel_cost = "travel_time",
  decay_function = decay_exponential(decay_value = 0.2)
)
head(negative_exp)

## ---- message = FALSE---------------------------------------------------------
bfca <- floating_catchment_area(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  demand = "population",
  method = "bfca",
  decay_function = decay_exponential(decay_value = 0.5)
)
head(bfca)

## ---- eval = requireNamespace(c("sf", "ggplot2"), quietly = TRUE), out.width = "80%", fig.width = 6, fig.height = 6----
grid <- system.file("extdata/grid_bho.rds", package = "accessibility")
grid <- readRDS(grid)

spatial_data <- merge(grid, cum_cutoff, by = "id")

ggplot() +
  geom_sf(data = spatial_data, aes(fill = jobs), color = NA) +
  labs(
    title = "Job accessibility by transit in under 30 min.",
    fill = "Accessible jobs"
  ) +
  scale_fill_viridis_c() +
  theme_void()

