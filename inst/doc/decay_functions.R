## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = requireNamespace("ggplot2", quietly = TRUE), out.width = "80%", fig.width = 6, fig.height = 6----
library(accessibility)
library(data.table)
library(ggplot2)

binary <- decay_binary(cutoff = 50)
linear <- decay_linear(cutoff = 50)
negative_exp <- decay_exponential(decay_value = 0.2)
inverse_power <- decay_power(decay_value = 0.2)
stepped <- decay_stepped(steps = c(30, 60, 90), weights = c(0.66, 0.33, 0))

travel_costs <- seq(1, 100, 0.1)

weights <- data.table(
  minutes = travel_costs,
  binary = as.numeric(binary(travel_costs)),
  linear = linear(travel_costs),
  negative_exp = negative_exp(travel_costs),
  inverse_power = inverse_power(travel_costs),
  stepped = stepped(travel_costs)
)

# reshape data to long format
weights <- melt(
  weights,
  id.vars = "minutes",
  variable.name = "decay_function",
  value.name = "weights"
)

ggplot(weights) +
  geom_line(
    aes(minutes, weights, color = decay_function),
    show.legend = FALSE
  ) +
  facet_wrap(. ~ decay_function, ncol = 2) +
  theme_minimal()

## -----------------------------------------------------------------------------
my_decay <- function(travel_cost) {
  weights <- 1 / travel_cost
  weights[weights > 1] <- 1
  return(weights)
}

## -----------------------------------------------------------------------------
my_decay(c(0, 0.5, 1, 2, 5, 10))

## -----------------------------------------------------------------------------
data_dir <- system.file("extdata", package = "accessibility")

travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

custom_gravity <- gravity(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  decay_function = my_decay
)
head(custom_gravity)

## -----------------------------------------------------------------------------
power_gravity <- gravity(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  decay_function = decay_power(1)
)
head(power_gravity)

## -----------------------------------------------------------------------------
decay_power(1)

