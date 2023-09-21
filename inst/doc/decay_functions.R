## ----include = FALSE----------------------------------------------------------
Sys.setenv(OMP_THREAD_LIMIT = 2)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(accessibility)

output_fn <- decay_exponential(c(0.2, 0.3))

output_fn(c(10, 15, 20))

## -----------------------------------------------------------------------------
stepped_output <- decay_stepped(
  steps = list(c(10, 20, 30), c(10, 20, 30, 40)),
  weights = list(c(0.67, 0.33, 0), c(0.75, 0.5, 0.25, 0))
)

stepped_output(c(15, 25, 35, 45))

## ----eval = requireNamespace("ggplot2", quietly = TRUE), out.width = "80%", fig.width = 6, fig.height = 6----
library(data.table)
library(ggplot2)

binary <- decay_binary(cutoff = 50)
linear <- decay_linear(cutoff = 50)
negative_exp <- decay_exponential(decay_value = 0.2)
inverse_power <- decay_power(decay_value = 0.2)
stepped <- decay_stepped(steps = c(30, 60, 90), weights = c(0.67, 0.33, 0))

travel_costs <- seq(1, 100, 0.1)

weights <- data.table(
  minutes = travel_costs,
  binary = as.numeric(binary(travel_costs)[["50"]]),
  linear = linear(travel_costs)[["50"]],
  negative_exp = negative_exp(travel_costs)[["0.2"]],
  inverse_power = inverse_power(travel_costs)[["0.2"]],
  stepped = stepped(travel_costs)[["s(30,60,90);w(0.67,0.33,0)"]]
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
my_second_decay <- function(decay_parameter) {
  function(travel_cost) {
    weights <- 1 / (decay_parameter * travel_cost)
    weights[weights > 1] <- 1
    return(weights)
  }
}

output_fn <- my_second_decay(2)
output_fn(c(0, 0.5, 1, 2, 5, 10))

# compare to the first custom decay function
my_decay(c(0, 0.5, 1, 2, 5, 10))

## -----------------------------------------------------------------------------
second_custom_gravity <- gravity(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  decay_function = my_second_decay(1)
)
head(second_custom_gravity)

## -----------------------------------------------------------------------------
decay_power(1)

## -----------------------------------------------------------------------------
my_third_decay <- function(decay_parameter) {
  function(travel_cost) {
    weighting_list <- lapply(
      decay_parameter,
      function(x) {
        weights <- 1 / (x * travel_cost)
        weights[weights > 1] <- 1
        return(weights)
      }
    )

    names(weighting_list) <- decay_parameter
    weighting_list
  }
}

output_fn <- my_third_decay(c(1, 2))
output_fn(c(0, 0.5, 1, 2, 5, 10))

# compare to the first and second custom decay functions

my_decay(c(0, 0.5, 1, 2, 5, 10))

my_second_decay(2)(c(0, 0.5, 1, 2, 5, 10))

## -----------------------------------------------------------------------------
third_custom_gravity <- gravity(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  decay_function = my_third_decay(c(1, 2))
)
third_custom_gravity

