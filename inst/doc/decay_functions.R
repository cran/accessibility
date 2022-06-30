## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(accessibility)
library(data.table)
library(ggplot2)

# Generate inputs
vec <- 0:100
decay_value <- 0.2
cutoff <- 50

# Return functions
step        <- decay_binary(cutoff=cutoff)
linear      <- decay_linear(cutoff=cutoff)
exponential <- decay_exponential(decay_value=decay_value)
power       <- decay_power(decay_value = decay_value)

df <- data.table(minutes       = vec,
                 binary        = step(vec),
                 linear        = linear(vec),
                 exponential   = exponential(vec),
                 inverse_power = power(vec)
                 )

# reshape the data to long format
df2 <- data.table::melt.data.table(data = df, 
                                   id.vars = 'minutes', 
                                   variable.name = 'decay_function', 
                                   value.name = 'impedance_factor')

# plot
ggplot() +
  geom_line(data=df2, aes(x=minutes, y=impedance_factor, color=decay_function), show.legend = FALSE) +
  facet_wrap(.~decay_function, ncol = 2) +
  theme_minimal()


## ---- message=FALSE, warning=FALSE--------------------------------------------

# load input data
data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
ttm <- readRDS(data_path)
head(ttm)


## -----------------------------------------------------------------------------
# create custom decay function
custom_decay_fun <- function(k) {
  impedance <- function(t_ij) {
    f <- 1 / t_ij ^ k
    return(f)
  }
}

## ---- message = FALSE---------------------------------------------------------
# calculate gravity-based accessibility
grav_custom <- gravity_access(
                  data = ttm,
                  opportunity_col = 'jobs',
                  decay_function = custom_decay_fun(k=0.5),
                  travel_cost_col = 'travel_time',
                  by_col = 'from_id'
                  )

head(grav_custom)

