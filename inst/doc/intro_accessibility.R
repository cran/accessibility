## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

old <- options()
options(scipen = 999)

## ---- eval = FALSE------------------------------------------------------------
#  # CRAN
#  install.packages('accessibility')
#  
#  # github
#  devtools::install_github("ipeaGIT/accessibility", subdir = "r-package")

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(accessibility)
library(data.table)
library(ggplot2)
library(sf)

## -----------------------------------------------------------------------------
data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
ttm <- readRDS(data_path)
head(ttm)


## ---- message = FALSE---------------------------------------------------------
tmi <- time_to_closest(data = ttm,
         opportunity_col = 'schools',
         n_opportunities = 1,
         travel_cost_col = 'travel_time',
         by_col = 'from_id'
         )

head(tmi)

## ---- message = FALSE---------------------------------------------------------
cum_cutoff <- cumulative_time_cutoff(
                data = ttm,
                cutoff = 30,
                opportunity_col = 'jobs',
                travel_cost_col = 'travel_time',
                by_col = 'from_id'
                )

head(cum_cutoff)

## ---- message = FALSE---------------------------------------------------------
cum_interval <- cumulative_time_interval(
                  data = ttm,
                  interval = c(20, 60),
                  stat = 'mean',
                  opportunity_col = 'jobs',
                  travel_cost_col = 'travel_time',
                  by_col = 'from_id'
                  )

head(cum_interval)

## ---- message = FALSE---------------------------------------------------------
grav_exp <- gravity_access(
              data = ttm,
              opportunity_col = 'jobs',
              decay_function = decay_exponential(decay_value = 0.2),
              travel_cost_col = 'travel_time',
              by_col = 'from_id'
              )

head(grav_exp)

## ---- message = FALSE---------------------------------------------------------
fca_2sfca <- floating_catchment_area(
                data = ttm,
                fca_metric = '2SFCA',
                population_col = 'population', 
                opportunity_col = 'schools',
                decay_function = decay_exponential(decay_value = 0.2),
                travel_cost_col = 'travel_time',
                orig_col = 'from_id',
                dest_col = 'to_id'
                )

head(fca_2sfca)

## ---- message = FALSE---------------------------------------------------------
fca_bfca <- floating_catchment_area(
                data = ttm,
                fca_metric = 'BFCA',
                population_col = 'population', 
                opportunity_col = 'schools',
                decay_function = decay_exponential(decay_value = 0.2),
                travel_cost_col = 'travel_time',
                orig_col = 'from_id',
                dest_col = 'to_id'
                )

head(fca_bfca)

## ---- out.width='100%', fig.width=6, fig.height=6-----------------------------
library(ggplot2)
library(sf)
library(data.table)

# load spatial data
grid <- system.file("extdata/grid_bho.rds", package = "accessibility")
grid <- readRDS(grid)

# merge accessibility output to spatial data
df <- merge(grid, cum_cutoff, by.x='id', by.y='from_id')

# for large data sets, this can be done much faster with `data.table` as follows:
df2 <- setDT(grid)[cum_cutoff, on=c('id'='from_id'), access := i.access]
df2 <- st_sf(df2, crs = 4326)


# plot
ggplot() +
  geom_sf(data = df2, aes(fill = access), color=NA, alpha=0.9) +
  labs(title = 'Employment accessibility by transit in under 30 min.', 
       fill='Number of jobs\naccessible') +
  scale_fill_viridis_c() + 
  theme_void()


## ---- include = FALSE---------------------------------------------------------
options(old)

