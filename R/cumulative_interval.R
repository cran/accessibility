#' Cumulative access based on maximum travel time interval
#'
#' Calculates the average or median number of opportunities that can be reached
#' considering multiple maximum travel cost thresholds within a given travel
#' cost interval specified by the user. The time interval cumulative
#' accessibility measures was originally proposed by
#' \insertCite{tomasiello2023time;textual}{accessibility}.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @param interval A `numeric` vector of length 2. Indicates the start and end
#'   points of the interval of travel cost thresholds to be used. The first
#'   entry must be lower than the second.
#' @param interval_increment A `numeric`. How many travel cost units separate
#'   the cutoffs used to calculate the accessibility estimates which will be
#'   used to calculate the summary estimate within the specified interval.
#'   Should be thought as the resolution of the distribution of travel costs
#'   within the interval. Defaults to 1.
#' @param summary_function A function. This function is used to summarize a
#'   distribution of accessibility estimates within a travel cost interval as a
#'   single value. Can be any function that takes an arbitrary number of
#'   numeric values as as input and returns a single number as output. Defaults
#'   to [stats::median()].
#' @template group_by
#' @template active
#'
#' @template return_accessibility
#'
#' @family cumulative access
#'
#' @references
#' \insertAllCited{}
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' df <- cumulative_interval(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   interval = c(20, 30),
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df)
#'
#' df <- cumulative_interval(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   interval = c(40, 80),
#'   opportunity = "jobs",
#'   travel_cost = "travel_time"
#' )
#' head(df)
#'
#' @export
cumulative_interval <- function(travel_matrix,
                                land_use_data,
                                opportunity,
                                travel_cost,
                                interval,
                                interval_increment = 1,
                                summary_function = stats::median,
                                group_by = character(0),
                                active = TRUE) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(travel_cost)
  checkmate::assert_logical(active, len = 1, any.missing = FALSE)
  assert_interval_increment(interval_increment)
  assert_summary_function(summary_function)
  assert_group_by(group_by)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(
    land_use_data,
    travel_matrix,
    opportunity,
    active = active
  )

  interval <- assert_and_assign_interval(interval)

  # if not a dt, keep original class to assign later when returning result

  if (!inherits(travel_matrix, "data.table")) {
    original_class <- class(travel_matrix)
    data <- data.table::as.data.table(travel_matrix)
  } else {
    data <- data.table::copy(travel_matrix)
  }

  if (!inherits(land_use_data, "data.table")) {
    land_use_data <- data.table::as.data.table(land_use_data)
  }

  cutoffs <- lapply(
    interval,
    function(pair) seq.int(pair[1], pair[2], by = interval_increment)
  )
  cutoffs <- unique(unlist(cutoffs))
  cutoffs <- cutoffs[order(cutoffs)]
  names(cutoffs) <- as.character(cutoffs)

  # small optimization: we can ditch anything that costs more than the highest
  # cost cutoff value, since it won't affect the results anyway
  .cost_colname <- travel_cost
  data <- data[get(.cost_colname) <= max(cutoffs)]

  merge_by_reference(
    data,
    land_use_data,
    opportunity,
    left_df_idcol = ifelse(active, "to_id", "from_id")
  )

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, group_by)
  env <- environment()

  warn_extra_cols(travel_matrix, travel_cost, group_id, groups)

  .opportunity_colname <- opportunity
  access_list <- lapply(
    cutoffs,
    function(x) {
      data[
        get(.cost_colname) <= x,
        .(access = sum(get(.opportunity_colname))),
        by = eval(groups, envir = env)
      ]
    }
  )

  # some of the estimates for lower thresholds may have less number of rows
  # because some origins may not reach any destinations when the travel cost
  # restrictions are harder. in such case, we have to fill these datasets with
  # the missing combinations having accessibility 0, otherwise the summary
  # measure would be miscalculated (e.g. think of median of 1 3 5 and median of
  # 0 1 3 5)

  nrow_more_restrictive <- nrow(access_list[[1]])
  nrow_less_restrictive <- nrow(access_list[[length(access_list)]])

  if (nrow_more_restrictive < nrow_less_restrictive) {
    access_list <- fill_access_list(access_list, travel_matrix, groups)
  }
  access_list <- data.table::rbindlist(access_list, idcol = "cutoffs")
  access_list[, cutoffs := as.numeric(cutoffs)]

  # the as.integer() call below makes sure that, even though the summary
  # function may return doubles, the result is an integer (afterall when using
  # a cumulative measure you cannot reach half of an opportunity) and that the
  # float is rounded down to the integer (if you reach 2.8 opportunities you do
  # reach 2, but you can't reach 3)

  access_list <- lapply(
    interval,
    function(pair) {
      access_list[
        cutoffs >= pair[1] & cutoffs <= pair[2],
        .(access = as.integer(summary_function(access))),
        by = eval(groups, envir = env)
      ]
    }
  )
  access <- data.table::rbindlist(access_list, idcol = "interval")
  data.table::setcolorder(access, c(groups, "interval", "access"))
  data.table::setnames(access, c(group_id, "access"), c("id", opportunity))

  if (length(interval) == 1) access[, interval := NULL]

  if (exists("original_class")) class(access) <- original_class

  return(access[])
}


#' @keywords internal
fill_access_list <- function(access, travel_matrix, groups) {
  unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
  names(unique_values) <- groups
  possible_combinations <- do.call(data.table::CJ, unique_values)

  access_nrows <- vapply(access, nrow, integer(1))
  possible_nrows <- nrow(possible_combinations)
  should_fill <- access_nrows < possible_nrows

  filled_access <- mapply(
    do_fill = should_fill,
    access_df = access,
    SIMPLIFY = FALSE,
    FUN = function(do_fill, access_df) {
      if (!do_fill) return(access_df)

      filled_access_df <- do_fill_missing_ids(
        access_df,
        possible_combinations,
        groups
      )

      return(filled_access_df[])
    }
  )

  return(filled_access)
}
