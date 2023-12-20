#' Convert a data.frame of WESP question responses to a `wesp_site`
#'
#' Read a `data.frame` that has been read in by [load_wesp_data()]
#' and convert it into a `wesp_site` object, in preparation for
#' calculating the indicators. This function also calculates various
#' "derived" values that are used in indicator calculations.
#'
#' @param data `data.frame` of questions and responses, ideally created by
#'   reading in data with [load_wesp_data()]. Contains columns `q_no`,
#'   `response_no`, and one or more `site_[x]` columns.
#' @param site A number, or the name of a column in `data` indicating which site to
#'   calculate, if more than one site in `data`. Defaults to the
#'   first `site_[x]` column.
#'
#' @return An object of type `wesp_site`
#' @export
as.wesp_site <- function(data, site = NULL) {

  site <- site %||% setdiff(names(data), c("q_no", "response_no"))[1]

  if (!is.numeric(site) && !site %in% names(data)) {
    stop("Invalid site specified. Must be a number or the name of a column in 'data'",
         call. = FALSE)
  }

  if (is.numeric(site)) {
    site <- setdiff(names(data), c("q_no", "response_no"))[site]
  }

  questions <- record_values(data, site = site)
  derived_values <- make_derived_values(questions)
  site <- list(
    site_name = site,
    questions = questions,
    derived_values = derived_values,
    indicators = indicators()
  )
  class(site) <- "wesp_site"
  site
}

make_core_questions <- function() {
  q_list <- lapply(split(question_metadata, question_metadata$no), as.list)

  format_q_list <- function(q) {
    # convert the columns of indicators that have a value (f,b,f/b)
    # into a vector containing the indicators the question pertains to
    q$used_by <- Filter(Negate(is.na), unlist(q[names(indicators())]))
    q$no_indicators <- length(q$used_by)

    # get rid of the original indicator columns since they're now stored
    # in `used_by`
    q <- q[setdiff(names(q), names(indicators()))]

    # An empty vector to hold responses
    if (length(q$n_responses) > 0 && !is.na(q$n_responses)) {
      q$value <- rep(NA, q$n_responses)
    }

    # get the validator function by matching the type with
    # the validator list
    q$validator = validators[[q$type]](q$no, q$n_responses)

    q
  }

  lapply(q_list, format_q_list)
}


#' Calculate derived values from question responses
#'
#' Examples are `all_water`, `never_water`, etc.
#'
#' @param cq an object resulting from running [record_values()]
#'
#' @return A named numeric vector containing derived values.
#' @noRd
make_derived_values <- function(cq) {
  derived_vals <- unname(lapply(cq, function(q) {
    if (!is.na(q$unique_values)) {
      extract_unique_values(q)
    }
  }))
  unlist(derived_vals)
}

extract_unique_values <- function(x) {
  # The unique values are specified in two columns, the first (unique_values)
  # denoting the names of the values derived from that question, separated
  # by commas (e.g., Moose,Bear,... etc).
  # The second (unique_value_response) are the R statements (separated by commas)
  # that make that derived value
  # (almost all Q==1)
  unique_vals <- trimws(strsplit(x$unique_values, split = ";")[[1]])
  unique_resps <- trimws(strsplit(x$unique_value_response, split = ";")[[1]])

  # parse and evaluate the expression for each in the "value" environment.
  # This includes calculating stressor sums and subscores
  is_val_true <- lapply(unique_resps, function(resp) {
    cll <- str2lang(resp)
    eval(cll, envir = x$value)
  })

  stats::setNames(is_val_true, unique_vals)
}

indicators <- function() {
  list(
    ws = list(fun = NULL, ben = NULL),
    sr = list(fun = NULL, ben = NULL),
    pr = list(fun = NULL, ben = NULL),
    cs = list(fun = NULL),
    fr = list(fun = NULL, ben = NULL),
    sens = list(fun = NULL),
    str = list(fun = NULL),
    nr = list(fun = NULL, ben = NULL),
    ap = list(fun = NULL, ben = NULL),
    pd = list(fun = NULL, ben = NULL),
    kmh = list(fun = NULL, ben = NULL),
    wb = list(fun = NULL, ben = NULL),
    pol = list(fun = NULL, ben = NULL),
    rsb = list(fun = NULL, ben = NULL),
    oe = list(fun = NULL),
    am = list(fun = NULL, ben = NULL),
    fh = list(fun = NULL, ben = NULL),
    sfts = list(fun = NULL, ben = NULL),
    cri = list(ben = NULL)
  )
}

update_site_indicator <- function(site, indicator, type = c("fun", "ben")) {
  check_wesp_site(site)

  if (!indicator %in% names(indicators()) ||
      !exists(indicator, where = site$indicators)) {
    stop("Invalid indicator: ", indicator, call. = FALSE)
  }

  type <- match.arg(type)

  if (!exists(type, where = site$indicators[[indicator]])) {
    stop("'", type, "' is not a valid type for indicator '", indicator, "'",
         call. = FALSE)
  }

  # This is a bit fragile - make the name of the function from indicator and type
  # args, and call it with do.call:
  indicator_fun <- paste(indicator, type, sep = "_")
  value <- do.call(indicator_fun, list(site = site))

  existing_value <- site$indicators[[indicator]][[type]]

  if (!is.null(existing_value)) {
    warning("'", indicator, ":", type, "' has exisiting value: ", existing_value, ". ",
            "Replacing it with ", round(value, 2))
  }

  site$indicators[[indicator]][[type]] <- value

  site
}
