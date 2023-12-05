make_core_questions <- function() {
  core_questions <- readr::read_csv(
    system.file("input_data/all_indicators.csv", package = "wespr"),
    col_types = readr::cols(
      n_responses = readr::col_integer(),
      no_indicators = readr::col_skip(),
      .default = readr::col_character()
    )
  ) |>
    dplyr::filter(no != "score")

  q_list <- lapply(split(core_questions, core_questions$no), as.list)

  format_q_list <- function(q) {
    # convert the columns of indicators that have a value (f,b,f/b)
    # into a vector containing the indicators the question pertains to
    q$used_by <- Filter(Negate(is.na), unlist(q[indicator_names()]))
    q$no_indicators <- length(q$used_by)

    # get rid of the original indicator columns since they're now stored
    # in `used_by`
    q <- q[setdiff(names(q), indicator_names())]

    # An empty vector to hold responses
    if (length(q$n_responses) > 0 && !is.na(q$n_responses)) {
      q$value <- rep(NA, q$n_responses)
    }

    # get the validator function by matching the type with
    # the validator list
    q$validator = validators[[q$type]](q$no, q$n_responses)

    if (grepl("^S", q$no)) {
      q$sum <- NA_integer_
      q$stressor_subscore <- NA_integer_
    }
    q
  }

  lapply(q_list, format_q_list)
}

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
  # The second (unique_value_response) are the conditions (as logical R
  # statements separated by commas) that make that derived value TRUE or FALSE
  # (almost all Q==1)
  unique_vals <- strsplit(x$unique_values, split = ",")[[1]]
  unique_resps <- strsplit(x$unique_value_response, split = ",")[[1]]

  # parse and evaluate the logical expression for each
  is_val_true <- vapply(unique_resps, function(resp) {
    eval(parse(text = paste0("x$value$", resp)))
  },
  FUN.VALUE = logical(1))

  stats::setNames(is_val_true, unique_vals)
}

indicator_names <- function() {
  c("ws", "sfts", "sr", "pr", "nr", "cs", "oe", "app", "fr", "fh",
    "am", "wb", "kmh", "rsb", "pd", "pol", "cri", "sens", "str")
}

