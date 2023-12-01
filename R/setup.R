# core_questions <- function() {
#   core_questions <- list(
#     list(
#       q_no = "OF1",
#       label = "Distance to Community",
#       response_type = "multichoice",
#       n_choices = 5,
#       validator = multi_choice(n = 5),
#       used_by = c("FR", "FH", "KMB", "CRI"), # ecosystem services that rely on this. Is this necessary?
#       value = rep(NA, 5)
#     ),
#     list(
#       q_no = "OF5",
#       label = "Relative Elevation in watershed ",
#       response_type = "numeric",
#       validator = numeric_value(min = 0, max = 1),
#       used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "WB", "Sens"),
#       value = NA_real_
#     ),
#     list(
#       # TODO: This is actually OF6 but needs to be fixed in wetflat.csv
#       q_no = "OF7",
#       label = "Stream Intersect ",
#       response_type = "multiresponse-binary",
#       n_choices = 3,
#       validator = multiresponse_binary(n = 3),
#       used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "PD", "POL", "Sens"),
#       value = rep(NA, 3)
#     ),
#     list(
#       q_no = "OF24",
#       label = "Species of Conservation Concern",
#       response_type = "multiresponse-binary",
#       n_choices = 4,
#       validator = multiresponse_binary(n = 4),
#       used_by = c("AM", "WB", "RSB", "PD", "Sens"),
#       value = rep(NA, 4)
#     ),
#     list(
#       q_no = "F1",
#       label = "Vegetation Height and Form Diversity",
#       response_type = "multiresponse-category",
#       n_choices = 6,
#       validator = multiresponse_category(n = 6, min = 0, max = 6),
#       used_by = c("SFTS", "NR", "CS", "APP", "FR", "KMB", "RSB", "PD", "POL", "Sens"),
#       value = rep(0, 6)
#     ),
#     list(
#       q_no = "F2",
#       label = "Height Class Interspersion",
#       response_type = "multichoice",
#       n_choices = 4,
#       validator = multi_choice(n = 4),
#       used_by = c("RSB", "KMH", "PD"),
#       value = rep(0, 4),
#       response_no = c("F2_A1", "F2_A2", "F2_B1", "F2_B2")
#     ),
#     list(
#       q_no = "F19",
#       label = "% Always WITHOUT Surface Water",
#       response_type = "multichoice",
#       n_choices = 6,
#       validator = multi_choice(n = 6),
#       used_by = c("SFTS", "SR", "NR", "APP", "FH", "FR", "PD"),
#       value = rep(NA, 6)
#     )
#   )
#   names_from_value(core_questions, "q_no")
# }

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

