core_questions <- function() {
  core_questions <- list(
    list(
      q_no = "OF1",
      label = "Distance to Community",
      response_type = "multichoice",
      n_choices = 5,
      validator = multi_choice(n = 5),
      used_by = c("FR", "FH", "KMB", "CRI"), # ecosystem services that rely on this. Is this necessary?
      value = rep(NA, 5)
    ),
    list(
      q_no = "OF5",
      label = "Relative Elevation in watershed ",
      response_type = "numeric",
      validator = numeric_value(min = 0, max = 1),
      used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "WB", "Sens"),
      value = NA_real_
    ),
    list(
      # TODO: This is actually OF6 but needs to be fixed in wetflat.csv
      q_no = "OF7",
      label = "Stream Intersect ",
      response_type = "multiresponse-binary",
      n_choices = 3,
      validator = multiresponse_binary(n = 3),
      used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "PD", "POL", "Sens"),
      value = rep(NA, 3)
    ),
    list(
      q_no = "OF24",
      label = "Species of Conservation Concern",
      response_type = "multiresponse-binary",
      n_choices = 4,
      validator = multiresponse_binary(n = 4),
      used_by = c("AM", "WB", "RSB", "PD", "Sens"),
      value = rep(NA, 4)
    ),
    list(
      q_no = "F1",
      label = "Vegetation Height and Form Diversity",
      response_type = "multiresponse-category",
      n_choices = 6,
      validator = multiresponse_category(n = 6, min = 0, max = 6),
      used_by = c("SFTS", "NR", "CS", "APP", "FR", "KMB", "RSB", "PD", "POL", "Sens"),
      value = rep(0, 6)
    ),
    list(
      q_no = "F2",
      label = "Height Class Interspersion",
      response_type = "multichoice",
      n_choices = 4,
      validator = multi_choice(n = 4),
      used_by = c("RSB", "KMH", "PD"),
      value = rep(0, 4),
      response_no = c("F2_A1", "F2_A2", "F2_B1", "F2_B2")
    ),
    list(
      q_no = "F19",
      label = "% Always WITHOUT Surface Water",
      response_type = "multichoice",
      n_choices = 6,
      validator = multi_choice(n = 6),
      used_by = c("SFTS", "SR", "NR", "APP", "FH", "FR", "PD"),
      value = rep(NA, 6)
    )
  )
  names_from_value(core_questions, "q_no")
}

empty_derived_values <- function() {
  derived_values = list(
    list(
      name = "never_water",
      calculated_from = "F19",
      # the [6] is the index of the sixth choice in q F19, indicating never
      # water. I don't love the use of this "magic number" here
      generator = \(x) x$F19$value[6],
      used_by = c("services that use it"),
      value = NA
    ),
    list(
      name = "all_water",
      calculated_from = "F19",
      # the [1] is the index of the fist choice in q F19, indicating always
      # water. I don't love the use of this "magic number" here
      generator = \(x) x$F19$value[1],
      used_by = c("services that use it"),
      value = NA
    )
  )

  names_from_value(derived_values, "name")
}

make_core_questions <- function() {
  core_questions <- readr::read_csv(
    system.file("input_data/all_indicators.csv", package = "wespr"),
    col_types = readr::cols(
      n_responses = readr::col_integer(),
      no_indicators = readr::col_integer(),
      .default = readr::col_character()
    )
  ) |>
    dplyr::filter(no != "score")

  q_list <- lapply(split(core_questions, core_questions$no), as.list)

  format_q_list <- function(q) {
    q$used_by <- Filter(Negate(is.na), unlist(q[indicator_names()]))
    q <- q[setdiff(names(q), indicator_names())]
    q$value <- rep(NA, q$n_responses)
  }

  lapply(q_list, format_q_list)
}

