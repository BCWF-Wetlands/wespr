source("R/functions.R")

core_questions <- list(
  list(
    name = "OF1",
    label = "Distance to Community",
    response_type = "multichoice",
    n_choices = 5,
    validator = multi_choice(n = 5),
    used_by = c("FR", "FH", "KMB", "CRI"), # ecosystem services that rely on this. Is this necessary?
    value = rep(FALSE, 5)
  ),
  list(
    name = "OF5",
    label = "Relative Elevation in watershed ",
    response_type = "numeric",
    validator = numeric_value(min = 0, max = 1),
    used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "WB", "Sens"),
    value = NA_real_
  ),
  list(
    name = "OF7",
    label = "Stream Intersect ",
    response_type = "multiresponse-binary",
    n_choices = 3,
    validator = multiresponse_binary(n = 3),
    used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "PD", "POL", "Sens"),
    value = rep(FALSE, 3)
  ),
  list(
    name = "OF24",
    label = "Species of Conservation Concern",
    response_type = "multiresponse-binary",
    n_choices = 4,
    validator = multiresponse_binary(n = 4),
    used_by = c("AM", "WB", "RSB", "PD", "Sens"),
    value = rep(FALSE, 4)
  ),
  list(
    name = "F1",
    label = "Vegetation Height and Form Diversity",
    response_type = "multiresponse-category",
    n_choices = 6,
    validator = multiresponse_category(n = 6, min = 0, max = 6),
    used_by = c("SFTS", "NR", "CS", "APP", "FR", "KMB", "RSB", "PD", "POL", "Sens"),
    value = rep(0, 6)
  )
)

derived_values = list(
  list(
    name = "always_water",
    calculated_from = "the question",
    response_type = "derived",
    validator = logical_value,
    used_by = c("services that use it"), 
    value = NA # calculated from relevant question.
  )
)

core_questions <- names_from_value(core_questions, "name")
derived_values <- names_from_value(derived_values, "name")

