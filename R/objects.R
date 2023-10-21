source("R/functions.R")

core_questions <- list(
  list(
    name = "OF1",
    label = "Distance to Community",
    response_type = "multichoice",
    n_choices = 5,
    valid_responses = 0:1,
    used_by = c("FR", "FH", "KMB", "CRI"), # ecosystem services that rely on this. Is this necessary?
    value = c(0, 0, 0, 0, 0)
  ),
  list(
    name = "OF24",
    label = "Species of Conservation Concern",
    response_type = "multiresponse-binary",
    n_choices = 4,
    valid_responses = c(TRUE, FALSE),
    used_by = c("AM", "WB", "RSB", "PD", "Sens"),
    value = c(FALSE, FALSE, FALSE, FALSE) # come in as 0/1, convert to logical??
  ),
  list(
    name = "F1",
    label = "Vegetation Height and Form Diversity",
    response_type = "multiresponse-category",
    n_choices = 6,
    valid_responses = 0:6,
    used_by = c("SFTS", "NR", "CS", "APP", "FR", "KMB", "RSB", "PD", "POL", "Sens"),
    value = c(0, 0, 0, 0, 0, 0)
  ),
  list(
    name = "OF5",
    label = "Relative Elevation in watershed ",
    response_type = "numeric",
    valid_responses = check_numeric, # a validator function?
    used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "WB", "Sens"),
    value = NA_real_
  )
)

derived_values = list(
  list(
    name = "always_water",
    calculated_from = "the question",
    response_type = "derived",
    valid_response = c(TRUE, FALSE),
    used_by = c("services that use it"), 
    value = NA # calculated from relevant question.
  )
)

core_questions <- names_from_value(core_questions, "name")
derived_values <- names_from_value(derived_values, "name")

# # TODO: validator functions. They should flag which question is invalid, and why.
# ,
#   OF7 = list(
#     label = "Stream Intersect ",
#     response_type = "binary",
#     valid_responses = c(TRUE, FALSE),
#     used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "PD", "POL", "Sens"),
#     value = FALSE
#   )
# )
