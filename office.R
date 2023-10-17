office_questions <- list(
  OF1 = list(
    label = "Ecoprovince",
    response_type = "multi-response",
    valid_responses = c(
      "Georgia Depression (GD)" = 1,
      "Coast & Mountains (CM); only the Skeena Region" = 2,
      "Southern Interior Mountains (SIM)" = 3,
      "Boreal Plains (BP)" = 4,
      "Taiga Plains (TP)" = 5
    ),
    used_by = c(),
    value = 1
  ),
  OF2 = list(
    label = "Distance to community ",
    response_type = "multi-response",
    valid_responses = c(
      "<100 m" = 1,
      "100 - 500 m" = 2,
      "0.5 - 1 km" = 3,
      "1 - 5 km" = 4,
      "> 5 km" = 5
    ),
    used_by = c("FR", "FH", "MB", "CRI"),
    value = 5
  ),
  OF3 = list(
    label = "Distance to Frequenty travelled Road",
    response_type = "multi-response",
    valid_responses = c(
      "<10 m" = 1,
      "10 - 25 m" = 2,
      "25 - 50 m" = 3,
      "50 - 100 m" = 4,
      "100 - 500 m" = 5,
      ">500 m" = 6
    ),
    used_by = c("NR", "FH", "AM", "CRI"),
    value = 6
  ),
  OF4 = list(
    label = "Distance to Ponded water ",
    response_type = "multi-response",
    valid_responses = c(
      "<100 m, and not separated by any width of paved roads, bare ground, or impervious surface. " = 1,
      "<100 m, but completely separated by those features." = 2,
      "100 m-1 km, and not separated." = 3,
      "100 m-1 km, but separated by those features." = 4,
      "1 - 5 km, and not separated." = 5,
      "1 - 5 km, but separated by those features." = 5,
      "None of the above (the closest patches or corridors that large are 5 km away)" = 7
    ),
    used_by = c("AM", "WB", "MB", "RSB", "Sens"),
    value = 4
  ),
  OF5 = list(
    label = "Distance to large ponded water ",
    response_type = "multi-response",
    valid_responses = c(
      "<100 m" = 1,
      "100 m - 1 km" = 2,
      "1 -2 km" = 3,
      "2-5 km" = 4,
      "5-10 km" = 5,
      ">10 km" = 6
    ),
    used_by = c("WB"),
    value = 6
  ),
  OF6 = list(
    label = "Relative Elevation in watershed ",
    response_type = "numeric",
    valid_responses = range(0, 1),
    used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "WB", "Sens"),
    value = 0.55
  ),
  OF7 = list(
    label = "Stream Intersect ",
    response_type = "binary",
    valid_responses = c(TRUE, FALSE),
    used_by = c("WS", "SFTS", "SR", "PR", "NR", "CS", "OE", "APP", "FR", "FH", "PD", "POL", "Sens"),
    value = FALSE
  )
)
