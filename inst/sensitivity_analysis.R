## TESTING AND DEVELOPMENT SCRIPT

# This script is and internal developemnt script to assess sensitivity of the calculations
# within the wespr project.

# Use is for internal and develpoment only and not for pacakge use.

# NOte - this may require some changes to file inputs and outputs as dont want to interfere
# with the package (ie. temp folder. )


#Prepare for and install wespr
#devtools::install_github("BCWF-Wetlands/wespr")
#library(wespr)
load_all()
library(dplyr)
library(readr)

#read wesp data into wespr

wesp_file <- system.file("input_data/wetFlat_20250325.csv", package = "wespr")
data150 <- readr::read_csv(wesp_file)

# take a subset of the data
data150 <- data150[,1:28]
write.csv(data150, "temp/sensitivity_test.csv", row.names = FALSE)
wesp_file <- "temp/sensitivity_test.csv"


# run the base scores for comparison
wesp_data <- load_wesp_data(wesp_file)
base_score <- calculate_jenks_score(wesp_data, out_dir = "temp", out_name = "wesp_scores_base.csv")
base_score <- readr::read_csv("temp/wesp_scores_base.csv")

base_raw <- base_score |>
  select(site, any_of(ends_with("_raw"))) |>
  tidyr::pivot_longer(-site, names_to = "question", values_to = "value") |>
  mutate(type = "base")

ggplot2::ggplot(base_raw, ggplot2::aes(.data$value)) +
  ggplot2::geom_density(alpha = 0.2) +
  ggplot2::facet_wrap(~.data$question, scales = "free") +
  ggplot2::theme_bw()


# read in the base data
question_metadata
indicator_weightings

## types of questions
unique(question_metadata$type)



#110 questions total
# A tibble: 8 × 2
#type                      n
#<chr>                 <int>
# 1 binary                   11 # complete
# 2 category                  1
# 3 multi_choice             70 # complete
# 4 multi_choice_flexible     5 # complete
# 5 multi_choice_numeric      1
# 6 multiresponse_binary      6 # complete
# 7 multiresponse_numeric     8
# 8 numeric


# check which binary did not work and rerun
#
fls
# complete
mrb <- question_metadata[question_metadata$type == "multiresponse_binary",]$no #complete (n = 6)
binaryq <- question_metadata[question_metadata$type == "binary",]$no # complete (n = 11)
mrb <- question_metadata[question_metadata$type == "multi_choice",]$no # complete (n = 70)
mrb <- question_metadata[question_metadata$type == "multi_choice_flexible",]$no #(n = 5)



# still to do
mrb <- question_metadata[question_metadata$type == "numeric",]$no #(n = 5)
# F46a, F46

mrb <- question_metadata[question_metadata$type == "category",]$no #(n = 5)
# OF29  - c("T", "L", "D", "U", "F", "M", "C")

mrb <- question_metadata[question_metadata$type == "multi_choice_numeric",]$no
# F45 - ph (number of 2x binary) - weird mix


mrb <- question_metadata[question_metadata$type == "multiresponse_numeric",]$no
# F1  (0,1,2,3,4,5) # large!
# OF20 (0,1,2,3) - but skewed so not all values possible?

## + Stressors 1, 2, 3, 4, 5, 6,



#origin <- readr::read_csv("temp/sensitivity_test.csv", show_col_types = FALSE)#
#
#range <- origin |>
#  filter(Question == "S1") |>
#  as.numeric()
#
# min(range, na.rm = TRUE)
# max(range, na.rm = TRUE)
# mean(range, na.rm = TRUE)





#############################################################################
# Numeric response

#############################################################################

mrb <- question_metadata[question_metadata$type == "numeric",]$no
#mrb <- question_metadata[question_metadata$type == "multi_choice_flexible",]$no #(n = 5)
mrb <- question_metadata[question_metadata$type == "category",]$no #(n = 1) #OF29 - not working


## loop through the options
out <- purrr::map(mrb, function(x){

  x <- mrb[1]

  if(x == "OF5"){
    vec <- c(0.01,  0.02, 0.03, 0.04, 0.05,  0.06,  0.07,  0.08,  0.09, 0.095)
  } else if(x == "OF35"){
    vec <- c(0.2, 0.4, 0.6, 0.8, 1)
  } else if(x == "OF25"){   # OF25 - local moisture (range from 114 - 360 )
    vec <- c(100, 150, 200, 250, 300, 350, 400)
  } else if(x == "OF26"){    # OF26 - Degree Days (range from 1360 - 2135)
    vec <- c(1000, 1250, 1500, 1750, 2000, 2250, 2500)
  } else if(x == "OF27"){   # OF27 - local solar inputs (range from)
    vec <- c(32, 32.5, 33, 33.5, 34, 34.5, 35, 35.5,36)
  } else if(x == "F46a"){
    vec <- c(0, 25, 50, 75, 100, 125, 150, 200, 250, 300, 350)
  } else if(x == "F46b"){
    vec <- c(0, 50, 75, 100, 150, 200, 300, 400, 500, 600, 700, 800)
  }else if(x == "F23"){
    vec <- c(0, 1)
  } else if (x == "OF29"){
    vec <- c("T", "L", "D", "U", "F", "M", "C")
  }


  noptions <- question_metadata[question_metadata$no == x,]$n_responses
  n_iterations <- noptions

  N   <- noptions
  #vec <- c(0, 1)
  lst <- lapply(numeric(N), function(x) vec)
  combos <- as.data.frame(as.matrix(expand.grid(lst)))

  # pair down the all combos options as there needs to be at least one option selected
#  combos$rs = rowSums(combos)
#  combos <- combos |>
#    #filter(rs %in% c(1,0)) |>    # for multi_choice_flexible
#    filter(rs == 1) |>          # for multi_choice_
#    select(-rs)

  # read in nad remove the values
  origin <- readr::read_csv("temp/sensitivity_test.csv", show_col_types = FALSE)
  nocols <- length(colnames(origin)[-1])
  allqs <-  origin$Question

  # remove the F56 from this data set
  #origin <- origin[-grep(paste0(x, "_"), origin$Question),]

  # remove rows where Question matches and starts with x
  origin <- origin[-grep(paste0("^", x, "_"), origin$Question),]

  #origin <- origin[-grep(x, origin$Question),]
  #allqs2 <-  origin$Question
  #setdiff(allqs, allqs2)

  iterate <- purrr::map(1:nrow(combos), function(i){
    #i <- 1
    combos[i,]
    origin.names <- origin[1,]

    # create three rows which are equal to the row of options and then add togther
    lines <- purrr::map(1:ncol(combos), function(j){
      #j <- 1
      coln <- paste0(x, "_", j)
      #outline <- c(coln, rep(combos[i,j], nocols))
      outline <- rbind(origin.names, c(coln, rep(combos[i,j], nocols)))
      outline[-1,]

    }) |> dplyr::bind_rows()

    # add the iteration to the main dataset
    outdf <- rbind(origin, lines)
    write_csv(outdf , fs::path("temp", paste0("sensitivity_test_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })


  # lets read in the files and determine the scores per output

  score <- purrr::map(1:nrow(combos), function(k){
    k <- 2
    iname <- fs::path("temp", paste0("sensitivity_test_", x,"_",k, ".csv"))

    wesp_file <- fs::path(iname)
    wesp_data <- load_wesp_data(wesp_file)

    site <- as.wesp_site(wesp_data)

    site <- calc_indicators(site)

    q0 <- calculate_multi_site(wesp_data) |>
      tidyr::pivot_longer(-site, names_to = "question", values_to ="value") |>
      mutate(type = k)

    q0
  })

  score1 <-  score |> dplyr::bind_rows()


  write_csv(score1, fs::path("temp", paste0("sensitivity_test_", x,"_scores.csv")))

}) # end of loop through questions







#############################################################################
# Multi-choice

#############################################################################

mrb <- question_metadata[question_metadata$type == "multi_choice",]$no
mrb <- question_metadata[question_metadata$type == "multi_choice_flexible",]$no #(n = 5)


## loop through the options

# still in development
out <- purrr::map(mrb, function(x){

  #x <- mrb[1]
  noptions <- question_metadata[question_metadata$no == x,]$n_responses
  n_iterations <- noptions

  N   <- noptions
  vec <- c(0, 1)
  lst <- lapply(numeric(N), function(x) vec)
  combos <- as.data.frame(as.matrix(expand.grid(lst)))

  # pair down the all combos options as there needs to be at least one option selected
  combos$rs = rowSums(combos)
  combos <- combos |>
    #filter(rs %in% c(1,0)) |>    # for multi_choice_flexible
    filter(rs == 1) |>          # for multi_choice_
    select(-rs)

  # read in nad remove the values
  origin <- readr::read_csv("temp/sensitivity_test.csv", show_col_types = FALSE)
  nocols <- length(colnames(origin)[-1])
  allqs <-  origin$Question

  # remove the F56 from this data set
  #origin <- origin[-grep(paste0(x, "_"), origin$Question),]

  # remove rows where Question matches and starts with x
  origin <- origin[-grep(paste0("^", x, "_"), origin$Question),]

  #origin <- origin[-grep(x, origin$Question),]
  #allqs2 <-  origin$Question
  #setdiff(allqs, allqs2)

  iterate <- purrr::map(1:nrow(combos), function(i){
   #i <- 1
    combos[i,]
    origin.names <- origin[1,]

    # create three rows which are equal to the row of options and then add togther
    lines <- purrr::map(1:ncol(combos), function(j){
      #j <- 1
      coln <- paste0(x, "_", j)
      #outline <- c(coln, rep(combos[i,j], nocols))
      outline <- rbind(origin.names, c(coln, rep(combos[i,j], nocols)))
      outline[-1,]

    }) |> dplyr::bind_rows()

    # add the iteration to the main dataset
    outdf <- rbind(origin, lines)
    write_csv(outdf , fs::path("temp", paste0("sensitivity_test_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })


  # lets read in the files and determine the scores per output

  score <- purrr::map(1:nrow(combos), function(k){
    #k <- 2
    iname <- fs::path("temp", paste0("sensitivity_test_", x,"_",k, ".csv"))

    wesp_file <- fs::path(iname)
    wesp_data <- load_wesp_data(wesp_file)

    site <- as.wesp_site(wesp_data)

    site <- calc_indicators(site)

    q0 <- calculate_multi_site(wesp_data) |>
      tidyr::pivot_longer(-site, names_to = "question", values_to ="value") |>
      mutate(type = k)

    q0
  })

  score1 <-  score |> dplyr::bind_rows()


  write_csv(score1, fs::path("temp", paste0("sensitivity_test_", x,"_scores.csv")))

}) # end of loop through questions






#############################################################################
# Multi-response_binary
#############################################################################

mrb <- question_metadata[question_metadata$type == "multiresponse_binary",]$no
binaryq <- question_metadata[question_metadata$type == "binary",]$no

mrb<- mrb[3]
mrb <-binaryq[10]

#mrb <- mrb[4:6]
## loop through the options

# still in development
out <- purrr::map(mrb, function(x){

 # x <- mrb[4]
  noptions <- question_metadata[question_metadata$no == x,]$n_responses
  n_iterations <- 2^noptions

  N   <- noptions
  vec <- c(0, 1)
  lst <- lapply(numeric(N), function(x) vec)
  combos <- as.data.frame(as.matrix(expand.grid(lst)))

  # read in nad remove the values
  origin <- readr::read_csv("temp/sensitivity_test.csv")
  nocols <- length(colnames(origin)[-1])

  # remove the F56 from this data set
  # remove rows where Question matches and starts with x
  origin <- origin[-grep(paste0("^", x, "_"), origin$Question),]

  #origin <- origin[-grep(x, origin$Question),]

  iterate <- purrr::map(1:nrow(combos), function(i){
    #i <- 1
    combos[i,]
    origin.names <- origin[1,]

    # create three rows which are equal to the row of options and then add togther
    lines <- purrr::map(1:ncol(combos), function(j){
      #j <- 1
      coln <- paste0(x, "_", j)
      #outline <- c(coln, rep(combos[i,j], nocols))
      outline <- rbind(origin.names, c(coln, rep(combos[i,j], nocols)))
      outline[-1,]

    }) |> dplyr::bind_rows()

    # add the iteration to the main dataset
    outdf <- rbind(origin, lines)
    write_csv(outdf , fs::path("temp", paste0("sensitivity_test_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })


  # lets read in the files and determine the scores per output

  score <- purrr::map(1:nrow(combos), function(k){
    #k <- 2
    iname <- fs::path("temp", paste0("sensitivity_test_", x,"_",k, ".csv"))

    wesp_file <- fs::path(iname)
    wesp_data <- load_wesp_data(wesp_file)
    q0 <- calculate_multi_site(wesp_data) |>
      tidyr::pivot_longer(-site, names_to = "question", values_to ="value") |>
    mutate(type = k)

    q0
  })

  score1 <-  score |> dplyr::bind_rows()

  write_csv(score1, fs::path("temp", paste0("sensitivity_test_", x,"_scores.csv")))

}) # end of loop through questions




#########################################################################
# sumarise all the scores
#########################################################################

# read in files and gnerate plots

fls <- list.files ("temp", pattern = "*scores.csv")
sort(fls)

done <- gsub("sensitivity_test_", "",fls)
done <- gsub("_scores.csv", "", done)
done


#fls

out1 <- purrr::map(fls, function(x) {
   #x <- fls[41]

  xame <- gsub("_scores.csv", "", x)
  xtype <- gsub("sensitivity_test_", "", xame)

  score <- read_csv(fs::path("temp", x))

  score2 <- score |>
    tidyr::pivot_wider(
      id_cols = c("site", "question"),
      names_from = "type", values_from = "value"
    )

  # # merge the base data and calculate the delta
  score3 <- left_join(score2, base_raw) %>%
    select(-type) |>
    select(site, question, value, everything())


  score3 <- score3 |>
    rowwise() |>
    mutate(across(c(4:ncol(score3)), ~ .x - value, .names = "delta_{.col}")) |>
    ungroup()

  # score3 <-  score3  |>
  score3 <- score3 |>
    mutate_if(is.numeric, ~ round(., digit = 2))

  score_long <- score3 |>
    select(site, question, starts_with("d")) |>
    tidyr::pivot_longer(-c(site, question), names_to = "type", values_to = "value_delta")

  head(score_long)
  summ <- score_long |>
    summarise(
      n = n(),
      no_change = sum(value_delta == 0, na.rm = TRUE),
      max_delta = max(value_delta, na.rm = TRUE),
      mean_delta = mean(value_delta, na.rm = TRUE),
      sd_delta = sd(value_delta, na.rm = TRUE)
    )


  p1 <- ggplot2::ggplot(score_long, ggplot2::aes(.data$value_delta, fill = .data$type)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::facet_wrap(~ .data$question, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = paste0("Question: ", xtype))

  ggplot2::ggsave(
    paste0("temp/", xame, "_delta_p1.png"),
    plot = p1
  )


  p2 <- ggplot2::ggplot(score_long, ggplot2::aes(.data$value_delta)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::facet_wrap(~ .data$question, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = paste0("Question: ", xtype))


  ggplot2::ggsave(
    paste0("temp/", xame, "_delta_p2.png"),
    plot = p2
  )
})


###############################################################
# Summary table

#############################################

out1 <- purrr::map(fls, function(x) {
  #x <- fls[44]

  xame <- gsub("_scores.csv", "", x)
  xtype <- gsub("sensitivity_test_", "", xame)

  score <- read_csv(fs::path("temp", x),show_col_types = FALSE)

  score2 <- score |>
    tidyr::pivot_wider(
      id_cols = c("site", "question"),
      names_from = "type", values_from = "value"
    )

  # # merge the base data and calculate the delta for each rotation
  score3 <- left_join(score2, base_raw) %>%
    select(-type) |>
    select(site, question, value, everything())

  # value = base score (site 1)
  # delta1 = iteration one, delta2 = iteration two.

  score3 <- score3 |>
    rowwise() |>
    mutate(across(c(4:ncol(score3)), ~ .x - value, .names = "delta_{.col}")) |>
    ungroup()

  score3 <- score3 |>
    mutate_if(is.numeric, ~ round(., digit = 2))

  score_long <- score3 |>
    select(site, question, starts_with("d")) |>
    tidyr::pivot_longer(-c(site, question), names_to = "type", values_to = "value_delta") |>
    mutate(qno = xtype)

  score_long
})

# combine all the data

out1 <- out1 |> dplyr::bind_rows()

write_csv(out1, fs::path("temp", "sensitivity_all_summary.csv"))

# prepare a summary plot

out1








