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

temp_outdir <- "temp/sensitivity_raw"


#read wesp data into wespr

wesp_file <- system.file("input_data/reference_GD_20250620.csv", package = "wespr")
gdsites <- readr::read_csv(wesp_file)

wesp_file <- system.file("input_data/reference_SIM_20250620.csv", package = "wespr")
simsites <- readr::read_csv(wesp_file)


# get the last numbered site #109
names(gdsites)

newnames <- as.character(seq(110, 110 +length(names(simsites)), by = 1))
names(simsites) <-  c("Question", newnames)

all_sites <- left_join(gdsites, simsites, by = "Question")

# output dataset
write.csv(all_sites, "temp/sensitivity_raw/sensitivity_raw_input.csv", row.names = FALSE)
wesp_file <- "temp/sensitivity_raw/sensitivity_raw_input.csv"

# run the base scores for comparison
wesp_data <- load_wesp_data(wesp_file)

origin_data <- wesp_data |>
  mutate(Question = paste0(q_no,"_", response_no ))|>
  select(-q_no, -response_no) |>
  select(Question, everything())



base_score <- calculate_jenks_score(wesp_data, out_dir = "temp/sensitivity_raw", out_name = "wesp_scores_base.csv")
base_score <- readr::read_csv("temp/sensitivity_raw/wesp_scores_base.csv")

base_raw <- base_score |>
  select(site, any_of(ends_with("_raw"))) |>
  tidyr::pivot_longer(-site, names_to = "question", values_to = "value") |>
  mutate(type = "base")

#
# ggplot2::ggplot(base_raw, ggplot2::aes(.data$value)) +
#   ggplot2::geom_density(alpha = 0.2) +
#   ggplot2::facet_wrap(~.data$question, scales = "free") +
#   ggplot2::theme_bw()


# read in the base data
question_metadata
indicator_weightings

## types of questions

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


## types of questions
unique(question_metadata$type)

binq <- question_metadata[question_metadata$type == "binary",]$no # done
catq <- question_metadata[question_metadata$type == "category",]$no # done
mchq <- question_metadata[question_metadata$type == "multi_choice",]$no # done
mchfq <- question_metadata[question_metadata$type == "multi_choice_flexible",]$no # done
mrbq <- question_metadata[question_metadata$type == "multiresponse_binary",]$no # done
mrnq <- question_metadata[question_metadata$type == "multiresponse_numeric",]$no #done
numq <- question_metadata[question_metadata$type == "numeric",]$no # done


# still to do...

#mrb <- question_metadata[question_metadata$type == "multi_choice_numeric",]$no
## F45 - ph (number of 2x binary) - weird mix

#mrb <- question_metadata[question_metadata$type == "multiresponse_numeric",]$no
## F1  (0,1,2,3,4,5) # large!
## OF20 (0,1,2,3) - but skewed so not all values possible?

## + Stressors 1, 2, 3, 4, 5, 6,

mrnq <- mrnq[grep("*S", mrnq, invert = TRUE)] # remove the stressors - temporarily


temp_outdir <- "temp/sensitivity_raw"

#############################################################################
# binary and multi response binary
#############################################################################
response <-c(binq, mrbq)

out <- purrr::map(response, function(x){

  #x <- response[1]

  noptions <- question_metadata[question_metadata$no == x,]$n_responses
  n_iterations <- 2^noptions

  N   <- noptions
  vec <- c(0, 1)
  lst <- lapply(numeric(N), function(x) vec)
  combos <- as.data.frame(as.matrix(expand.grid(lst)))

  # read in nad remove the values
  origin <- origin_data
  nocols <- length(colnames(origin)[-1])

  # remove rows where Question matches and starts with x
  origin <- origin[-grep(paste0("^", x, "_"), origin$Question),]

  # loop through and
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
    write_csv(outdf , fs::path(temp_outdir, paste0("sensitivity_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })

  score <- purrr::map(1:nrow(combos), function(k){
    # k <- 2
    iname <- fs::path(temp_outdir, paste0("sensitivity_", x,"_",k, ".csv"))
    wesp_file <- fs::path(iname)
    wesp_data <- load_wesp_data(wesp_file)
    q0 <- calculate_multi_site(wesp_data) |>
      tidyr::pivot_longer(-site, names_to = "question", values_to ="value") |>
      mutate(type = k)
    q0
  })

  score1 <-  score |> dplyr::bind_rows()

  write_csv(score1, fs::path(temp_outdir, paste0("sensitivity_", x,"_scores.csv")))

})



#############################################################################
# Numeric response
#############################################################################

response <- c(numq, catq, mrnq)

response <- mrnq[2]

#mrb <- question_metadata[question_metadata$type == "numeric",]$no
#mrb <- question_metadata[question_metadata$type == "category",]$no #(n = 1) #OF29 - not working
#mrb <- question_metadata[question_metadata$type == "multiresponse_numeric",]$no

## loop through the options
out <- purrr::map(response, function(x){

 # x <- response[2]

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
  } else if (x == "OF20"){
    vec <- c(0, 1, 2, 3)
  } else if (x == "F1"){
    vec <- c(0, 1, 2, 3, 4)
  }

  noptions <- question_metadata[question_metadata$no == x,]$n_responses
  n_iterations <- noptions

  N   <- noptions
  lst <- lapply(numeric(N), function(x) vec)
  combos <- as.data.frame(as.matrix(expand.grid(lst)))

  # pair down the all combos options as there needs to be at least one option selected
  #  combos$rs = rowSums(combos)
  #  combos <- combos |>
  #    #filter(rs %in% c(1,0)) |>    # for multi_choice_flexible
  #    filter(rs == 1) |>          # for multi_choice_
  #    select(-rs)

  # read in and remove the values
  origin <- origin_data
  nocols <- length(colnames(origin)[-1])
  allqs <-  origin$Question

  # remove the F56 from this data set
  #origin <- origin[-grep(paste0(x, "_"), origin$Question),]

  # remove rows where Question matches and starts with x
  origin <- origin[-grep(paste0("^", x, "_"), origin$Question),]
  #origin <- origin[-grep(paste0("^", "F46_1"), origin$Question),] # for F46a

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
    write_csv(outdf , fs::path(temp_outdir, paste0("sensitivity_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })

  # lets read in the files and determine the scores per output

  score <- purrr::map(1:nrow(combos), function(k){
    # k <- 3
    iname <- fs::path(temp_outdir, paste0("sensitivity_", x,"_",k, ".csv"))

    wesp_file <- fs::path(iname)
    wesp_data <- load_wesp_data(wesp_file)

    site <- as.wesp_site(wesp_data)

    site <- calc_indicators(site)

    q0 <- calculate_multi_site(wesp_data) |>
      tidyr::pivot_longer(-site, names_to = "question", values_to ="value") |>
      mutate(type = k)

    q0
  })

  # up to here - then run for response[2]


  score1 <-  score |> dplyr::bind_rows()
  write_csv(score1, fs::path(temp_outdir, paste0("sensitivity_", x,"_scores.csv")))

}) # end of loop through questions



#############################################################################
# Multi-choice
#############################################################################

response <- mchq

out <- purrr::map(response, function(x){

  #x <- response[1]
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
  origin <- origin_data
  nocols <- length(colnames(origin)[-1])
  allqs <-  origin$Question

  # remove rows where Question matches and starts with x
  origin <- origin[-grep(paste0("^", x, "_"), origin$Question),]

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
    write_csv(outdf , fs::path(temp_outdir, paste0("sensitivity_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })


  # lets read in the files and determine the scores per output

  score <- purrr::map(1:nrow(combos), function(k){
    #k <- 2
    iname <- fs::path(temp_outdir, paste0("sensitivity_", x,"_",k, ".csv"))

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


  write_csv(score1, fs::path(temp_outdir, paste0("sensitivity_", x,"_scores.csv")))

}) # end of loop through questions



#############################################################################
# Multi-choice - flexibel

#############################################################################

response <- mchfq

## loop through the options
out <- purrr::map(response, function(x){

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
    filter(rs %in% c(1,0)) |>    # for multi_choice_flexible
    #filter(rs == 1) |>          # for multi_choice_
    select(-rs)

  # read in nad remove the values
  origin <- origin_data
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
    write_csv(outdf , fs::path(temp_outdir, paste0("sensitivity_", x,"_",i, ".csv")))
    cli::cli_alert_info(paste0("saving iteration ", i, " of ", n_iterations))

  })

  # lets read in the files and determine the scores per output

  score <- purrr::map(1:nrow(combos), function(k){
    #k <- 2
    iname <- fs::path(temp_outdir, paste0("sensitivity_", x,"_",k, ".csv"))

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

  write_csv(score1, fs::path(temp_outdir, paste0("sensitivity_", x,"_scores.csv")))

}) # end of loop through questions












#########################################################################
# sumarise all the scores
#########################################################################
library(ggplot2)

# read in files and gnerate plots

fls <- list.files ("temp/sensitivity_raw/", pattern = "*scores.csv")
sort(fls)


#Decide on reference score to be either the first site? or the average... although I dont love
# the average as it washes out the differnces between sites/

base_ref <- base_raw |>
  rename("base_score" = value) |>
  filter(site == 1) |> # remove the value column as it is not needed
  select(-type, -site)


out <- purrr::map(fls, function(x) {

  #x <- fls[63]

  xame <- gsub("_scores.csv", "", x)
  xtype <- gsub("sensitivity_", "", xame)

  score <- read_csv(fs::path("temp/sensitivity_raw", x), show_col_types = FALSE)

  score <- score |>
    mutate(question_no =  xtype)

  # calculate the average score per question_no and question

  score_averages <- score |>
    group_by(question) |>
    summarise(ave_score = mean(value, na.rm = TRUE), .groups = "drop") |>
    left_join(base_ref)

  score <- left_join(score, score_averages, by = "question")

  score

}) |> bind_rows()


  # calculate the delta values per site and question
  # what is the variation in the score and what is the direction (+ve or -ve)

  # # plot the number of options per question
  # resp_per_qu <- out |>
  #   select(question_no, type) |>
  #   distinct() |>
  #   group_by(question_no, type) |>
  #   summarise(n = n()) |>
  #   slice_max(type, n = 1) |>
  #   arrange(desc( question_no)) |>
  #   select(question_no, type)
  #
  # ggplot2::ggplot(resp_per_qu, ggplot2::aes(y = .data$type, x = .data$question_no)) +
  #   geom_bar(stat = "identity") +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  # plot is not super helpful here, very few questions have lots of cominations


  # calculate the delta value and summarise per question

  delta_out <- out |>
    rowwise() |>
    mutate(delta_ave = ave_score - value,
           delta_base = base_score - value)

  # create summary table
  summary_delta <- delta_out |>
    group_by(question, type, question_no ) |>
    summarise( n = n(),
      #minimal_change = sum(delta_ave == 0, na.rm = TRUE),
      min_delta = min(delta_ave, na.rm = TRUE),
      max_delta = max(delta_ave, na.rm = TRUE),
      mean_delta = mean(delta_ave, na.rm = TRUE),
      sd_delta = sd(delta_ave, na.rm = TRUE))


  # write out the summary table
  write_rds(delta_out, fs::path("temp", "sensitivity_raw", "all_summaries_compiled.rds"))


###########################################################################################

# read in data

delta_out <- read_rds(fs::path("temp", "sensitivity_raw", "all_summaries_compiled.rds"))

# lets review a single metrics and figure out the key questions

 # WS_f_raw

delta_out <- delta_out |> filter(question == "WS_b_raw") |>
   filter(site %in% c(1:10))


p1 <- ggplot2::ggplot(delta_out, ggplot2::aes(.data$delta_ave )) +
   ggplot2::geom_density(alpha = 0.5) +
   ggplot2::facet_wrap(~ .data$question_no, scales = "free") +
   ggplot2::theme_bw() #+
          #     ggplot2::theme(legend.position = "none") +
          #     ggplot2::labs(title = paste0("Question: ", xtype))
          #
          #   ggplot2::ggsave(
          #     paste0("temp/", xame, "_delta_p1.png"),
          #     plot = p1
          #   )


 # create summary table
 summary_delta <- delta_out |>
   group_by(question, type, question_no ) |>
   summarise( n = n(),
              #minimal_change = sum(delta_ave == 0, na.rm = TRUE),
              min_delta = min(delta_ave, na.rm = TRUE),
              max_delta = max(delta_ave, na.rm = TRUE),
              mean_delta = mean(delta_ave, na.rm = TRUE),
              sd_delta = sd(delta_ave, na.rm = TRUE))


 #xx <- out1 |>
  # filter(question == x)|>
  # filter(value_delta!=0) |>
   # mutate(impact = case_when(
   #   value_delta > -1 & value_delta <1 ~ "minor",
   #   value_delta > 1 & value_delta <4 ~ "moderate",
   #   value_delta < -1 ~ "moderate",
   #   value_delta >4 ~ "large",
   #   value_delta < -4 ~ "large")
   # )

   p1 <- ggplot(delta_out , aes(delta_ave, question_no, colour = type)) +
   geom_jitter(height = 0.08)#+
   #scale_color_viridis() +
   theme_bw()+
   ggplot2::theme(legend.position = "none") +
   xlab("Difference in overall score") +
   ylab("Question Number") #+
   #ggplot2::labs(title = paste0("Eco service: ", x),
                 #subtitle = "Potential change in score as compared to reference site, based on contributing questions"
   #)

 p1

 ggplot2::ggsave(
   paste0("temp/", x, "_overall_deltas.png"),
   plot = p1,
   width = 25,
   height = 20,
   units = "cm"
 )
 })





  p2 <- ggplot2::ggplot(df, ggplot2::aes(.data$delta_ave)) +
        ggplot2::geom_density(alpha = 0.5) +
        ggplot2::facet_wrap(~ .data$question_no, scales = "free") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(title = paste0("Question: ", xtype))


      ggplot2::ggsave(
        paste0("temp/", xame, "_delta_p2.png"),
        plot = p2
      )





#   # # merge the base data and calculate the delta
#   score3 <- left_join(score2, base_raw) %>%
#     select(-type) |>
#     select(site, question, value, everything())
#
#   score3 <- score3 |>
#     rowwise() |>
#     mutate(across(c(4:ncol(score3)), ~ .x - value, .names = "delta_{.col}")) |>
#     ungroup()
#
#   # score3 <-  score3  |>
#   score3 <- score3 |>
#     mutate_if(is.numeric, ~ round(., digit = 2))
#
#   score_long <- score3 |>
#     select(site, question, starts_with("d")) |>
#     tidyr::pivot_longer(-c(site, question), names_to = "type", values_to = "value_delta")
#
#   head(score_long)
#   summ <- score_long |>
#     summarise(
#       n = n(),
#       no_change = sum(value_delta == 0, na.rm = TRUE),
#       max_delta = max(value_delta, na.rm = TRUE),
#       mean_delta = mean(value_delta, na.rm = TRUE),
#       sd_delta = sd(value_delta, na.rm = TRUE)
#     )
#
#
#   p1 <- ggplot2::ggplot(score_long, ggplot2::aes(.data$value_delta, fill = .data$type)) +
#     ggplot2::geom_density(alpha = 0.5) +
#     ggplot2::facet_wrap(~ .data$question, scales = "free") +
#     ggplot2::theme_bw() +
#     ggplot2::theme(legend.position = "none") +
#     ggplot2::labs(title = paste0("Question: ", xtype))
#
#   ggplot2::ggsave(
#     paste0("temp/", xame, "_delta_p1.png"),
#     plot = p1
#   )
#
#
#   p2 <- ggplot2::ggplot(score_long, ggplot2::aes(.data$value_delta)) +
#     ggplot2::geom_density(alpha = 0.5) +
#     ggplot2::facet_wrap(~ .data$question, scales = "free") +
#     ggplot2::theme_bw() +
#     ggplot2::theme(legend.position = "none") +
#     ggplot2::labs(title = paste0("Question: ", xtype))
#
#
#   ggplot2::ggsave(
#     paste0("temp/", xame, "_delta_p2.png"),
#     plot = p2
#   )
# })


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




#################################################################

## Grahics with full distribution

######################################################################


library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggdist)
#install.packages("viridis")
library(viridis)

# prepare a summary plot

#out1 <- read_csv( fs::path("temp", "sensitivity_all_summary.csv"))
out1  <- read.csv(fs::path("temp", "initial_sensitivity_testing", "sensitivity_all_summary.csv"))

ecotype <- unique(out1$question)

# xx <- out1 |>
#   mutate(mcol = case_when(
#     value_delta <0 & value_delta>0.05 ~ "red",
#     value_delta == 0 ~ "grey"
#   ))


purrr::map(ecotype, function(x){

  x <- ecotype[1]

  xx <- out1 |>
    filter(question == x)|>
    filter(value_delta!=0) |>
    # mutate(impact = case_when(
    #   value_delta > -1 & value_delta <1 ~ "minor",
    #   value_delta > 1 & value_delta <4 ~ "moderate",
    #   value_delta < -1 ~ "moderate",
    #   value_delta >4 ~ "large",
    #   value_delta < -4 ~ "large")
    # )

  p1 <- ggplot(xx , aes(value_delta, qno, colour = value_delta)) +
    geom_jitter(height = 0.08)+
    scale_color_viridis() +
    theme_bw()+
    ggplot2::theme(legend.position = "none") +
    xlab("Difference in overall score") +
    ylab("Question Number") +
    ggplot2::labs(title = paste0("Eco service: ", x),
                  #subtitle = "Potential change in score as compared to reference site, based on contributing questions"
    )

  p1

  ggplot2::ggsave(
    paste0("temp/", x, "_overall_deltas.png"),
    plot = p1,
    width = 25,
    height = 20,
    units = "cm"
  )
})

# try to creat a matrix risk

x <- ecotype[1]

xx <- out1 |>
  filter(question == x)|>
  filter(value_delta!=0) #|>
  # mutate(impact = case_when(

max_impact <- out1 |>
  group_by(question, qno) |>
  summarise(max_impact = max(abs(value_delta), na.rm = TRUE)) |>
  filter(max_impact > 1)


tt <- max_impact |> filter(question == "AM_b_raw")

tta <- tidyr::pivot_wider(
  tt,
  id_cols = question,
  names_from = qno,
  values_from = max_impact
) |>
  ungroup() |>
  select(-question)

tta <- rbind(rep(10, ncol(tta)),
             rep(0, ncol(tta)),
             tta)


radarchart(tta,axistype=1 ,
  #custom polygon
  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,

  #custom the grid
  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2), cglwd=0.8,
  #custom labels
  vlcex=0.8 )


# build spider plot

library(fmsb)
install.packages("fmsb")

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
 head(data)

# The default radar chart

 # Custom the radarChart !
 radarchart( data  , axistype=1 ,

             #custom polygon
             pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,

             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,

             #custom labels
             vlcex=0.8
 )




xx <- xx |>
  filter(question == ecotype[1]) |>
  filter(value_delta!=0)


ggplot(xx , aes(value_delta, qno, colour = value_delta)) +
  geom_point()+
  scale_color_viridis() +
  theme_bw()+
  ggplot2::theme(legend.position = "none") +
  xlab("Difference in overall score") +
  ylab("Question Number") +
  ggplot2::labs(title = paste0("Eco service: ", ecotype[1]),
                #subtitle = "Potential change in score as compared to reference site, based on contributing questions"
  )

ggplot2::ggsave(
  paste0("temp/", xame, "_delta_p2.png"),
  plot = p2
)


