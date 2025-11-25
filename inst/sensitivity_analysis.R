## TESTING AND DEVELOPMENT SCRIPT

# This script is and internal developemnt script to assess sensitivity of the calculations
# within the wespr project.

# Use is for internal and develpoment only and not for pacakge use.

# NOte - this may require some changes to file inputs and outputs as dont want to interfere
# with the package (ie. temp folder. )


#Prepare for and install wespr
#devtools::install_github("BCWF-Wetlands/wespr")
#library(wespr)
#load_all()
library(dplyr)
library(readr)
library(ggplot2)


temp_outdir <- "temp/sensitivity_raw"

#read wesp data into wespr
# collate all the sitees in the calibration dataset

wesp_file <- system.file("input_data/reference_GD_20250620.csv", package = "wespr")
gdsites <- readr::read_csv(wesp_file)

wesp_file <- system.file("input_data/reference_SIM_20250620.csv", package = "wespr")
simsites <- readr::read_csv(wesp_file)

# get the last numbered site #109
#names(gdsites)

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
library(tidyverse)

# read in files and gnerate plots

fls <- list.files ("temp/sensitivity_raw/", pattern = "*scores.csv")
sort(fls)
#fls <- fls[1:5]

out <- purrr::map(fls, function(x) {
  # x <- fls[63]

  xame <- gsub("_scores.csv", "", x)
  xtype <- gsub("sensitivity_", "", xame)

  score <- read_csv(fs::path("temp/sensitivity_raw", x),show_col_types = FALSE)

  score <- score |>
    mutate(question_no = xtype) |>
    select(site, question_no, type, question, value) |>
    rename("site" = site, "question" = question_no, "response" = type, "score_name" = question,
           "score" = value)

  score

}) |> bind_rows()

#write out as rda

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
library(dplyr)
library(ggdist)
library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(ggridges)

#install.packages("MetBrewer")
#library(patchwork)
#library(ggrepel)
#library(ggtext)
library(viridis)

# prepare a summary plot

#out1 <- read_csv( fs::path("temp", "sensitivity_all_summary.csv"))
#out1  <- read.csv(fs::path("temp", "initial_sensitivity_testing", "sensitivity_all_summary.csv"))
out1 <- readRDS(fs::path("temp", "sensitivity_raw","all_summaries_compiled.rds"))

# get list of ecotypes
ecotype <- unique(out1$question)
#ecotype<- ecotype[1:2] # testing line

# get list of questions
responseno <- unique(out1$question_no)


summary_top_questions <- purrr::map(ecotype, function(x){

  x <- ecotype[1]

  xx <- out1 |>
    filter(question == x)|>
    select(site, question, value, type, question_no)

  xx <- xx |>
    group_by(site) |>
    mutate(median_val = round(median(value),2)) |>
    ungroup() |>
    mutate(delta = round(value,2) - median_val)

  xxx <- xx

  # select any questions which impact the result
  xx_to_keep <- xxx |>
    filter(delta != 0) |>
    select(question_no) |>
    distinct() |>
    pull()

  # catergorise the imapact of question on result
  xxx <- xxx |>
    filter(question_no %in% xx_to_keep) |>
    mutate(impact = case_when(
      delta > -1 & delta <1 ~ "minor",
      delta > 1 & delta <4 ~ "moderate",
      delta < -4 ~ "large",
      delta < -1 ~ "moderate",
      delta >4 ~ "large")
    )

  # plot 1: plot a simple bar chart

  # p1 <- ggplot(xxx , aes(delta, question_no, colour = impact)) +
  #   #geom_point(alpha = 0.5)+
  #   geom_jitter(height = 0.01, alpha = 0.2)+
  #   scale_color_viridis_d() +
  #   theme_bw()+
  #   ggplot2::theme(legend.position = "none") +
  #   #xlab("Difference in overall score") +
  #   ylab("Question Number") +
  #   ggplot2::labs(title = paste0("Eco service: ", x),
  #                 #subtitle = "Potential change in score as compared to reference site, based on contributing questions"
  #   )
  # p1


  # plot the distribution of the range of posible options

  p2 <-ggplot(xxx, aes(x = delta, y = question_no))+ #, fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 0.8, rel_min_height = 0.02) +
    scale_fill_viridis(name = "Temp. [F]", option = "C") +
    #geom_jitter(size = 0.2, colour = "darkgrey", alpha = 0.2, height = -0.9)+
    geom_point(size = 0.2, aes(colour = impact), alpha = 0.2)+#, alpha = 0.2)+
    #stat_interval(.width = c(0.5, 0.75, 0.95), interval_size=1) +
    scale_color_viridis_d() +
    #scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
    theme_ridges() +
    theme(legend.position = "none")

  p2


  # plot 3 basic histogram

  # p3 <- ggplot(xxx , aes(delta, fill = question_no)) +
  #   geom_histogram( alpha=0.6, position = 'identity')+
  #   #geom_density(data=xxx, aes(x=delta, group=question_no, fill=question_no), adjust=1.5, alpha=.4) +
  #   scale_color_viridis_d() +
  #   facet_wrap(~question_no, scales = "free_y") +
  #   #theme_ridges()+
  #   ggplot2::theme(legend.position = "none") +
  #   #xlab("Difference in overall score") +
  #   ylab("Question Number") +
  #   ggplot2::labs(title = paste0("Eco service: ", x),
  #   )
  #
  # p3

  ggplot2::ggsave(
    paste0("temp/sensitivity_raw/", x, "_deltas_all.jpeg"),
    #plot = p2,
    width = 25,
    height = 20,
    units = "cm"
  )

  # summarise the questions that have the most impact on result (> +1/-1)
  xx_high_impact <- xxx |>
    filter(impact %in%  c("moderate", "large")) |>
    select(question_no) |>
    distinct() |>
    mutate(ecotype = x)

  xx_high_impact_qs <- xxx |>
    filter(question_no %in% unique(xx_high_impact$question_no))

  # plot only the highest impact

  p3 <-ggplot(xx_high_impact_qs, aes(x = delta, y = question_no))+ #, fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 0.8, rel_min_height = 0.02) +
    scale_fill_viridis(name = "Temp. [F]", option = "C") +
    #geom_jitter(size = 0.2, colour = "darkgrey", alpha = 0.2, height = -0.9)+
    geom_point(size = 0.9, aes(colour = impact), alpha = 0.2)+#, alpha = 0.2)+
    #stat_interval(.width = c(0.5, 0.75, 0.95), interval_size=1) +
    scale_color_viridis_d() +
    #scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
    theme_ridges() +
    theme(legend.position = "none")

  p3

  ggplot2::ggsave(
    paste0("temp/sensitivity_raw/", x, "_deltas_topqs.jpeg"),
    #plot = p2,
    width = 25,
    height = 20,
    units = "cm"
  )

  xx

})#|>  bind_rows()

##############################################################################
# Combine all outputs with full data sensitivity output

all <- summary_top_questions |>
  bind_rows()

all <- all |> select(question, type, question_no, delta)
all <- all |>
  mutate(question = gsub("_raw", "", question)) |>
  filter(!is.na(delta))

max_delta <- all |>
  group_by(question, question_no) |>
  summarise(
    max_delta = max(delta),
    min_delta = min(delta)
  ) |>
  ungroup()

# part 1: field questions
fmax_delta <- max_delta |>
  filter(startsWith(question_no, "F"))

tta <- tidyr::pivot_wider(
  fmax_delta,
  id_cols = question,
  names_from = question_no,
  values_from = max_delta
)

tta <- as.data.frame(tta)
row.names(tta) <- tta$question
tta <- tta[, -1]

# install.packages("heatmaply")
library(heatmaply)
heatmaply(tta, Rowv = FALSE, Colv = FALSE, file = "temp/sensitivity_raw/heatmap_maxdelta_fieldqs.html")
browseURL("temp/sensitivity_raw/heatmap_maxdelta_fieldqs.html")

# part 2: office questions
fmax_delta <- max_delta |>
  filter(startsWith(question_no, "O"))

tta <- tidyr::pivot_wider(
  fmax_delta,
  id_cols = question,
  names_from = question_no,
  values_from = max_delta
)

tta <- as.data.frame(tta)
row.names(tta) <- tta$question
tta <- tta[, -1]

# install.packages("heatmaply")
library(heatmaply)
heatmaply(tta, Rowv = FALSE, Colv = FALSE, file = "temp/sensitivity_raw/heatmap_maxdelta_officeqs.html")
browseURL("temp/sensitivity_raw/heatmap_maxdelta_officeqs.html")

# library(RColorBrewer)
# note these cut off the number of columns and rows that can be shown

# heatmap(as.matrix(tta), Colv = NA, Rowv = NA, scale="column",
#        xlab="ecosystem function/benefit", ylab="question")#,
# col= colorRampPalette(brewer.pal(8, "Blues"))(25))
#
# #install.packages("ggheatmap", dep = T)
# library(ggheatmap)
# ggheatmap(tta, shape = "circle", show_cluster_rows = F )
#
# install.packages("gplots")
# library(gplots)
# heatmap.2(as.matrix(tta),dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none')
#
# geom_tile(tta)


####################################################################################

# Part 2: extract the top responses

# summary_top_questions
# write.csv(summary_top_questions, fs::path("temp","sensitivity_raw","top_qs_per_ecofunction.csv"))

xxx <- summary_top_questions |> bind_rows()

ecotype <- unique(xxx$question)
# ecotype<- ecotype[1:2] # testing line

top_questions <- purrr::map(ecotype, function(x) {
  #  x <- ecotype[1]

  xx <- xxx |>
    filter(question == x) |>
    select(site, question, question_no, delta)

  # select any questions which impact the result
  xx_to_keep <- xx |>
    filter(delta != 0) |>
    select(question_no) |>
    distinct() |>
    pull()

  # catergorise the imapact of question on result
  xx <- xx |>
    filter(question_no %in% xx_to_keep) |>
    mutate(impact = case_when(
      delta > -1 & delta < 1 ~ "minor",
      delta > 1 & delta < 4 ~ "moderate",
      delta < -4 ~ "large",
      delta < -1 ~ "moderate",
      delta > 4 ~ "large"
    ))

  # summarise the questions that have the most impact on result (> +1/-1)
  xx_high_impact <- xx |>
    filter(impact %in% c("moderate", "large")) |>
    select(question_no) |>
    distinct() |>
    mutate(ecotype = x)

  xx_high_impact
}) |> bind_rows()

top_questions

write.csv(top_questions, fs::path("temp","sensitivity_raw","top_qs_per_ecofunction.csv"))





