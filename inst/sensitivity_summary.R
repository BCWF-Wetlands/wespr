# sensitivity summary outputs

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
#ecotype<- ecotype[1:3] # testing line

# get list of questions
responseno <- unique(out1$question_no)

summary_top_questions <- purrr::map(ecotype, function(x){

  #x <- ecotype[1]

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
    ) |>
    mutate(impactf = factor(impact, levels = c("minor", "moderate" , "large")))

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
  #

  # plot the distribution of the range of posible options

  p2 <-ggplot(xxx, aes(x = delta, y = question_no))+ #, fill = after_stat(x))) +
    #geom_density_ridges_gradient(scale = 0.8, rel_min_height = 0.02) +
    geom_point(size = 1, aes(colour = impactf), alpha = 0.2)+#, alpha = 0.2)+
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
    width = 20,
    height = 30,
    units = "cm"
  )
  #
  # summarise the questions that have the most impact on result (> +1/-1)
  xx_high_impact <- xxx |>
    filter(impact %in%  c("moderate", "large")) |>
    select(question_no) |>
    distinct() |>
    mutate(ecotype = x)

  xx_high_impact_qs <- xxx |>
    filter(question_no %in% unique(xx_high_impact$question_no))

#  plot only the highest impact

  p3 <-ggplot(xx_high_impact_qs, aes(x = delta, y = question_no))+ #, fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 0.8, rel_min_height = 0.02) +
    scale_fill_viridis(name = "Temp. [F]", option = "C") +
    #geom_jitter(size = 0.2, colour = "darkgrey", alpha = 0.2, height = -0.9)+
    geom_point(size = 0.9, aes(colour = impactf), alpha = 0.2)+#, alpha = 0.2)+
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





