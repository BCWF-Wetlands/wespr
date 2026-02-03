# sensitivity summary outputs

# internal script to summarise and plot the full sensitivity analysis
# this is only run on the linux machine


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

#   xxx <- xx
#
#   # select any questions which impact the result
#   xx_to_keep <- xxx |>
#     filter(delta != 0) |>
#     select(question_no) |>
#     distinct() |>
#     pull()
#
#   # catergorise the imapact of question on result
#   xxx <- xxx |>
#     filter(question_no %in% xx_to_keep) |>
#     mutate(impact = case_when(
#       delta > -1 & delta <1 ~ "minor",
#       delta > 1 & delta <4 ~ "moderate",
#       delta < -4 ~ "large",
#       delta < -1 ~ "moderate",
#       delta >4 ~ "large")
#     ) |>
#     mutate(impactf = factor(impact, levels = c("minor", "moderate" , "large")))
#
#   # plot 1: plot a simple bar chart
#
#   # p1 <- ggplot(xxx , aes(delta, question_no, colour = impact)) +
#   #   #geom_point(alpha = 0.5)+
#   #   geom_jitter(height = 0.01, alpha = 0.2)+
#   #   scale_color_viridis_d() +
#   #   theme_bw()+
#   #   ggplot2::theme(legend.position = "none") +
#   #   #xlab("Difference in overall score") +
#   #   ylab("Question Number") +
#   #   ggplot2::labs(title = paste0("Eco service: ", x),
#   #                 #subtitle = "Potential change in score as compared to reference site, based on contributing questions"
#   #   )
#   # p1
#   #
#   qname_site <- unique(xxx$question)
#   qname <- if(str_detect(qname_site, "_f_")== TRUE){
#     "function"
#   } else {
#     "benefit"
#   }
#   qtype <- str_split_i(qname_site, "_", 1)
#   qname <- paste0(qtype ," : ", qname)
#
#   plasma_pal <- c(viridis::viridis(n = 3))
#   plasma_pal
#
#   # plot the distribution of the range of posible options
#
#   p2 <-ggplot(xxx, aes(x = delta, y = question_no))+ #, fill = after_stat(x))) +
#     #geom_density_ridges_gradient(scale = 0.8, rel_min_height = 0.02) +
#     geom_point(size = 1, aes(colour = impactf), alpha = 0.2)+#, alpha = 0.2)+
#     #stat_interval(.width = c(0.5, 0.75, 0.95), interval_size=1) +
#     #scale_color_viridis_d() +
#     scale_color_manual(values = plasma_pal)+
#     #scale_fill_brewer(palette = 2) + # Line added
#     #scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
#     theme_ridges() +
#     theme(legend.position = "none")+
#     labs(title = qname)
#
#   p2
#
#
#   # plot 3 basic histogram
#
#   # p3 <- ggplot(xxx , aes(delta, fill = question_no)) +
#   #   geom_histogram( alpha=0.6, position = 'identity')+
#   #   #geom_density(data=xxx, aes(x=delta, group=question_no, fill=question_no), adjust=1.5, alpha=.4) +
#   #   scale_color_viridis_d() +
#   #   facet_wrap(~question_no, scales = "free_y") +
#   #   #theme_ridges()+
#   #   ggplot2::theme(legend.position = "none") +
#   #   #xlab("Difference in overall score") +
#   #   ylab("Question Number") +
#   #   ggplot2::labs(title = paste0("Eco service: ", x),
#   #   )
#   #
#   # p3
#
#   ggplot2::ggsave(
#     paste0("temp/sensitivity_raw/", x, "_deltas_all.jpeg"),
#     #plot = p2,
#     width = 20,
#     height = 30,
#     units = "cm"
#   )
#
#   # summarise the questions that have the most impact on result (> +1/-1)
#   xx_high_impact <- xxx |>
#     filter(impact %in%  c("moderate", "large")) |>
#     select(question_no) |>
#     distinct() |>
#     mutate(ecotype = x)
#
#   xx_high_impact_qs <- xxx |>
#     filter(question_no %in% unique(xx_high_impact$question_no))
#
# #  plot only the highest impact
#
#   p3 <-ggplot(xx_high_impact_qs, aes(x = delta, y = question_no))+ #, fill = after_stat(x))) +
#     geom_density_ridges_gradient(scale = 0.8, rel_min_height = 0.02) +
#     scale_fill_viridis(name = "Temp. [F]", option = "C") +
#     #geom_jitter(size = 0.2, colour = "darkgrey", alpha = 0.2, height = -0.9)+
#     geom_point(size = 0.9, aes(colour = impactf), alpha = 0.2)+#, alpha = 0.2)+
#     #stat_interval(.width = c(0.5, 0.75, 0.95), interval_size=1) +
#     scale_color_viridis_d() +
#     #scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
#     theme_ridges() +
#     theme(legend.position = "none")+
#     labs(title = qname)
#
#   p3
#
#    ggplot2::ggsave(
#      paste0("temp/sensitivity_raw/", x, "_deltas_topqs.jpeg"),
#      #plot = p2,
#      width = 25,
#      height = 20,
#      units = "cm"
#    )

  xx

})#|>  bind_rows()

##############################################################################
# Combine all outputs with full data sensitivity output

all <- summary_top_questions |>
  bind_rows()

all <- all |> select(question, question_no, delta)
all <- all |>
  mutate(question = gsub("_raw", "", question)) |>
  filter(!is.na(delta)) |>
  filter(delta !=0)

max_delta <- all |>
  group_by(question, question_no) |>
  summarise(
    max_delta = round(max(delta),2),
    min_delta = round(min(delta),2)
  ) |>
  ungroup()



# functions
max_delta_type <- max_delta |>
  mutate(question_type = str_split_i(question, "_", 2))|>
  mutate(overall_impact = (max_delta + (min_delta* -1))/2)

# benefit type
max_delta_b <- max_delta_type |>
  filter(question_type == "b") |>
  mutate(question = gsub("_b", "", question))

## max delta
raw_plotb <- ggplot(max_delta_b, aes(x = question, y = question_no))+
  geom_tile(aes(fill = overall_impact))+
  #scale_colour_gradient()
  scale_fill_distiller(palette = "Blues", direction = 1)+
  theme_minimal()+
  labs(title = "Benefits")

# theme(legend.position = "none")
raw_plotb


saveRDS(max_delta_b, fs::path("temp", "sensitivity_raw","summary_benefit_overview.rds"))


ggplot2::ggsave(
  paste0("temp/sensitivity_raw/benefit_overall_imapact.jpeg"),
  width = 25,
  height = 30,
  units = "cm"
)



# function type

max_delta_f <- max_delta_type |>
  filter(question_type == "f") |>
  mutate(question = gsub("_f", "", question))

raw_plotf <- ggplot(max_delta_f, aes(x = question, y = question_no))+
  geom_tile(aes(fill = overall_impact))+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  theme_minimal()+
  labs(title = "Functions")

raw_plotf
saveRDS(max_delta_f, fs::path("temp", "sensitivity_raw","summary_function_overview.rds"))



ggplot2::ggsave(
  paste0("temp/sensitivity_raw/function_overall_imapact.jpeg"),
  width = 25,
  height = 30,
  units = "cm"
)


####################################################################################
#
# # Part 2: extract the top responses
#
# # summary_top_questions
# # write.csv(summary_top_questions, fs::path("temp","sensitivity_raw","top_qs_per_ecofunction.csv"))
#
# xxx <- summary_top_questions |> bind_rows()
#
# ecotype <- unique(xxx$question)
# # ecotype<- ecotype[1:2] # testing line
#
# top_questions <- purrr::map(ecotype, function(x) {
#   #  x <- ecotype[1]
#
#   xx <- xxx |>
#     filter(question == x) |>
#     select(site, question, question_no, delta)
#
#   # select any questions which impact the result
#   xx_to_keep <- xx |>
#     filter(delta != 0) |>
#     select(question_no) |>
#     distinct() |>
#     pull()
#
#   # catergorise the imapact of question on result
#   xx <- xx |>
#     filter(question_no %in% xx_to_keep) |>
#     mutate(impact = case_when(
#       delta > -1 & delta < 1 ~ "minor",
#       delta > 1 & delta < 4 ~ "moderate",
#       delta < -4 ~ "large",
#       delta < -1 ~ "moderate",
#       delta > 4 ~ "large"
#     ))
#
#   # summarise the questions that have the most impact on result (> +1/-1)
#   xx_high_impact <- xx |>
#     filter(impact %in% c("moderate", "large")) |>
#     select(question_no) |>
#     distinct() |>
#     mutate(ecotype = x)
#
#   xx_high_impact
# }) |> bind_rows()
#
# top_questions
#
# write.csv(top_questions, fs::path("temp","sensitivity_raw","top_qs_per_ecofunction.csv"))
#
#

##################################################################################



# Review all the questions and get a summary of Qs, overall impact , no of questions that they contribute to?


ff <- readRDS(fs::path("temp", "sensitivity_raw","summary_function_overview.rds"))
bb <- readRDS(fs::path("temp", "sensitivity_raw","summary_benefit_overview.rds"))

# Review all the benefits and functions (summary)


library(googlesheets4)
library(dplyr)
library(readr)
library(janitor)

# Edit spreadsheet at:
# https://docs.google.com/spreadsheets/d/1l2h7Z65H5z0cKv_gvorxkT6k9LC7ZKLS/edit#gid=924841838
#
# Then run this script to update the internal datasets `question_metadata` and
# `indicator_weightings`
#
# To authorize the googlesheets download, set your google auth email with:
# options(
#   gargle_oauth_email = "email.which.gives.you.access.to.these.files@gmail.com"
# )
# If this is different from your normal google auth email you can add this to a
# project-specific .Rprofile file to cache

gs_id <- "1kk_RT7_cz6yT6hBYx6Es5ZIie3V9IU85ZkraJBDHTj8"



# 1) summary of the number of questions used per type (function and benefit)

type_summary <- read_sheet(gs_id, sheet = "all_indicators", col_types = "c",
                                .name_repair = make_clean_names) |>
  filter(is.na(no)) |>
  select(-no, -question, -n_responses, -unique_values, -unique_value_response, -x, -no_indicators) |>
  filter(!is.na(type))

type_sum <- t(type_summary)
type_sums_id <- row.names(type_sum)

type_sumt <- as.data.frame(type_sum)

names(type_sumt) <- c("Function", "Benefit", "Stressor", "other" )
type_sum <- type_sumt[-1,] |>
  mutate(Function = as.numeric(Function),
         Benefit = as.numeric(Benefit),
         Stressor = as.numeric(Stressor),
         other = as.numeric(other)) |>
  rowwise() |>
  mutate(total_no = sum(`Function`, `Benefit`, `Stressor`, `other`, na.rm = TRUE)) |>
  ungroup()

  out <- cbind(type =type_sums_id[-1], type_sum)




# 2) summary of the questions and frequency at which they are used overall

question_sum<- read_sheet(gs_id, sheet = "all_indicators", col_types = "c",
                                .name_repair = make_clean_names) |>
  filter(!is.na(no), no != "score") |>
  select(no,  no_indicators) |>
  mutate(no_indicators = ifelse(no_indicators == "ALL", 19, as.numeric(no_indicators)))



#################################################################################
# Jan 2026 = make a list of the least important questions for each function and all functions

# data we have to work with

# 1) ff / bb - sensitiivity analysis
# 2) out  - number of questions that contribute to each ecosystemt indicator (f/b)
# 3) question_sum - no of indicators per question

all <- bind_rows(ff, bb)

# 1) functions

fi <-all |>
  #filter(question_type == "f") |>
  select(question, question_no, question_type,overall_impact) |>
  mutate (indicator = tolower(question)) |>
  select(-question) |>
  rowwise() |>
  mutate(question_method = substring(question_no, 1, 1)) |>
  ungroup() |>
  group_by(question_no, question_type, question_method) |>
  summarise(total_impact = sum(overall_impact)) |>
   ungroup() #|>
  # #arrange(total_impact) |>
  # mutate(total_impact = case_when(
  #   question_type == "f" ~ -total_impact,
  #   question_type == "b" ~ total_impact,
  # )) |>
  # arrange(total_impact)


  # mutate(col_impact = case_when(
  #       total_impact > -1 & total_impact <1 ~ "minor",
  #       total_impact > 1 & total_impact <4 ~ "moderate",
  #       total_impact < -4 ~ "large",
  #       total_impact < -1 ~ "moderate",
  #       total_impact >4 ~ "large")
  #     )



#out
# add the question no:
question_sum <- question_sum |>
  rename("question_no" = no)


fi = left_join(fi, question_sum)

library(ggplot2)

p1 <- ggplot(fi, aes(fill = question_type,
                      y = reorder(question_no, -total_impact),
                      x = ifelse(test = question_type == "f",
                                   yes = -total_impact,
                                    no = total_impact)))+
             #colour = col_impact)) +
  geom_vline(xintercept= c(-4, 4), linetype = "dashed", colour = "darkgrey")+
  geom_vline(xintercept= c(-1, 1), linetype = "dashed", color= "darkgrey")+

  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("darkblue", "lightblue") ) +
  #scale_fill_brewer(palette = "") +
  facet_wrap(~question_method, scales = "free")+
  #geom_vline(xintercept= c(-4, 4), linetype = "dashed", colour = "darkgrey")+
  #geom_vline(xintercept= c(-1, 1), linetype = "dashed", color= "darkgrey")+
  labs(
    x = "combined delta value",
    y = "questions") +
  coord_cartesian(xlim =c(-20, 20))+
  geom_text(aes(label = no_indicators, y = reorder(question_no, -total_impact), x = 20), colour = "darkslategrey")
  #               hjust = ifelse(total_impact <0, 1.25 , -0.25)))

  p1


ggplot2::ggsave(
  paste0("temp/sensitivity_raw/overall_imapact.jpeg"),
  width = 40,
  height = 30,
  units = "cm"
)

