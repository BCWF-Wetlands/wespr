#' Format raw inputs
#' This function cleans, formats and combines raw outputs from field and desktop analysis survey123 forms.
#'
#' @param field_data  A file path to survey123 csv file for field data collected
#' @param desktop_data A file path to survey123 csv file for Desktop data
#' @param write_subfiles A logical value specifying whether to write the subfiles. Default = TRUE
#' @param out_dir A character string specifying the output directory.
#' @param overwrite A logical value specifying whether to overwrite the output files. Default = FALSE
#'
#' @returns dataframe in format for use in wespr
#' @export
#'
#' @examples
#' \dontrun{
#'ww <- format_rawinputs(
#'  field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr"),
#'  desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr"),
#'  write_subfiles = FALSE,
#'  out_dir = "input_data",
#'  overwrite = TRUE)
#'}
format_rawinputs <- function(field_data,
                            desktop_data,
                            write_subfiles = TRUE,
                            out_dir = "temp",
                            overwrite = FALSE) {

  # # testing files
  # field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr")
  # desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr")
  # write_subfiles = FALSE
  # out_dir <- "inst/input_data/processed1"
  # overwrite = TRUE

  if (!exists(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE)
    cli::cli_alert_success("Output directory created")
  }

  # check format for field and data files
  if (tools::file_ext(field_data) != "csv") {
    cli::cli_abort("field data is required to be in .csv format, please check input")
  }

  if (tools::file_ext(desktop_data) != "csv") {
    cli::cli_abort("field data is required to be in .csv format, please check input")
  }


  cli::cli_alert("Processing field data")

  ff <- readr::read_csv(field_data,  name_repair = "minimal", show_col_types = FALSE)

  # rename the fields and drop any that are not needed

 indata <- ff |>
    dplyr::rename("objectid" = .data$ObjectID,
           "globalid" = .data$GlobalID,
           "datetime" = .data$`Date and time`,
           "Wetland_ID" = .data$`Preassigned wetland ID number (region initials followed by a number ie SI_1234)`,
           "surveyors" = .data$`Who completing the assessment?`,
           "region" = .data$`Which region of BC are you located in?`,
           "x1" =  .data$`If you are in one of the specified biogeoclimatic zones common in the Southern Interior and Central Interior at lower elevations (PP, BG, IDFxh, IDFxw,or IDFxm), are you including a 10m buffer around the wetland as part of your AA?`,
           "x2" = .data$`Logo_note`,
           "F1" = .data$`Following EACH row below, indicate the percentage of the vegetated part of the AA (excluding submerged and floating-leaved aquatics) which is occupied by that type of woody cover. Woody cover should include woody plants beneath a canopy of taller vegetation. Percentages may sum to less than 100% (if vegetation is largely herbaceous or moss) or more than 100% (if multiple vertical strata of woody plants are present). If no woody vegetation is present, leave this question blank.`,
           "F1_1"=  .data$`coniferous (including tamarack) taller than 3 metres.`,
           "F1_2" = .data$`deciduous taller than 3 m`,
           "F1_3" = .data$`coniferous or evergreen 1-3 m tall (e.g., stunted black spruce)`,
           "F1_4" = .data$`deciduous 1-3 m tall`,
           "F1_5" = .data$`coniferous or evergreen <1 m tall (e.g., many ericaceous shrub species)`,
           "F1_6" = .data$`deciduous <1 m tall (e.g., deciduous tree seedlings)`,
           "x4" =  .data$`Picture`,
           "F2_0" = .data$`If <1% of the vegetated AA contains woody vegetation taller than 3 m, leave this question blank and SKIP to F4. Otherwise, follow the key below and mark ONE option that best describes MOST of the vegetated part of the AA.`,
           "F3_0" = .data$`Mark ALL the types that comprise >5% of the woody canopy cover in the AA or >5% of the wooded areas (if any) along its upland edge (perimeter). The edge should include only the trees whose canopies extend into the AA`,
           "F4_0" = .data$`If <5% of the AA has short (<1 m) woody cover, SELECT N/A. Otherwise, determine which two woody plant genera comprise the greatest portion of the short (<1 m) woody cover. Then choose one:`,
           "F5_0" = .data$`Within the AA, willows taller than 2 m comprise ___% of the vegetated cover, in the AA or along its water edge (whichever has more).`,
           "F6_0" = .data$`Woody plants that have flowers with conspicuous petals at some time of the year comprise ___% of the vegetated part of the AA.`,
           "F7_0" = .data$`Woody plants that potentially produce succulent fruits or berries comprise ___% of the vegetated part of the AA.`,
           "F8_0" =.data$`The number of large snags (diameter >20 cm) in the AA plus adjacent upland area within 10 m of the wetland edge is:`,
           "F9_0" = .data$`The number of downed wood pieces longer than 2 m and with diameter >10 cm, and not persistently submerged, is __ per 10 x 10 m plot.`,
           "F10_0" = .data$`The cover of mosses that form a dense cushion many centimeters thick (i.e., Sphagnum and other peat-forming species), including the moss obscured by taller sedges, shrubs, and other plants rooted in it, is ___% of the vegetated part of the AA.`,
           "F11_0" = .data$`The areal cover of forbs (plants with conspicuous flowers at any time of year) reaches an annual maximum of ___% of the vegetated part of the AA.`,
           "F12_0" = .data$`Sedges (Carex spp.) and cottongrass (Eriophorum spp.) that form tussocks (raised mounds with dense stems and deep roots) occupy ___% of the vegetated area.`,
           "F13_0" = .data$`The extent of invasive plant cover in the vegetated AA is --% of the herbaceous cover (or woody cover, if the invasives are woody). The BC Report Invasives Andoid/iOS App or the Invasive Species Council of BC invasive species library may be used as resources`,
           "F14_0" = .data$`The percentage of the vegetated cover in the AA or along its water edge (whichever has more) that contains nitrogen-fixing plants is ___.`,
           "F15_0" = .data$`The extent of bare soil or sediment at mid-summer (excluding parts not visible because under water or snow) is:`,
           "F16_0" = .data$`During any 2 consecutive weeks of the growing season, the extent of waters that are both shallower than 5 cm and not shaded by vegetation, added to areas that have bare saturated substrate (e.g., mudflat) that similarly have sparse or no vegetation canopy, are:`,
           "F17_0" =.data$`In parts of the AA that lack persistent water, the texture of soil in the uppermost layer is mostly: [To determine this, use a trowel to check in at least 3 different topographic positions within the site.]`,
           "F18_0" =.data$`Imagine the AA without any living vegetation (other than moss, if any). Excluding the portion of the AA that is always under water, the amount of hummocks, small pits, raised mounds, animal burrows, ruts, gullies, natural levees, microdepressions, and other areas of peat or mineral soil that are  raised or depressed >10 cm compared to most of the area within a few meters surrounding them is:`,
           "x5" = .data$`Picture2`,
           "F19_0" = .data$`The percentage of the AA that never contains surface water during an average year (that is, except perhaps for a few hours after snowmelt or rainstorms), but which is still a wetland, is:`,
           "F20_0" = .data$`The percentage of the AA's area that is between the annual high water and the annual low water (surface water) is: (must add to ~100 with F19 and F21)`,
           "F21_0" = .data$`Identify the parts of the AA that still contain surface water (flowing or ponded, open or hidden beneath vegetation) even during the driest times of a normal year, i.e., when the AA's surface water is at its lowest annual level. At that time, the percentage of the AA that still contains surface water is: (must add to ~100 with F19 and F20)`,
           "F22_0" = .data$`During most of the year, is open water within or abutting the vegetated part of the wetland much wider than the maximum width of the vegetated zone within the wetland?`,
           "F23_0" = .data$`Is the vegetated part of the AA within or abutting a body of standing open water whose size exceeds 8 hectares during most of a normal year?`,
           "F24_0" = .data$`At mid-day during the warmest time of year, the amount of surface water within the AA that is shaded by vegetation and other features that are within the AA at that time is ___% of the total surface water:`,
           "F25_0" = .data$`The annual vertical fluctuation in unfrozen surface water within most of the parts of the AA that contain surface water at least temporarily is:`,
           "F26_0" = .data$`During most of the time when surface water is present in the AA during the growing season, its depth in most flooded areas of the AA is:`,
           "F27_0" = .data$`During most times when surface water is present, the percentage -- with or without inundated vegetation -- that is ponded (stagnant, or flows so slowly that fine sediment is not held in suspension) is:`,
           "F28_0" = .data$`During most of the growing season the cover for fish that is provided NOT by living vegetation, but by accumulations of  partly-submerged dead wood, undercut banks, and/or fish-accessible water deeper than 1 m  is:`,
           "F29_0" = .data$`Within or abutting the AA, the largest ponded surface water patch, with or without inundated vegetation, that remains flooded to a depth of >10 cm for at least 3 weeks of most growing seasons comprises:`,
           "F30_0" = .data$`Within the AA, the largest ponded surface water patch, with or without inundated vegetation, that remains flooded to a depth of >10 cm for at least 3 consecutive weeks during the usual growing season comprises:`,
           "F31_0" = .data$`Including any open water abutting the AA, the summed area of all patches with open water (ponded or flowing) during most of the growing season.`,
           "F32_0" = .data$`Most of the time when surface water is present, the direct distance (fetch) measured along the longest dimension of open ponded water in the AA (or channel width if there is no ponded water) and possibly extending into any abutting waters is:`,
           "F33_0" = .data$`At the time during the growing season when the AA's water level is lowest, the average width that seperates adjoining uplands from edge of open water within the AA:`,
           "F34_0" = .data$`Most of the time when surface water is present, the distance from the edge of the largest body of open water within the AA to the nearest sizeable stand of tall dense woody cover (>1 ha, >2 m tall, >60% crown closure), either in the wetland or in upland, is`,
           "F35_0" = .data$`During most of the part of the growing season when surface water is present, the spatial pattern of inundated vegetation within the open water (or the open water within the vegetation) is mostly:`,
           "F36_0" = .data$`During most of the part of the growing season when water is present, the percentage of the AA's water edge length that is abutted by steep (>30% slope) unvegetated banks that are >1 m high is:`,
           "F37_0" = .data$`The percentage of the AA that contains sedge tussocks, tall bulrush, cattail, or living woody vegetation that  remains partially underwater  for more than 2 weeks of the growing season annually is:`,
           "F38_0" = .data$`Aquatic vascular plants that live mostly underwater or on the water surface`,
           "F39_0" = .data$`The appearance of surface water that enters the AA and is most prevalent during the growing season is:`,
           "F40_0" = .data$`The most persistent surface water connection (outlet channel or pipe, ditch, or overbank water exchange) between the AA and a downslope stream network is:`,
           "F41_0" = .data$`During major runoff events, in the places where surface water exits the AA or connected waters nearby, the water:`,
           "F42_0" = .data$`Surface water from a tributary channel that is >100 m long flows into the AA for >3 consecutive weeks most years (seasonal or perennial input). Or, surface water from a larger permanent water body abutting the AA spills or backs into the AA for at least 2 consecutive days most years. If it enters only via a pipe, that pipe must be fed by a mapped stream or lake. If no,  SKIP to F45  (pH Measurement)`,
           "F43_0" = .data$`During its travel through the AA at the time of peak annual flow, water arriving in channels: [select only the ONE statement encountered by  most  of the incoming water].`,
           "F44_0" = .data$`The gradient along most of the flow path (in channel or as diffuse runoff or seepage) within the AA is:`,
           "F45_0" = .data$`The pH in most of the AA's surface water:`,
           "F45_1" = .data$`pH measurement`,
           "F46_0" = .data$`Was EC (Electrical Conductivity) measured?`,
           "F46_1" = .data$`Enter EC in μS/cm`,
           "F47_0" = .data$`Select the first applicable choice:`,
           "F48_0" = .data$`Use of the AA by beaver during the past 5 years is (select most applicable ONE):`,
           "F49_0" = .data$`Within a 30 m-wide buffer around the AA (or a 50 m-wide buffer if the AA is >5 ha), are there roads, trails, buildings, or any other human-associated features, or areas burned intensively during the past 5 years, that have reduced vegetation normally present on any side of this AA. If no,  SKIP to F53.`,
           "F50_0" = .data$`Within that 30 m buffer (or 50 m - if AA >5 ha), the percentage that contains water or vegetation taller than 10 cm (e.g., not lawns, most row crops, heavily grazed lands, bare ground, buildings, pavement) is:`,
           "F51_0" = .data$`Within 30 m (or 50 m - if AA >5 ha) upslope of where the wetland transitions to upland, the upland land cover that is NOT perennial vegetation is mostly (mark ONE):`,
           "F52_0" = .data$`The average percent slope of the upland between the most disturbed part of the 30 m (or 50 m - if AA >5 ha) buffer (the part lacking perennial native vegetation) and the edge of the AA that it potentially drains into is:`,
           "F53_0" = .data$`The distance from the wetland edge to the nearest suitable nest sites for pollinator colonies or nesting swallows such as a steep mostly-bare bank, or human-made features ie. a bridge, building, artificial nest structure, or other human-made features.`,
           "F54_0" = .data$`The closest wells or water bodies that currently provide drinking water are:`,
           "F55_0" = .data$`More than 1% of the AA's previously vegetated area (select the first true condition):`,
           "F56_0" = .data$`Assuming access permission was granted, select  ALL  statements that are true of the AA as it currently exists:`,
           "F57_0" = .data$`Recent evidence was found within the AA of the following potentially-sustainable consumptive uses. Select  ALL  that apply.`,
           "F58_0" = .data$`Although WESP does not require undertaking formal surveys for the following wetland-associated species, mark all those observed directly by yourself or other qualified observers you are working with onsite. For animals, indirect evidence noted during the site visit may be considered.`,
           "F58_A" = .data$`If any rare plant species were observed, enter those names here.`,
           "F58_B" = .data$`If any rare plant communities were detected, enter those names here.`,
           "F58_C" = .data$`If any rare amphibian species were detected, enter those names here.`,
           "F58_D" = .data$`If any rare waterbirds were observed, enter those names here.`,
           "F58_E" = .data$`If any rare wetland bird species were detected, enter those names here.`,
           "F58_F" = .data$`If any rare reptile species were detected, enter those names here.`,
           "Cultural_1" = .data$`On a separate vegetation list with dominant vegetation estimates, also record all culturally significant plants found in this wetland. Then estimate the percentage of the wetland's vegetated area occupied by all of those together.`,
           "Wet_Plant" = .data$`Enter all wetland plant associations comprising more than 1% of the AA's vegetated area. Enter each plant association name here and the coorisponding estimated percent. I.e., if you had a Ws02 that made up 70%, and Wm01 that made up 30%, enter Ws02 70, Wm01 30.`,
           "F59_0" = .data$`How many types of wetland plant associations occur in the wetland?`,
           "S1" = .data$`Select any item that is likely to have caused the timing of water inputs (but not necessarily their volume) in the AA to shift by hours, days, or weeks, becoming either more flashy (larger or more frequent spikes but over shorter times) or more muted (smaller or less frequent peaks spread over longer times, more temporal homogeneity of flow or water levels) [SFTS,FH, PH,STR]`,
           "S1_11" = .data$`Spatial extent within the AA shift`,
           "S1_12" = .data$`If any aberrant timing of inputs or outputs was recorded, mark when most of the timing shift began.`,
           "S1_13" = .data$`Mark the shift of timing input now vs. previously`,
           "S1_14" = .data$`Mark the degree of flashiness vs. muting`,
           "S2" = .data$`Please mark next to any item -- occurring in either the wetland or its CA -- that is likely to have accelerated the inputs of nutrients to the wetland. [APP, CS, STR]`,
           "S2_6" = .data$`If any accelerated inputs were marked, please indicate the type of loading`,
           "S2_7" =.data$`If any accelerated inputs were marked, please indicate the frequency and duration of input`,
           "S2_8" = .data$`If any accelerated inputs were marked, please indicate AA proximity to main sources (actual or potential):`,
           "S3" = .data$`Please mark any item -- occurring in either the wetland or its CA -- that is likely to have accelerated the inputs of contaminants or salts to the AA. [NRR,POL, STR]`,
           "S3_10" = .data$`If any accelerated inputs were marked, indicate the usual toxicity of most toxic contaminants:`,
           "S3_11" = .data$`If any accelerated inputs were marked, indicate the frequency & duration of input:`,
           "S3_12" = .data$`If any accelerated inputs were marked, indicate AA proximity to main sources (actual or potential):`,
           "S4" = .data$`Please mark next to any item present in the CA that is likely to have elevated the load of waterborne or windborne sediment reaching the wetland from its CA. [SRv, APP, FH, PH, STR]`,
           "S4_9" = .data$`If any excessive sediment loading was marked, please indicate evidence type`,
           "S4_10" = .data$`If any excessive sediment loading was marked, please indicate recentness of significant soil disturbance in the CA`,
           "S4_11" = .data$`If any excessive sediment loading was marked, please indicate duration of sediment inputs to the wetland`,
           "S4_12" = .data$`If any excessive sediment loading was marked, please indicate AA proximity to actual or potential sources:`,
           "S5" = .data$`Please mark any item present in the wetland that is likely to have compacted, eroded, or otherwise altered the wetland's soil. Consider only items occurring within past 100 years or since wetland was created or restored (whichever is less). [PSv, SS, SSv, SR, NRRv, CAC, PH, CS, STR]`,
           "S5_9" = .data$`If any soil or sediment alteration was marked, please indicate the spatial extent of altered soil`,
           "S5_10" = .data$`If any soil or sediment alteration was marked, please indicate recentness of significant soil alteration in wetland`,
           "S5_11" = .data$`If any soil or sediment alteration was marked, please indicate duration`,
           "S5_12" = .data$`If any soil or sediment alteration was marked, please indicate timing of soil alteration`,
           "S6" = .data$`Please mark any item present in the wetland that is likely to have increased the potential for disturbance of wildlife. [WBH, OBH, STR]`,
           "S6_3" = .data$`If any items were marked above, please indicate the frequency of the disturbance`,
           "S6_4" = .data$`If any items were marked above, please indicate the duration of the disturbance`,
           "Obs_1Landcover" = .data$`Primary Observed Landcover (required)`,
           "Obs_2Landcover" = .data$`Secondary Observed Landcover`,
           "Obs_3Landcover" = .data$`Tertiary Observed Landcover`,
           "Obs_1Disturbance" = .data$`Primary Observed Disturbance (required)`,
           "Obs_2Disturbance" = .data$`Secondary Observed Disturbance`,
           "Obs_3Disturbance" = .data$`Tertiary Observed Disturbance`,
           "Note_feat" = .data$`Other Notable Features`,
           "Comments" = .data$`Comments on site or observations and appropriate considerations`,
           "CreationDate"= .data$`CreationDate`,
           "Creator" = .data$`Creator`,
           "EditDate" = .data$`EditDate`,
           "Editor" = .data$`Editor`,
           "x" =  .data$`x`,
           "y" = .data$`y`
    )


  # update the names of columns to be able to map to questions

  #1) format Date and time type

  indata <- indata  |>
    #dplyr::filter(.data$region == EcoP) |>
    dplyr::mutate_all(as.character) |>
    #dplyr::mutate(date = format(as.POSIXct(.data$datetime, format = "%m/%d/%Y %H:%M:%S"), format = "%m/%d/%Y")) %>%
    dplyr::mutate(date = lubridate::mdy_hms(.data$datetime)) |>
    dplyr::mutate(date = lubridate::date(.data$date)) |>
    dplyr::rename("Wetland_Co" = .data$Wetland_ID) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., ""))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~stringr::str_trim(.))) |>
    dplyr:: mutate(dplyr::across(dplyr::where(is.character), ~na_if(., ""))) # not sure if this is actually needed.

  # check if date is read in correctly
  if (anyNA(indata$date)) {
    cli::cli_alert_danger("Some dates are incompatible formate, please check datetime
                        column and ensure all date is in the format dd/mm/yyyy,
                        rows which do not have this format will be removed")
    indata <- indata |>
      dplyr::filter(!is.na(.data$datetime))
  }

  #2) check ecoregion are within the acceptable range

  fn_reg <- unique(indata$region)
  accep_regions <- c( "GD", "CI", "SIM", "BTP", "NBM", "SBI", "SI", "CM")

  if(all(fn_reg %in% accep_regions) == FALSE){
    cli::cli_abort("some entries contain invalid regions. Please ensure all regions are one of the following: ")
  }

  #3) check unique site names

  if(!all(duplicated(indata$Wetland_Co)) == FALSE){
    cli::cli_abort("There are duplicate wetland_ids in this dataset, please check there are no duplicate")
  }

  # Missing fields check
  indata <- indata |>
    dplyr::select(.data$Wetland_Co, dplyr::starts_with(c("F", "S"))) |>
    dplyr::select(-c("surveyors"))

  # check the fields are all present.
  standard_cols <- c(
    "Wetland_Co", "F1_1", "F1_2", "F1_3", "F1_4", "F1_5",
    "F1_6", "F2_0", "F3_0", "F4_0", "F5_0", "F6_0", "F7_0",
    "F8_0", "F9_0", "F10_0", "F11_0", "F12_0",
    "F13_0", "F14_0", "F15_0", "F16_0", "F17_0", "F18_0",
    "F19_0", "F20_0", "F21_0", "F22_0", "F23_0", "F24_0",
    "F25_0", "F26_0", "F27_0", "F28_0", "F29_0", "F30_0",
    "F31_0", "F32_0", "F33_0", "F34_0", "F35_0", "F36_0",
    "F37_0", "F38_0", "F39_0", "F40_0", "F41_0", "F42_0",
    "F43_0", "F44_0", "F45_0", "F45_1", "F46_1", "F46_0", "F47_0",
    "F48_0", "F49_0", "F50_0", "F51_0", "F52_0", "F53_0",
    "F54_0", "F55_0", "F56_0", "F57_0", "F58_0", "F59_0",
    "S1", "S1_11", "S1_12", "S1_13", "S1_14", "S2",
    "S2_6", "S2_7", "S2_8", "S3", "S3_10", "S3_11",
    "S3_12", "S4", "S4_9", "S4_10", "S4_11", "S4_12",
    "S5", "S5_9", "S5_10", "S5_11", "S5_12", "S6",
    "S6_3", "S6_4"
  )

  missing_cols <- dplyr::setdiff(standard_cols, colnames(indata))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("The following columns are missing from the data : {missing_cols}}")
    cli::cli_alert_danger("Please check the input data and ensure all columns are present")
  }


  #############################################################
  # Format Field data

  WForm4 <- processing_fielddata(indata = indata)

  # openxlsx::write.xlsx(WForm4, fs::path(out_dir, "wesp_f_csv.xlsx"))
  # need to check if this output matches the other wesp_f_csv.xlsx matches the otiginal wesp_f.xlsx


  if (write_subfiles) {
    readr::write_csv(WForm4, fs::path(out_dir, "wesp_f.csv"))
  }

  cli::cli_alert_success("Field data sucessfully processed")


  #############################################################
  # 3) Format stressor data:

  cli::cli_alert("Processing stressor data")

  WFormS3 <- processing_stressordata(indata = indata)


  if(write_subfiles) {
    readr::write_csv(WFormS3,  fs::path(out_dir, "wesp_s.csv"))
  }

  cli::cli_alert_success("Stressor data sucessfully processed")


  #############################################################
  # 3) Format office data:

  cli::cli_alert("Processing desktop analysis data")

  ofdata <- readr::read_csv(desktop_data,  name_repair = "minimal", show_col_types = FALSE)

  WFormOF <- processing_officedata(indata = ofdata)

  if(write_subfiles) {
    readr::write_csv(WFormOF ,  fs::path(out_dir, "wesp_s.csv"))
  }


  # merge all the components together

  fd <- WForm4
  sd <- WFormS3
  osd <- WFormOF

  # check length of outputs

  if (nrow(fd) != nrow(sd)) {
    cli::cli_alert_danger("Field and stressor data do not have the same number of rows")
  }
  if (nrow(fd) != nrow(osd)) {
    cli::cli_alert_danger("Field and office data do not have the same number of rows")
  }

  wespF <- list(fd, osd, sd)

  wesp.1 <- wespF %>%
    purrr::reduce(dplyr::full_join, by = "Wetland_Co") %>%
    dplyr::select(-dplyr::contains("_0"))

  wesp <- stats::setNames(as.data.frame(t(wesp.1)), row.names(wesp.1))
  colOrder <- stringr::str_sort(rownames(wesp), numeric = TRUE)

  wesp <- wesp[match(colOrder, rownames(wesp)), ]

  # where there is only one column this causes issues as convert to vector
  if (is.vector(wesp)){
    wesp <- as.data.frame(wesp, row.names = colOrder)
    colnames(wesp)<- '1'
  }

  wesp <- tibble::rownames_to_column(wesp, var = "Question")

  # convert NAs to 0
  wesp[is.na(wesp)] <- as.character(0)

  return(wesp)
}
