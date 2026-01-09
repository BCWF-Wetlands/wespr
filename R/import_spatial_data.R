#' Download base spatial information layers to support desktop analysis for Wespr
#'
#' @param aoi An `sf` object (e.g. polygon) of designated wetland
#' @param out_dir A character string of filepath which points to output location.
#'
#' @return path to the output directory where files are written (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' #import_spatial_data(aoi, out_dir = fs::path("./temp/spatial_layers"))
#' }
#'
#'
import_spatial_data <- function(aoi = NA, out_dir = NA) {

  # testing line
  # aoi <- sf::st_read(fs::path("./temp/spatial_layers/aoi_example.gpkg"))
  # end testing line

  # check aoi is an sf object and the crs is in EPSG:3005
  if(!inherits(aoi, "sf")) {
    stop("The 'aoi' parameter must be an 'sf' object.")
  }
  if(sf::st_crs(aoi)$epsg != 3005) {
    stop("The 'aoi' parameter must have a coordinate reference system of EPSG:3005.")
  }


  # check that the out_dir exists, if not create it
  if(!fs::dir_exists(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
    cli::cli_alert_info("Created output directory at {.path {out_dir}}")
  }


  # buffer_2km_nowetland <- sf::st_buffer(aoi, dist = 2000)
  # sf::st_write(buffer_2km_nowetland, fs::path(out_dir, "aoi_buffer_2km.gpkg"), append = FALSE, quiet =TRUE)
  # differenced <- sf::st_difference(buffer_2km_nowetland, aoi)
  # sf::st_write(differenced, fs::path(out_dir, "aoi_buffer_2km_nowetland.gpkg"), append = FALSE, quiet =TRUE)
  #
  # buffer_100m_nowetland <- sf::st_buffer(aoi, dist = 100)
  # sf::st_write(buffer_100m_nowetland, fs::path(out_dir, "aoi_buffer_100m_inclwetland.gpkg"), append = FALSE, quiet =TRUE)
  # differenced <- sf::st_difference(buffer_100m_nowetland, aoi)
  # sf::st_write(differenced, fs::path(out_dir, "aoi_buffer_100m_nowetland.gpkg"), append = FALSE, quiet =TRUE)
  #
  # buffer_1km <- sf::st_buffer(aoi, dist = 1000)
  # sf::st_write(buffer_1km, fs::path(out_dir, "aoi_buffer_1km.gpkg"), append = FALSE, quiet =TRUE)
  #
  # buffer_5km <- sf::st_buffer(aoi, dist = 5000)
  # sf::st_write(buffer_5km, fs::path(out_dir, "aoi_buffer_5km.gpkg"), append = FALSE, quiet =TRUE)
  #
  buffer_10km <- sf::st_buffer(aoi, dist = 10000)
  sf::st_write(buffer_10km, fs::path(out_dir, "aoi_buffer_10km.gpkg"), append = FALSE, quiet =TRUE)


  # 1) Watershed Assessment Unit
  # https://catalogue.data.gov.bc.ca/dataset/97d8ef37-b8d2-4c3b-b772-6b25c1db13d0

  wau <- bcdata::bcdc_query_geodata("97d8ef37-b8d2-4c3b-b772-6b25c1db13d0") |>
   bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
   bcdata::collect()

  # check that the buffer_10km interesects the entire polygon
  wau_check_id <- bcdata::bcdc_query_geodata("97d8ef37-b8d2-4c3b-b772-6b25c1db13d0") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select("WATERSHED_FEATURE_ID") |>
    bcdata::collect() |>
    dplyr::select(.data$WATERSHED_FEATURE_ID, .data$AREA_HA) |>
    sf::st_drop_geometry()

  wau_area <- wau |>
    dplyr::filter(.data$WATERSHED_FEATURE_ID %in% wau_check_id$WATERSHED_FEATURE_ID) |>
    sf::st_area()

  if(any(as.numeric(wau_area) < (0.9 * (wau_check_id$AREA_HA * 10000)))) {
    cli::cli_alert_warning("The 10 km buffer does not fully intersect the Watershed Assessment Unit(s). Consider increasing the buffer size to ensure complete data capture.")
  }


  cli::cat_line()
  cli::cli_alert_success("Watershed Assessment Unit downloaded and to written to {.path {out_dir}}")

  sf::st_write(wau, fs::path(out_dir, "wau.gpkg"), append = FALSE, quiet =TRUE)



  # 2) Fire historic Perimeters
  fire <- bcdata::bcdc_query_geodata("22c7cb44-1463-48f7-8e47-88857f207702") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("FIRE_YEAR") |>
    bcdata::collect() |>
    dplyr::filter(.data$FIRE_YEAR > 2010)

  if (nrow(fire) > 0) {
    # can be zero
    sf::st_write(fire, fs::path(out_dir, "fire_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Fire data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No fires in area of interest")
  }


  # TODO: This dataset has to be manually downloaded first
  # 3) BC Cumulative Effects Framework  Human Disturbance (Government of British Columbia, 2023a)
  # needs to be downloaded
  # https://catalogue.data.gov.bc.ca/dataset/7d61ff12-b85f-4aeb-ac8b-7b10e84b046c # needs to be downloaded
  # BC Cumulative Effects Framework  Human Disturbance (Government of British Columbia, 2023a)
  # Natural vs disturbed
  # Disturbed = Agriculture and clearing, cutblocks, mining and extraction, OGC Geophysical, OGC Infrastructure, Power, ROW, Rail and infrastructure, Urban
  # Natural = all other categories (under CEF Disturb Group) 	OF 31, 32, 41

  # TODO: This dataset has to be manually downloaded first
  # 4) BC Cumulative Effects Framework Integrated Roads  2025 (Government of British Columbia, 2025a)
  # https://catalogue.data.gov.bc.ca/dataset/a489bc6a-f676-4503-8cd7-dcf0bdf2ae99
  # needs to be downloaded



  # 5) BC Historical Fish Distribution Points (50,000) (Government of British Columbia, 2006)
  # #3b723ef2-abd4-4722-abea-9a51258eae15

  fish_obs <- bcdata::bcdc_query_geodata("3b723ef2-abd4-4722-abea-9a51258eae15") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(fish_obs) > 0) {
    # can be zero
    sf::st_write(fish_obs, fs::path(out_dir, "fish_obs_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Fish Observations downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Fish Observations in area of interest")
  }


  # 6) Conservation Lands (Government of British Columbia, 2025c)
  # Could be replaced by designated lands dataset mentioned from 2018 below ?https://github.com/bcgov/designatedlands/releases/download/v0.1.0/designatedlands.gpkg.zip
  # https://catalogue.data.gov.bc.ca/dataset/68327529-c0d5-4fcb-b84e-f8d98a7f8612
  # OF 21, 22
  # Used for intersect
  cons_lands <- bcdata::bcdc_query_geodata("68327529-c0d5-4fcb-b84e-f8d98a7f8612") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(cons_lands) > 0) {
    # can be zero
    sf::st_write(cons_lands, fs::path(out_dir, "cons_lands_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Conservation Lands downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Conservation Lands  in area of interest")
  }


  # 7) Critical Habitat for Federally-Listed Species at Risk-Posted (Government of British Columbia, 2024a)
  # https://catalogue.data.gov.bc.ca/dataset/076b8c98-a3f1-429b-9dae-03faed0c6aef
  # OF 21
  crit_hab <- bcdata::bcdc_query_geodata("076b8c98-a3f1-429b-9dae-03faed0c6aef") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(crit_hab) > 0) {
    # can be zero
    sf::st_write(crit_hab, fs::path(out_dir, "crit_hab_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Critical Habitat downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Critical Habitat  in area of interest")
  }


  # TODO: This dataset has to be manually downloaded first
  # 8) Designated Lands (GitHub File)
  # https://github.com/bcgov/designatedlands/releases/download/v0.1.0/designatedlands.gpkg.zip
  # dowloadable link
  # vOF 21, 22


  # 9) Digital Road Atlas (DRA) Master Partially
  # Attributed Roads (Government of British Columbia, 2025d)
  # https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e
  # OF 2 (if applicable), 12, 30, 31, 42
  # Used for AA buffer intersects

  roads <- bcdata::bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("FEATURE_TYPE", "ROAD_NAME_FULL", "ROAD_SURFACE", "NUMBER_OF_LANES") |>
    bcdata::collect()

  cli::cat_line()
  cli::cli_alert_success("Roads downloaded and to written to {.path {out_dir}}")

  sf::st_write(roads, fs::path(out_dir, "roads_10km.gpkg"), append = FALSE, quiet =TRUE)


  # 10) Ecoprovinces - Ecoregion Ecosystem Classification of British Columbia (Government of British Columbia, 2004)
  # https://catalogue.data.gov.bc.ca/dataset/51832f47-efdf-4956-837a-45fc2c9032dd
  # fields could be tidied, keeping only ECOPROVINCE_NAME
  # OF 44

  ecop <- bcdata::bcdc_query_geodata("51832f47-efdf-4956-837a-45fc2c9032dd") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select("ECOPROVINCE_NAME") |>
    bcdata::collect()

  cli::cat_line()
  cli::cli_alert_success("Ecoprovince data downloaded and to written to {.path {out_dir}}")

  sf::st_write(ecop, fs::path(out_dir, "ecop_10km.gpkg"), append = FALSE, quiet =TRUE)


  # 11) Freshwater Atlas Lakes (Government of British Columbia, 2008b)
  # https://catalogue.data.gov.bc.ca/dataset/cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6
  # OF 3, 18, 19
  # Used for AA buffer intersects
  # Keeping:
  # Waterbody_polyID
  # Watershed_group_ID
  # Waterbody_type
  # Area_Ha

  fwa_lakes <- bcdata::bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("WATERBODY_POLY_ID", "WATERSHED_GROUP_ID", "WATERBODY_TYPE", "AREA_HA") |>
    bcdata::collect() |>
    dplyr::select(-.data$FEATURE_CODE, -.data$OBJECTID, -.data$WATERSHED_GROUP_CODE)


  if (nrow(fwa_lakes) > 0) {
    # can be zero
    sf::st_write(fwa_lakes, fs::path(out_dir, "fwa_lakes_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas Lakes downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas Lakes in area of interest")
  }



  # 12) Freshwater Atlas Rivers (Government of British Columbia, 2025e)
  # https://catalogue.data.gov.bc.ca/dataset/f7dac054-efbf-402f-ab62-6fc4b32a619e

  # Clean up fields (keeping only):
  # GNIS_NAME_1
  # WATERSHED_GROUP_ID
  # FEATURE_CODE_50K, others?	OF 6, 9

  fwa_rivers <- bcdata::bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("GNIS_NAME_1", "WATERSHED_GROUP_ID") |> # ,"FEATURE_CODE_50K") |>
    bcdata::collect() |>
    dplyr::select(
      -.data$WATERBODY_POLY_ID, -.data$WATERBODY_TYPE, -.data$WATERSHED_GROUP_CODE,
      -.data$AREA_HA, -.data$OBJECTID
    )

  if (nrow(fwa_rivers) > 0) {
    # can be zero
    sf::st_write(fwa_rivers, fs::path(out_dir, "fwa_rivers_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas Rivers downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas Rivers in area of interest")
  }



  # 13) Freshwater Atlas Stream Directions (Government of British Columbia, 2023b)
  # https://catalogue.data.gov.bc.ca/dataset/d7165359-52ef-41d0-b762-c53e3468ff3f
  # Maintain all fields and features	OF 6, 8, 9, 10, 11

  fwa_stdir <- bcdata::bcdc_query_geodata("d7165359-52ef-41d0-b762-c53e3468ff3f") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()


  if (nrow(fwa_stdir) > 0) {
    # can be zero
    sf::st_write(fwa_stdir, fs::path(out_dir, "fwa_stdir_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas Stream Directions downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas Stream Directions in area of interest")
  }



  # 14) Freshwater Atlas Glaciers (Government of British Columbia, 2008a)
  # https://catalogue.data.gov.bc.ca/dataset/8f2aee65-9f4c-4f72-b54c-0937dbf3e6f7
  # Remove fields except WATERSHED_GROUP_CODE, WATERSHED_GROUP_ID	OF 8

  fwa_glacier <- bcdata::bcdc_query_geodata("8f2aee65-9f4c-4f72-b54c-0937dbf3e6f7") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("WATERSHED_GROUP_CODE", "WATERSHED_GROUP_ID") |>
    bcdata::collect()

  if (nrow(fwa_glacier) > 0) {
      sf::st_write(fwa_glacier, fs::path(out_dir, "fwa_glacier_10km.gpkg"), append = FALSE, quiet =TRUE)
      cli::cat_line()
      cli::cli_alert_success("Freshwater Atlas Glaciers downloaded and to written to {.path {out_dir}}")
    } else {
      cli::cli_alert_warning("No Freshwater Atlas Glaciers in area of interest")
    }



  # 15) Freshwater Atlas Stream Network (Government of British Columbia, 2024b)
  # https://catalogue.data.gov.bc.ca/dataset/92344413-8035-4c08-b996-65a9b3f62fca

  # Remove features that have the following (EDGE_TYPE)
  # Flow Connector (1450)
  # Isolated (1400)
  # Waterbody Skeletons (1200,1300,
  # Underground Connector (1425)
  # Inferred Connector (1410)
  #
  # Clean up attributes, keeping only:
  # Edge_type
  # Linear_feature_ID
  # Watershed_group_ID
  # Feature_code
  # GNES_name
  # OF 6, 8, 9, 10, 11

  fwa_stn <- bcdata::bcdc_query_geodata("92344413-8035-4c08-b996-65a9b3f62fca") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select(
      "EDGE_TYPE", "LINEAR_FEATURE_ID", "WATERSHED_GROUP_ID",
      "FEATURE_CODE", "GNIS_NAME"
    ) |>
    bcdata::filter(-data$EDGE_TYPE %in% c(1450, 1400, 1200, 1300, 1425, 1410)) |>
    bcdata::collect() |>
    dplyr::select(-.data$LENGTH_METRE, -.data$FEATURE_SOURCE, -.data$STREAM_ORDER, -.data$STREAM_MAGNITUDE, -.data$OBJECTID)

  if (nrow(fwa_stn) > 0) {
    sf::st_write(fwa_stn, fs::path(out_dir, "fwa_stn_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas Stream Network downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas Stream Network in area of interest")
  }



  # 16) Freshwater Atlas  Watershed Groups (Government of British Columbia, 2008c)
  # Need to populate max and min elevation (m) fields for each watershed (by Watershed_Group_Code)
  # Future update to use regional lidar for more accurate elevations	OF 5, 6, 9
  # Used for AA buffer intersects
  # https://catalogue.data.gov.bc.ca/dataset/51f20b1a-ab75-42de-809d-bf415a0f9c62


  fwa_wsg <- bcdata::bcdc_query_geodata("92344413-8035-4c08-b996-65a9b3f62fca") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(fwa_wsg) > 0) {
    sf::st_write(fwa_wsg, fs::path(out_dir, "fwa_stn_10km.gpkg"), append = FALSE, quiet =TRUE)

    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas Watershed Groups downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas Watershed Groups data in area of interest")
  }


  # 17) Freshwater Atlas Watershed Boundaries (Government of British Columbia, 2025f)
  # https://catalogue.data.gov.bc.ca/dataset/ab758580-809d-4e11-bb2c-df02ac5465c9
  # Retain only these fields:
  #  WATERSHED_GROUP_CODE
  # FEATURE_SOURCE
  #  OBJECTID
  #  WATERSHED_GROUP_ID 	OF 11
  # Used for AA buffer intersects

  fwa_wsb <- bcdata::bcdc_query_geodata("ab758580-809d-4e11-bb2c-df02ac5465c9") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("WATERSHED_GROUP_CODE", "FEATURE_SOURCE", "OBJECTID", "WATERSHED_GROUP_ID") |>
    bcdata::collect() |>
    dplyr::select(-.data$WATERSHED_BOUNDARY_ID, -.data$EDGE_TYPE, -.data$LENGTH_METRE, -.data$FEATURE_CODE, -.data$OBJECTID)


  if (nrow(fwa_wsb) > 0) {
    sf::st_write(fwa_wsb, fs::path(out_dir, "fwa_wsb_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas Watershed Boundaries downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas Watershed Boundaries in area of interest")
  }



  # 18) Freshwater Atlas Wetlands (Government of British Columbia, 2025g)
  # https://catalogue.data.gov.bc.ca/dataset/93b413d8-1840-4770-9629-641d74bd1cc6
  # OF 19, 43
  # Used for AA buffer intersects

  wetlands <- bcdata::bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select(
      "WATERBODY_POLY_ID", "FEATURE_CODE", "FEATURE_AREA_SQM",
      "WATERSHED_GROUP_ID", "WATERSHED_GROUP_CODE", "OBJECTID"
    ) |>
    bcdata::collect()


  if (nrow(wetlands) > 0) {
    sf::st_write(wetlands, fs::path(out_dir, "fwa_wetlands_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Freshwater Atlas wetlands downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Freshwater Atlas wetlands in area of interest")
  }


  # 19) Geology Faults  (Government of British Columbia, 2018)
  # Maintain all attributes and features	OF 17
  # https://catalogue.data.gov.bc.ca/dataset/c94e0c13-5385-49c1-9922-822e10135fc6

  geo_fault <- bcdata::bcdc_query_geodata("c94e0c13-5385-49c1-9922-822e10135fc6") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(geo_fault) > 0) {
    sf::st_write(geo_fault, fs::path(out_dir, "geo_fault_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Geology Faults downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("Geology Faults in area of interest")
  }

  # 20) Glaciers (Government of British Columbia, 2025h)
  # Maintain all attributes and features	OF 8
  # https://catalogue.data.gov.bc.ca/dataset/832b4523-7853-4ec5-8f9c-4314a0154927

  glac2 <- bcdata::bcdc_query_geodata("832b4523-7853-4ec5-8f9c-4314a0154927") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(glac2) > 0) {
    sf::st_write(glac2, fs::path(out_dir, "geo_fault_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Glaciers downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("Geology Faults in area of interest")
  }



  # 21) Municipalities Legally Defined Administrative Areas of BC (Government of British Columbia, 2016)
  # https://catalogue.data.gov.bc.ca/dataset/e3c3c580-996a-4668-8bc5-6aa7c7dc4932
  # Clean up fields, keeping:
  #  ADMIN_AREA_NAME
  #  ADMIN_AREA_ABBREVIATION
  # OIC_MO_YEAR	OF 1
  municipals <- bcdata::bcdc_query_geodata("e3c3c580-996a-4668-8bc5-6aa7c7dc4932") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::select("ADMIN_AREA_NAME", "ADMIN_AREA_ABBREVIATION", "OIC_MO_YEAR") |>
    bcdata::collect()

  if (nrow(municipals) > 0) {
    sf::st_write(municipals, fs::path(out_dir, "municipals_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Municipality data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Municipality data in area of interest")
  }


  # 21.2) Towns
  # https://catalogue.data.gov.bc.ca/dataset/e3c3c580-996a-4668-8bc5-6aa7c7dc4932
  # Clean up fields, keeping:

  towns <- bcdata::bcdc_query_geodata("b678c432-c5c1-4341-88db-0d6befa0c7f8") |>
      bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
      bcdata::collect()

  if (nrow(towns) > 0) {
    sf::st_write(towns, fs::path(out_dir, "bc_major_towns.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("BC towns data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No major BC towns data in area of interest")
  }



  # 22) NGO Conservation Areas Fee Simple (Government of British Columbia, 2024c)
  # No modifications needed (small dataset)	OF 22
  # https://catalogue.data.gov.bc.ca/dataset/a306e21b-58d6-4b71-bac7-f3b1c8a4c779                                                         Used for AA buffer intersects

  ngo <- bcdata::bcdc_query_geodata("a306e21b-58d6-4b71-bac7-f3b1c8a4c779") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow( ngo ) > 0) {
    sf::st_write(ngo, fs::path(out_dir, "ngo_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("NGO Conservation Areas downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No NGO Conservation Areas in area of interest")
  }

  # 23) Reconnaissance Karst Potential Mapping (Government of British Columbia, 2002)
  # https://catalogue.data.gov.bc.ca/dataset/395568e1-d233-4217-9732-7afadb6f4265
  # No modifications needed (small dataset)	OF 16
  # Could be used for AA buffer intersects

  karst <- bcdata::bcdc_query_geodata("395568e1-d233-4217-9732-7afadb6f4265") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

   if (nrow( karst ) > 0) {
    sf::st_write(karst, fs::path(out_dir, "karst_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Karsts downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Karsts Faults in area of interest")
  }


  # 24) Species and Ecosystems at Risk Publicly Available Occurrences - CDC (Government of British Columbia, 2025i)
  # Suspect no modifications needed 	OF 21, 22, 24
  # https://catalogue.data.gov.bc.ca/dataset/0e035e55-f257-458f-9a96-80c01c69d389
  SAR <- bcdata::bcdc_query_geodata("0e035e55-f257-458f-9a96-80c01c69d389") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow( SAR ) > 0) {
    sf::st_write(SAR, fs::path(out_dir, "SAR_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Species and Ecosystems at Risk downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("NO Species and Ecosystems at Risk in area of interest")
  }



  # 25) TANTALIS Wildlife Management Areas (Government of British Columbia, 2025k)
  # Suspect no modifications needed 	OF 21, 22, 24
  # https://catalogue.data.gov.bc.ca/dataset/f3ece977-aa7f-4cb2-b7d0-de64155f6c83
  wma <- bcdata::bcdc_query_geodata("f3ece977-aa7f-4cb2-b7d0-de64155f6c83") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

   if (nrow (wma) > 0) {
    sf::st_write(wma, fs::path(out_dir, "wma_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Wildlife Management Areas downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Wildlife Management Areas in area of interest")
  }


  # 26) TANTALIS Conservancy Areas  (Government of British Columbia, 2025j)
  # No modifications needed (fairly simple dataset)	OF 21, 22
  # https://catalogue.data.gov.bc.ca/dataset/550b3133-2004-468f-ba1f-b95d0e281e78
  cons <- bcdata::bcdc_query_geodata("550b3133-2004-468f-ba1f-b95d0e281e78") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(cons) > 0) {
    # can be zero
    sf::st_write(cons, fs::path(out_dir, "cons_area_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Conservancy Areas data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No conservancy Areas in area of interest")
  }

  # 27) Ungulate Winter Range Approved (Government of British Columbia, 2025l)
  # OF 21
  # https://catalogue.data.gov.bc.ca/dataset/712bd887-7763-4ed3-be46-cdaca5640cc1
  uwr <- bcdata::bcdc_query_geodata("712bd887-7763-4ed3-be46-cdaca5640cc1") |>
    bcdata::filter(bcdata::INTERSECTS(buffer_10km)) |>
    bcdata::collect()

  if (nrow(uwr) > 0) {
    # can be zero
    sf::st_write(uwr, fs::path(out_dir, "uwr_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Ungulate winter range data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No ungulate winter range in area of interest")
  }

  # 28) VRI 2024 Forest Vegetation Composite Polygons (Government of British Columbia, 2024d)
  # https://catalogue.data.gov.bc.ca/dataset/6ba30649-14cd-44ad-a11f-794feed39f40

  # Retain attributes:
  # Site index
  # Site_Position_Meso
  # Bclcs_level_5
  # Bclcs_level_4 (forest type)
  # bclcs_level_3 (wetlands)
  # bclcs_level_2 (treed/non-treed/land/water)
  # bclcs_level_1 (vegetated or non-vegetated)	OF 28, 29, 38, 39, 40

  vri <- bcdata::bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::select(c("SITE_INDEX", "SITE_POSITION_MESO", "BCLCS_LEVEL_5", "BCLCS_LEVEL_4", "BCLCS_LEVEL_3", "BCLCS_LEVEL_2", "BCLCS_LEVEL_1")) |> # Treed sites
    bcdata::collect() #|>
  # dplyr::select(c("BCLCS_LEVEL_2", "BCLCS_LEVEL_4", "PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1"))

  sf::st_write(vri, fs::path(out_dir, "vri_10km.gpkg"), append = FALSE, quiet =TRUE)
  cli::cat_line()
  cli::cli_alert_success("VRI data downloaded and to written to {.path {out_dir}}")



  # 29) Wildlife Species Inventory Incidental Observations Publicly Available (Government of British Columbia, 2025o)
  # Proponent likely to submit CDC data request for masked data anyways	OF 24
  # https://catalogue.data.gov.bc.ca/dataset/7d5a14c4-3b6e-4c15-980b-68ee68796dbe

  wsi <- bcdata::bcdc_query_geodata("7d5a14c4-3b6e-4c15-980b-68ee68796dbe") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(wsi) > 0) {
    # can be zero
    sf::st_write(wsi, fs::path(out_dir, "wsi_10km.gpkg"), append = FALSE, quiet =TRUE)

    cli::cat_line()
    cli::cli_alert_success("Wildlife Species Inventory data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Wildlife Species Inventory in area of interest")
  }


  # 30) Wildlife Species Inventory Survey Observations Publicly Available (Government of British Columbia, 2025n)
  # Proponent likely to submit CDC data request for masked data anyways	OF 24
  # https://catalogue.data.gov.bc.ca/dataset/8f45a611-ce07-4e9f-a4b5-27e123972816

  wsi2 <- bcdata::bcdc_query_geodata("8f45a611-ce07-4e9f-a4b5-27e123972816") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(wsi2) > 0) {
    # can be zero
    sf::st_write(wsi2, fs::path(out_dir, "wsi2_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Wildlife Species Inventory data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Wildlife Species Inventory in area of interest")
  }


  # 31) Wildlife Species Inventory Telemetry Points Publicly Available (Government of British Columbia, 2025p)
  # Proponent likely to submit CDC data request for masked data anyways	OF 24
  # https://catalogue.data.gov.bc.ca/dataset/6d48657f-ab33-43c5-ad40-09bd56140845
  wsi3 <- bcdata::bcdc_query_geodata("6d48657f-ab33-43c5-ad40-09bd56140845") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(wsi3) > 0) {
    # can be zero
    sf::st_write(wsi3, fs::path(out_dir, "wsi3_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Wildlife Species Inventory data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Wildlife Species Inventory in area of interest")
  }

  # 32) Wildlife Habitat Features Incidental Observations Publicly Available  (Government of British Columbia, 2025m)
  # Proponent likely to submit CDC data request for masked data anyways	OF 24
  # https://catalogue.data.gov.bc.ca/dataset/50dc6ba5-8883-4bfc-b3aa-b420b190b45b

  wsi4 <- bcdata::bcdc_query_geodata("50dc6ba5-8883-4bfc-b3aa-b420b190b45b") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(wsi4) > 0) {
    # can be zero
    sf::st_write(wsi4, fs::path(out_dir, "wsi4_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Wildlife Species Inventory data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Wildlife Species Inventory in area of interest")
  }

  # 33) Wildlife Habitat Features Survey Observations Publicly Available  (Government of British Columbia, 2025n)
  # Proponent likely to submit CDC data request for masked data anyways	OF 24
  # https://catalogue.data.gov.bc.ca/dataset/884c20fa-17c1-491a-b5cb-993be5dff8d3

  wsi5 <- bcdata::bcdc_query_geodata("884c20fa-17c1-491a-b5cb-993be5dff8d3") |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()

  if (nrow(wsi5) > 0) {
    # can be zero
    sf::st_write(wsi5, fs::path(out_dir, "wsi5_10km.gpkg"), append = FALSE, quiet =TRUE)
    cli::cat_line()
    cli::cli_alert_success("Wildlife Species Inventory data downloaded and to written to {.path {out_dir}}")
  } else {
    cli::cli_alert_warning("No Wildlife Species Inventory in area of interest")
  }



  return(TRUE)
}


#import_spatial_data(aoi, out_dir = fs::path("./temp/spatial_layers"))
