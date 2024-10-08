# title: "Functions for Species Occurrence Data Pulls"
# author:
#   - name: "Matthew Van Scoyoc" 
#     affiliation: |
#       | Mountain Planning Service Group, Regions 1-4
#       | Forest Service, USDA
# date: 3 January, 2024
# 
# This script holds functions for reading and clipping feature classes from 
#     geodatabases and pulling G-ranks from NatureServe.
# -----------------------------------------------------------------------------





#' Add Missing Columns
#' This function adds missing columns to one data frame that are     
#' @param df1 
#' @param df2 
add_cols <- function(df1, df2){
  new_cols = colnames(df2)[!colnames(df2) %in% colnames(df1)]
  df1[new_cols] = NA
  return(df1)
}





#' Create project directories
#' This function evaluates if a project directory exists. If one does not, it 
#'     creates a project directory. This function also makes a copy of the 
#'     species data pull report (*.Rmd) in the project directory.
#'
#' @param unit_dir Character. File path of the project directory.
create_dir <- function(unit_dir){
  if(!dir.exists(unit_dir)){
    dir.create(file.path(unit_dir))
    message("Unit directory created.")
    } else(message("Unit directory already exist."))
  
  if(!dir.exists(file.path(unit_dir, "data"))){
    dir.create(file.path(unit_dir, "data"))
    message("`data` directory created.")
    } else(message("`data` directory already exist."))
  
  if(!dir.exists(file.path(unit_dir, "reproduce"))){
    dir.create(file.path(unit_dir, "reproduce"))
    message("`reproduce` directory created.")
    } else(message("`reproduce` directory already exist."))
  
  if(!file.exists(file.path(unit_dir, "reproduce", "state_nhp.R"))){
    file.copy(from = here::here('state_nhp.R'), 
              to = file.path(unit_dir, "reproduce", "state_nhp.R"))
    message("state_nhp.R copied to the `reproduce` directory.")
    } else(message("state_nhp.R already exists."))
  
  if(!file.exists(file.path(unit_dir, "reproduce", "gbif.R"))){
    file.copy(from = here::here('gbif.R'), 
              to = file.path(unit_dir, "reproduce", "gbif.R"))
    message("gbif.R copied to the `reproduce` directory.")
    } else(message("gbif.R already exists."))
  
  if(!file.exists(file.path(unit_dir, "reproduce", "idibgio.R"))){
    file.copy(from = here::here('idibgio.R'), 
              to = file.path(unit_dir, "reproduce", "idibgio.R"))
    message("idibgio.R copied to the `reproduce` directory.")
    } else(message("idibgio.R already exists."))
  
  if(!file.exists(file.path(unit_dir, "reproduce", "imbcr.R"))){
    file.copy(from = here::here('imbcr.R'), 
              to = file.path(unit_dir, "reproduce", "imbcr.R"))
    message("imbcr.R copied to the `reproduce` directory.")
    } else(message("imbcr.R already exists."))
  
  if(!file.exists(file.path(unit_dir, "reproduce",
                            paste0(unit_short, "_sppPullReport.Rmd")))){
    file.copy(from = here::here('sppPullReport.Rmd'), 
              to = file.path(unit_dir, "reproduce",
                             paste0(unit_short, "_sppPullReport.Rmd")))
    message("sppPullReport.Rmd copied to the `reproduce` directory.")
    } else(message("sppPullReport.Rmd already exists."))
  }





#' Create vector from comma separated variable (column)
#' This function creates a vector from a comma separated variable or column, 
#'     then removes NAs and blanks 
#'
#' @param id_var Input variable, usually df$var (e.g., spp_list$GBIF_taxonKey) 
id_vec <- function(id_var){
  vec = stringr::strsplit(id_var, split = ", ") |> unlist() |> as.numeric()
  vec = vec[!is.na(vec)] # Remove NA's
  vec = vec[vec != ""]   # Remove blanks
}





#' View duplicate species in a data frame
#'
#' @param spp_dat Data frame or tibble of species data.
#' @param spp_vec Vector of species names from the same data frame.
view_dups <- function(spp_dat, spp_vec){
  dups <- spp_vec[duplicated(spp_vec)]
  spp_dat[spp_vec %in% dups, ] |> View()
  }





#' Create a WTK string
#' Creates a well-known text string from a polygon (sf object).
#'
#' @param my_polygon sf object. An sf polygon object.
wkt_string <- function(my_polygon){
  sf::st_bbox(my_polygon) |> 
    sf::st_as_sfc() |> 
    sf::st_as_text()
  }
