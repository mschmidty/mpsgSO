#' Read feature class into R.
#' This function uses the `sf` package to read a feature class into R from a 
#'     geodatabase. It then checks that the sf object is valid and validates the 
#'     object if it is not valid.
#'
#' @param lyr Character. Feature class name.
#' @param dsn Character. Path to geodatabase that holds the feature class.
#'
#' @return sf objectread_fc <- function(lyr, dsn, crs){
#' @export
#' 
#' @examples
#' ## Not run:
#' # Read a feature class into R
#' read_lyr(lyr = "admin_bdy", dsn = file.path("T:/path/to/geodatabase"))
#' 
#' ## End (Not run)
read_fc <- function(lyr, dsn){
  sf_lyr = sf::read_sf(dsn = dsn, layer = lyr)
  if(!all(sf::st_is_valid(sf_lyr))) sf_lyr = sf::st_make_valid(sf_lyr)
  return(sf_lyr)
}





#' Clip sf object to polygon
#' 
#' This function clips a sf object using `sf::st_intersection()`. First, this 
#'     function checks that the coordinate reference system (CRS) of the input 
#'     object is the same as the clipping object. If it is not, this function 
#'     transforms the CRS of the input object to the clipping object using  
#'     `sf::st_transform()`before clipping. The returned sf object will be in 
#'     the clipping object CRS if they are not the same.
#'
#' @param sf_lyr  sf object. Spatial data to be clipped.
#' @param sf_clip sf polygon object. Spatial layer to clip data by.
#' @param locale  Character. Short description of clipped layer, usually the 
#'                    location (e.g., Forest acronym or "Buffer").
#'
#' @return sf object
#' @export
#' 
#' @examples
#' ## Not run:
#' # Clip species occurrence point data to the administrative boundary of a 
#' # forest.
#' clip_sf(sf_lyr = spocc_sf, sf_clip = admin_bdy_sf, locale = "USFS")
#' 
#' ## End (Not run)
clip_sf <- function(sf_lyr, sf_clip, locale){
  if(sf::st_crs(sf_lyr) != sf::st_crs(sf_clip)){
    sf_lyr = sf::st_transform(sf_lyr, crs = sf::st_crs(sf_clip))
  }
  sf::st_intersection(sf_lyr, sf_clip) |> 
    dplyr::mutate(locale = locale) |> 
    dplyr::select(-tidyselect::any_of(colnames(sf_clip)))
}
