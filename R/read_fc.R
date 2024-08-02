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