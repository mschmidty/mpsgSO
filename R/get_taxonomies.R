#' adds taxonomy information from GBIF to any df with scientific names and returns a tibble. Returns gbif_taxonID which is the GBIF ID for the given scientific name and full taxonomy for the accepted taxonomy from the GBIF backbone taxonomies database. taxon_id as the accepted taxonomy ID from GBIF backbone.
#' @param x a dataframe containing at least on column including scientific name in species or subspecies form.
#' @param query_field the name of the field with scientific names. Defaults to "scientific_name"
#' @export
get_taxonomies <- function(x, query_field = "scientific_name") {
  sci_name <- x |>
    dplyr::pull(query_field) |>
    stringr::str_replace("[\r\n]", " ") |>
    stringr::str_replace("[\r\n]", "") |>
    stringr::str_to_sentence()



  orig_id <- taxize::get_gbifid(sci_name, ask = FALSE, rows = 1, messages = FALSE)
  class <- taxize::classification(orig_id, db = "gbif")

  convert_taxonomy <- function(i, x) {
    gbif_taxonID <- names(x)[[i]]

    if (!is.na(gbif_taxonID)) {
      named_taxonomy <- x[[i]] |>
        dplyr::select(rank, name) |>
        tidyr::pivot_wider(names_from = rank, values_from = name)

      final_id <- x[[i]] |>
        dplyr::filter(rank == "species") |>
        dplyr::pull(id)

      named_taxonomy |>
        dplyr::bind_cols(tibble(taxon_id = as.character(final_id))) |>
        dplyr::bind_cols(tibble(gbif_taxonID = gbif_taxonID))
    }
  }
  t <- lapply(seq_along(class), convert_taxonomy, class)

  all_taxonomies <- dplyr::bind_rows(t)

  x |>
    dplyr::bind_cols(tibble(gbif_taxonID = as.character(orig_id))) |>
    dplyr::left_join(all_taxonomies, by = "gbif_taxonID")
}
