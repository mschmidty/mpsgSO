#' Generate a dataframe of species names with NatureServe G- and S-ranks
#'
#' This function uses the `natserv` package to query the NatureServe API
#'     and read those data into R. This function then convers the JSON
#'     data returned into a `tibble::tibble()`.
#
#' @param species_list A vector of species names.
#' @param states A vector of 2-letter US state codes. Default is NULL, if
#'     NULL all US states are returned.
#'
#' @return tibble::tibble()
#'
#' @example
#' spp = c("Buteo jamaicensis", "Falco peregrinus", "Aquila chrysaetos")
#' ns_ranks(spp, c("UT", "CO"))

ns_ranks <- function(species_list, states = NULL) {
  #-- Clean species list
  spp <- species_list[!is.na(species_list)] |>
    unique() |>
    trimws()
  #-- Assign states
  if (is.null(states)) states <- state.abb

  #-- Notify user of time to pull data
  message(
    paste0(
      length(spp), " unique species. Approximate time to complete: ",
      round((length(spp) * 0.557) / 60, 0), " minuntes"
    )
  )
  #-- Query NatureServe API for each species in the list
  dat <- lapply(spp, function(sp) {
    # sp = spp[3]
    #-- Courtesy count
    message(paste(match(sp, spp), " "), appendLF = FALSE)
    #-- Pull NatureServe data
    nsdat <- try(natserv::ns_search_spp(text = sp))
    #-- Validate data and munge if data are good
    if (nrow(nsdat$results) > 0) {
      #-- Find and validate indices
      # Index of input species name
      i <- which(sapply(nsdat$results$scientificName, FUN = function(x) sp %in% x))
      # Validate i
      if (length(i) > 1) i <- min(i)
      if (length(i) == 0) i <- 1
      #-- Create initial data
      dat <- tibble::tibble(dplyr::select(nsdat$results, -speciesGlobal)[i, ]) |>
        dplyr::bind_cols(nsdat$results$speciesGlobal[i, ]) |>
        dplyr::select(
          scientificName, primaryCommonName, roundedGRank, gRank,
          usesaCode, kingdom:genus
        ) |>
        dplyr::rename(
          "scientific_name" = scientificName,
          "common_name" = primaryCommonName,
          "rounded_gRank" = roundedGRank,
          "usfws_esa" = usesaCode, "class" = taxclass,
          "order" = taxorder
        ) |>
        dplyr::mutate(input_name = sp) |>
        dplyr::mutate_if(is.logical, as.character)
      #-- Find and validate indices
      # Validate S-Rank data
      if (length(nsdat$results$nations[[i]]) == 0) {
        return()
      }
      # Index of s-rank and nativity list
      y <- try(which(sapply(nsdat$results$nations[[i]]$nationCode,
        FUN = function(x) "US" %in% x
      )))
      # Validate y
      if (length(y) == 0) {
        return()
      }
      # Validate i & y
      if (length(nsdat$results$nations[[i]]$subnations[[y]]) == 0) {
        return()
      }
      #-- Pull S-ranks
      sranks <- nsdat$results$nations[[i]]$subnations[[y]] |>
        dplyr::filter(subnationCode %in% states) |>
        dplyr::mutate(state_srank = paste0(subnationCode, "_sRank")) |>
        dplyr::select(state_srank, roundedSRank) |>
        dplyr::arrange(state_srank) |>
        tidyr::pivot_wider(
          names_from = state_srank, values_from = roundedSRank,
          values_fill = NA
        )
      #-- Pull nativity
      nativity <- nsdat$results$nations[[i]]$subnations[[y]] |>
        dplyr::filter(subnationCode %in% states) |>
        dplyr::mutate(state_nat = paste0(subnationCode, "_native_NS")) |>
        dplyr::select(state_nat, native) |>
        dplyr::arrange(state_nat) |>
        tidyr::pivot_wider(
          names_from = state_nat, values_from = native,
          values_fill = FALSE
        )
      #-- Create final data frame
      dat <- dat |>
        dplyr::bind_cols(sranks) |>
        dplyr::bind_cols(nativity)
    }
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(
      scientific_name:gRank, dplyr::contains("srank"), usfws_esa,
      dplyr::contains("native"), kingdom:genus, input_name
    ) |>
    dplyr::arrange(kingdom, phylum, class, order, family, genus)
  return(dat)
  message("", appendLF = FALSE)
}
