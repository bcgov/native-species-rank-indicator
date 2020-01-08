lookup_elcode <- function(sciname, key = NULL, ...) {
  if (!requireNamespace("natserv")) {
    stop("Package 'natserv' required")
  }
  name_search_results <- try(natserv::ns_search(sciname, key = key, ...))

  if (inherits(name_search_results, "try-error")) {
    warning("No results found for ", sciname)
    return(NA_character_)
  }

  global_id <- unique(name_search_results$globalSpeciesUid)

  if (length(global_id) > 1) {
    warning("More than one result for ", sciname)
    return(NA_character_)
  }

  spp_data <- try(natserv::ns_data(global_id, key = key, ...))

  if (inherits(spp_data, "try-error")) {
    warning("An error occurred getting data for ", sciname)
    return(NA_character_)
  }

  spp_data[[1]]$speciesCode
}

lookup_elcodes <- function(scinames, key = NULL, ...) {
  vapply(scinames, lookup_elcode, FUN.VALUE = character(1),
         key = key, ...)
}
