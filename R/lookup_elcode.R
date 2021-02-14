lookup_elcode <- function(sciname, ...) {
  if (!requireNamespace("natserv")) {
    stop("Package 'natserv' required")
  }

  name_search_results <- try(
    natserv::ns_search_spp(
      text_adv = list(searchToken = sciname,
                      matchAgainst = "allScientificNames",
                      operator = "equals"),

      location = list(nation = "CA", subnation = "BC"),
      ...)
  )

  if (inherits(name_search_results, "try-error")) {
    warning("No results found for ", sciname)
    return(NA_character_)
  }

  summ <- name_search_results$resultsSummary
  res <- name_search_results$results

  n_match <- summ[summ$name == "species_total", "value"][[1]]

  if (n_match == 0) {
    warning("No results found for ", sciname)
    return(NA_character_)
  }

  if (n_match > 1) {

    res <- res[
      simpl(res$scientificName) == simpl(sciname) |
        vapply(res$speciesGlobal$synonyms, function(x) {
          any(simpl(sciname) %in% simpl(x))
        }, FUN.VALUE = logical(1)),
    ]

    if (nrow(res) > 1L) {
      warning("More than one result for ", sciname)
      return(NA_character_)
    }

    if (nrow(res) == 0) {
      warning("No results found for ", sciname)
      return(NA_character_)
    }
  }

  res$elcode

}

lookup_elcodes <- function(scinames, ...) {
  vapply(scinames, lookup_elcode, FUN.VALUE = character(1),
        ...)
}

simpl <- function(x) tolower(gsub("\\s+", "", x))
