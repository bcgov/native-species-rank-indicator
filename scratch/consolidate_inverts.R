# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# import and consolidate invertebrte data:

library(tidyr)
library(readxl)
library(dplyr)
library(stringr)

source("R/lookup_elcode.R")


# read in data files
invert.files <- as.list(list.files(file.path("data", "Inverts", "Completed"),
                                   pattern = "_final.xlsx", full.names = TRUE))

inverts_all <- lapply(invert.files, function(file){

  idata <- read_xlsx(file, na = "NA")

  yrs <- as.list(c('1995', '1999', '2000', '2001', '2004', '2005', '2006', '2007', '2008',
                   '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016',
                   '2017', '2018'))

  # check both old and adjusted column and get adjusted if updated.

  longout <- lapply(yrs, function(y){
    yr = y
    yr_adj = paste0(yr, "_Adj_SRank")

    if(yr %in% names(idata)) {

    out <- idata %>%
      mutate(final = ifelse(is.na( !!sym(yr_adj)), !!sym(yr),!!sym(yr_adj))) %>%
      dplyr::select(final)

    names(out) = yr
    out
    }
  })


  out <- do.call("cbind", plyr::compact(longout))


  out_full <- idata %>%
    dplyr::select( ELCODE, scientific_name, common_name, taxonomic_group) %>%
    bind_cols(out)

})


invert_final <- do.call("bind_rows", inverts_all)


# check names with the provincial list

prov_list <- read_csv(file.path("data","raw",
                                "BCSEE_Plants_Animals_final.csv"),
                      col_types = cols_only(
                        Year = col_integer(),
                        `Scientific Name` = col_character(),
                        `English Name` = col_character(),
                        `Element Code` = col_character(),
                        `Name Category` = col_character(),
                        `BC List` = col_character(),
                        `Origin` = col_character()
                      )
) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x))) %>%
  filter(!name_category %in% c("Vascular Plant", "Non-Vascular Plant",
                               "Nonvascular Plant", "Fungus",
                               "International Vegetation Classification")) %>%
  mutate(scientific_name = tolower(scientific_name)) %>%
  group_by(scientific_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(-c(year, english_name, name_category))

# add the provinical listing information

indata <- invert_final %>%
  left_join(prov_list, by = "scientific_name")



      # check species with non-matching provincial listings
      # export a list for CDC to check:

      no.list = indata %>%
        filter(is.na(bc_list))

      leps <- read_xlsx(invert.files[[1]], na = "NA") %>%
        select(ELCODE, scientific_name,common_name, other_scientifi_names,
                Comments) %>%
        filter(scientific_name %in% no.list$scientific_name)

      mols <- read_xlsx(invert.files[[2]], na = "NA") %>%
        select(ELCODE, scientific_name,common_name, scientific_name_long,
               Comments) %>%
        rename(other_scientifi_names = scientific_name_long) %>%
        filter(scientific_name %in% no.list$scientific_name)

      comments = bind_rows(leps, mols)

      no.list.coms <- no.list %>%
        left_join(comments, by = c("scientific_name", "common_name", "ELCODE")) %>%
        select(-"element_code") %>%
        select(ELCODE, scientific_name, other_scientifi_names, common_name,
               taxonomic_group, Comments, everything())

      # export data for lea to check
      write.csv(no.list.coms , file.path("data", "raw","manual_checks","inverts_tax_check.csv"), row.names = FALSE)





## Still to do !!!



    # fix individual species with invalide taxonomy - waiting on CDC

    #invert_to_remove <- c("clossiana titania", "agriades rusticus")



indata


# convert to long format for export to BC data catalogue
indata <- indata %>%
  select(-c("element_code", english_name))

# comvert to long
inverts <-pivot_longer(indata, cols = -c(taxonomic_group, scientific_name, common_name, bc_list, origin, ELCODE),
               names_to = "year", values_to = "srank") %>%
  rename_all(.funs = tolower) %>%
  select(elcode, taxonomic_group, scientific_name, common_name,
         bc_list, origin, year, srank)


write.csv(inverts, file.path("data", "inverts_retroranks.csv"), row.names = FALSE)

