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

# convert to long format

library(tidyr)

inverts <- gather(invert_final, key = year, value = SRank,
                  -ELCODE, -scientific_name, -common_name, -taxonomic_group) %>%
rename_all(.funs = tolower)


write.csv(inverts, file.path("data", "inverts_consolidated.csv"), row.names = FALSE)

