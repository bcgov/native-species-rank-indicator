# Copyright 2019 Province of British Columbia
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


#remotes::install_github("bcgov/ranktrends")

library(ranktrends)
library(tidyr)
library(dplyr)

if (!exists("indata"))
indata = readRDS(file.path("data","indata.rds"))
yrs_tax = readRDS(file.path("data","yrs_tax.rds"))


indata <- indata %>%
  filter(!is.na(srank)) %>%
  select(-c(elcode, bc_list, origin))



# S?
# S1?
# S2?B
# S3?N
# S3S4N,SZN"
# S4S5B  (currently looks for ,\\s?)
# SNR
# SUB
# SA
# SU
# SNA
# S?
# SZN
#S4N,S5M"
# S5M




status_data_wts <- indata %>%
  mutate(parsed_rank_single = ranks_to_numeric(srank, simplify = TRUE, round_fun = min)) %>%
  filter(!is.na(parsed_rank_single)) %>%
  mutate(wts = 5 - parsed_rank_single)


#status_data_wts <- indata %>%
#  mutate(parsed_rank_single = ranks_to_numeric(srank, simplify = FALSE),
#        wts = 5 - parsed_rank_single)


status_complete <- status_data_wts %>%
  group_by(taxonomic_group) %>%
  complete(nesting(scientific_name, common_name), year) %>%
  semi_join(
    group_by(., taxonomic_group, scientific_name, common_name) %>%
      summarize())


# use table to filter years for taxonomic grouup
status_complete <- status_complete %>%
  left_join(yrs_tax, by = c("year", "taxonomic_group")) %>%
  filter(!is.na(count)) %>%
  select(-count)


# now throwing an error here? Dplyr ? or R 4.0?



# remove those species which are extinct
species_to_remove <- status_complete %>%
  filter(year == min(year) & rank == "SX") %>%
  pull(scientific_name) %>%
  unique()


status_data_final <- status_complete  %>%
  filter(!scientific_name %in% species_to_remove) %>%
  ungroup() %>%
  mutate(taxonomic_group = ifelse(
    taxonomic_group %in% c("Amphibians", "Reptiles and Turtles"),
    "Reptiles & Amphibians",
    taxonomic_group))

# remove NAs
status_data_final <- status_data_final %>%
  filter(!is.na(wts))

#run function
csi <- sampled_index(status_data_final, "taxonomic_group","wts","year")
csi <- csi %>% drop_na(taxonomic_group)


saveRDS(csi, file.path("data","csi_taxonomic.R"))


# Group by BC Listing  ----------------------------------------------------

status_complete <- status_data_wts %>%
  group_by(bc_list) %>%
  complete(nesting(scientific_name, common_name), year) %>%
  semi_join(
    group_by(., bc_list, scientific_name, common_name) %>%
      summarize())

# remove those species which are extinct
species_to_remove <- status_complete %>%
  filter(year == min(year) & rank == "SX") %>%
  pull(scientific_name) %>%
  unique()


status_data_final <- status_complete  %>%
  filter(!scientific_name %in% species_to_remove) %>%
  ungroup() #%>%
#  mutate(taxonomic_group = ifelse(
#    taxonomic_group %in% c("Amphibians", "Reptiles and Turtles"),
#    "Reptiles & Amphibians",
#    taxonomic_group))

# remove NAs
status_data_final <- status_data_final %>%
  filter(!is.na(wts))

#run function
csi_bc <- sampled_index(status_data_final, "bc_list","wts","year")
csi_bc <- csi_bc %>% drop_na(bc_list)


saveRDS(csi_bc, file.path("data","csi_bclist.R"))



