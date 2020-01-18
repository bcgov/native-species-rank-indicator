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


remotes::install_github("bcgov/ranktrends")

library(ranktrends)


# or read in a local copy temporarily
indata <- readRDS(file.path("data","indata.r"))

# note this is native sp.only

status_data_wts <- indata %>%
  mutate(parsed_rank_single = ranks_to_numeric(rank, simplify = TRUE, round_fun = min),  # tried to extract 1st value ? not working
         wts = 5 - parsed_rank_single)

status_complete <- status_data_wts %>%
  group_by(taxonomic_group) %>%
  complete(nesting(scientific_name, common_name), year) %>%
  semi_join(
    group_by(., taxonomic_group, scientific_name, common_name) %>%
      summarize())

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



