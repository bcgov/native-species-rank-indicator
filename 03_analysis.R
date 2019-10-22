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


# or read in a local copy temporarily
indata <- readRDS(file.path("data","indata.r"))

# note this is native sp.only

status_data_wts <- indata %>%
  mutate(parsed_rank_single = ranks_to_numeric(SRank, simplify = TRUE, round_fun = min),  # tried to extract 1st value ? not working
         wts = 5 - parsed_rank_single)

status_complete <- status_data_wts %>%
  group_by(Taxonomic_Group) %>%
  complete(nesting(Scientific_Name, Common_name), Year) %>%
  semi_join(
    group_by(., Taxonomic_Group, Scientific_Name, Common_name) %>%
      summarize())

# remove those species which are extinct
species_to_remove <- status_complete %>%
  filter(Year == min(Year) & SRank == "SX") %>%
  pull(Scientific_Name) %>%
  unique()


status_data_final <- status_complete  %>%
  filter(!Scientific_Name %in% species_to_remove) %>%
  ungroup() %>%
  mutate(Taxonomic_Group = ifelse(
    Taxonomic_Group %in% c("Amphibians", "Reptiles and Turtles"),
    "Reptiles & Amphibians",
    Taxonomic_Group))

# remove NAs
status_data_final <- status_data_final %>%
  filter(!is.na(wts))

#run function
csi <- sampled_index(status_data_final, "Taxonomic_Group","wts","Year")
csi <- csi %>% drop_na(Taxonomic_Group)


saveRDS(csi, file.path("data","csi_taxonomic.R"))


# Group by BC Listing  ----------------------------------------------------

status_complete <- status_data_wts %>%
  group_by(`BC List`) %>%
  complete(nesting(Scientific_Name, Common_name), Year) %>%
  semi_join(
    group_by(., `BC List`, Scientific_Name, Common_name) %>%
      summarize())

# remove those species which are extinct
species_to_remove <- status_complete %>%
  filter(Year == min(Year) & SRank == "SX") %>%
  pull(Scientific_Name) %>%
  unique()


status_data_final <- status_complete  %>%
  filter(!Scientific_Name %in% species_to_remove) %>%
  ungroup() #%>%
#  mutate(Taxonomic_Group = ifelse(
#    Taxonomic_Group %in% c("Amphibians", "Reptiles and Turtles"),
#    "Reptiles & Amphibians",
#    Taxonomic_Group))

# remove NAs
status_data_final <- status_data_final %>%
  filter(!is.na(wts))

#run function
csi_bc <- sampled_index(status_data_final, "`BC List`","wts","Year")
csi_bc <- csi_bc %>% drop_na(`BC List`)


saveRDS(csi_bc, file.path("data","csi_bclist.R"))



