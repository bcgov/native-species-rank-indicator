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


# remotes::install_github("bcgov/ranktrends")

library(readr)
library(dplyr)
library(ranktrends)

# read in data set from data catalogue
#ref.0  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/bc-vertebrates-conservation-status-rank-history-1992-2012/resource/842bcf0f-acd2-4587-a6df-843ba33ec271https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")


# or read in a local copy temporarily
indata <- readRDS(file.path("data","indata.r"))

indata <- read_csv(file.path("data", "inverts_consolidated.csv"))



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


# add provincial listing

indata <- indata %>%
  left_join(prov_list, by = "scientific_name")

no.list = indata %>%
  filter(is.na(bc_list))




# check the number of species by types:
sp.type <- indata %>%
  group_by(origin) %>%
  summarise(count = n())


# check the number of species by BC List
sp.catergory <- indata %>%
  group_by(`bc_list`) %>%
  summarise(count = n())


# check the number of species by BC List
sp.catergory <- indata %>%
  group_by(origin, bc_list) %>%
  summarise(count = n())


# remove the exotics
indata <- indata %>%
  filter(!origin %in% c("Exotic", "Unknown/Undetermined"))

saveRDS(indata, file.path("data","indata.R"))
