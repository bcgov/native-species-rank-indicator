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

#remotes::install_github("bcgov/bcgovr")
#remotes::install_github("bcgov/envreportutils")

library(bcgovr)
library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)
library(stringr)
library(purrr)
library(lubridate)
library(gtools)
library(readr)

source("R/lookup_elcode.R")


# Create tables to populate

#ref.0  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/d3651b8c-f560-48f7-a34e-26b0afc77d84/resource/39aa3eb8-da10-49c5-8230-a3b5fd0006a9/download/bcsee_plants_animals.csv")
# or

# hist.data <- file.path(
#  soe_path("Operations ORCS/Data - Working/plants_animals/trends-status-native-species/2019/historical_ranks_for_databc"),
#  "BCSEE_Plants_Animals_final.csv"
# )

hist.data <- file.path(("data"),
  "BCSEE_Plants_Animals_final.csv"
)

ref.0 <- read_csv(hist.data,
                  col_types = cols_only(
                    Year = col_integer(),
                    `Scientific Name` = col_character(),
                    `English Name` = col_character(),
                    `Element Code` = col_character(),
                    `Prov Status` = col_character(),
                    `Prov Status Review Date` = col_character(),
                    `Prov Status Change Date` = col_character(),
                    `Name Category` = col_character()
                  )
) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x))) %>%
  filter(!name_category %in% c("Vascular Plant", "Non-Vascular Plant",
                              "Nonvascular Plant", "Fungus",
                              "International Vegetation Classification")) %>%
  rename(ELCODE = element_code,
         common_name = english_name) %>%
  filter(!grepl("^(Search|Sort|Open|Animals)", scientific_name),
         !is.na(scientific_name)) %>%
  mutate(scientific_name = tolower(trimws(scientific_name, "both")))

if (!file.exists("data/tax_key_full.csv")) {

  # create a key with all historic ELcode, names, scinema , Taxanomic
  full_key <- ref.0 %>%
    select(scientific_name) %>%
    distinct() %>%
    left_join(ref.0 %>%
                select(scientific_name, ELCODE, year) %>%
                filter(!is.na(ELCODE)) %>%
                distinct())

  sum(is.na(full_key$ELCODE))

  full_key$ELCODE[is.na(full_key$ELCODE)] <- lookup_elcodes(full_key$scientific_name[is.na(full_key$ELCODE)])

  sum(is.na(full_key$ELCODE))

  full_key <- full_key %>% mutate(taxonomic_group = case_when(
    # startsWith(ELCODE, "AA")  ~ "Amphibians",
    # startsWith(ELCODE, "AB")  ~ "Breeding Birds",
    # startsWith(ELCODE, "AF")  ~ "Freshwater Fish",
    # startsWith(ELCODE, "AM")  ~ "Mammals",
    # startsWith(ELCODE, "AR")  ~ "Reptiles and Turtles",
    startsWith(ELCODE, "IILEP") ~ "Lepidoptera",
    startsWith(ELCODE, "IIODO") ~ "Odonata",
    startsWith(ELCODE, "IMBIV") ~ "Molluscs",
    TRUE ~ NA_character_)) %>%
    filter(!is.na(taxonomic_group))

  key <- full_key %>%
    select(scientific_name, ELCODE, taxonomic_group) %>%
    distinct() %>%
    mutate(scientific_name = tolower(scientific_name))

  # Create a key with just one row for each ELCODE to use for authoritative names
  latest_key <- group_by(full_key, ELCODE, taxonomic_group) %>%
    filter(year == max(year)) %>%
    select(-year)

  write_csv(key, "data/tax_key_full.csv")
  write_csv(latest_key, "data/tax_key_latest.csv")
} else {
  key <- read_csv("data/tax_key_full.csv")
  latest_key <- read_csv("data/tax_key_latest.csv")
}

ref <- ref.0 %>%
  select(year, scientific_name, common_name,
           prov_status, prov_status_review_date,
           prov_status_change_date) %>%
  mutate(scientific_name = tolower(scientific_name)) %>%
  left_join(key, by = "scientific_name") %>% # get ELCODES where there weren't
  # replace scientific name with latest
  left_join(latest_key, by = "ELCODE",suffix = c(".old", "")) %>%
  mutate(scientific_name = case_when(
    is.na(scientific_name) ~ scientific_name.old,
    TRUE ~ scientific_name
  )) %>%
  select(-scientific_name.old)

# Run through each group and add pre2004 to historic data sets.

# function to add rank columns per
col.names.fn <- function(x) {
  adjname <- paste0(x,"_Adj_SRank")
  adjcode <- paste0(x,"_Adj_SRank_Code")
  adjcomm <- paste0(x,"_Adj_SRank_Comment")
  new.col.names <- c(adjname, adjcode, adjcomm)
  new.col.names
}

group.oi <- c("IIODO" = "Odonata", "IILEP" = "Lepidoptera", "IMBIV" = "Molluscs")

output_dirs <- file.path("data", "Inverts", "Contractor_datasets", group.oi)
lapply(output_dirs, dir.create, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(group.oi)) {
  elcode_abbrev <- names(group.oi)[i]
  group <- group.oi[elcode_abbrev]
  gref <- ref %>%
    filter(taxonomic_group ==  group) %>%
    select(scientific_name, year, prov_status) %>%
    spread(year, prov_status) %>%
    mutate(scientific_name = tolower(scientific_name)) %>%
    distinct()

  # read in the per 2004 data
  pre2004 <- read_excel(file.path("data",
                                  paste(group ,"_pre2004.xlsx",sep = "")),
                        na = c("", "n/a")) %>%
    mutate(scientific_name = tolower(Scientific_name)) %>%
    select(-Scientific_name)

  if (!"ELCODE" %in% names(pre2004)){
    pre2004 <- pre2004 %>%
      left_join(select(key, scientific_name, ELCODE), by = "scientific_name")
  }

  pre2004$ELCODE[is.na(pre2004$ELCODE)] <- lookup_elcodes(pre2004$scientific_name[is.na(pre2004$ELCODE)])

  pre2004 <- pre2004 %>%
    filter(is.na(ELCODE) | grepl(elcode_abbrev, ELCODE)) %>%
    # replace scientific name with latest
    left_join(latest_key, by = "ELCODE",suffix = c(".old", "")) %>%
    mutate(scientific_name = case_when(
      is.na(scientific_name) ~ scientific_name.old,
      TRUE ~ scientific_name
    )) %>%
    select(-scientific_name.old)


  data_all <- full_join(gref, pre2004, by = "ELCODE", suffix = c("", ".1")) %>%
    mutate(
      scientific_name = ifelse(is.na(scientific_name),
                                 scientific_name.1,
                                 scientific_name)
    ) %>%
    select(-scientific_name.1) %>%
    group_by(scientific_name, ELCODE) %>%
    summarise_all(max, na.rm = TRUE)

  # get the years of interest
  oyears1 <- names(data_all)[str_detect(names(data_all), c("1"))]
  oyears2 <- names(data_all)[str_detect(names(data_all), c("2"))]
  oyears <- unique(c(oyears1, oyears2))
  oyears <- as.list(as.character(sort(as.numeric(oyears))))

  # create the new columns using fnc and add to data frame
  new.cols.to.add <- as.vector(unlist(map(oyears, col.names.fn)))

  data_all[, new.cols.to.add] <- ""

  groups.to.keep <- c("IIODO", "ILEP", "IMBIV")

  data_all <- data_all %>%
    filter(str_detect(ELCODE, paste(groups.to.keep , collapse = "|"))) %>%
    select(sort(names(data_all))) %>%
    select(ELCODE, scientific_name, everything())

  write_csv(data_all, file.path(output_dirs[i], paste0(group, "_deliverable.csv",sep = "")))

}


# create a data set for each group with change reasons

# or read Git local version
excel_file <- file.path("data",
                        "Copy of Rank_Changes_Verts_Leps_Odonates_Molluscs2.xlsx")

sdata <- read_excel(excel_file, sheet = "Query Output",
                    range = "A2:O2731",
                    col_types = c("numeric", "text", "numeric",
                                  rep("text", 4), rep("date",3),
                                  rep("text", 2), "numeric",
                                  rep("text", 2)),
                    col_names = c("ID", "ELCODE", "EST_ID",
                                  "Scientific_Name", "Common_Name",
                                  "current_SRANK", "BC_LIST",
                                  "rank_review_date", "rank_change_date",
                                  "change_entry_date", "prev_SRank",
                                  "new_SRank", "code", "reason", "comment"))  %>%
  rename_at(setdiff(names(.), c("BC_LIST", "ELCODE")), tolower) %>%
  select(scientific_name, common_name, everything()) %>%
  filter(!BC_LIST == "Exotic") %>%
  filter(str_detect(ELCODE, 'I')) %>%
  filter(!str_detect(ELCODE, 'IMGAS'))

#generate Lep output
leps <- sdata %>%
  filter(str_detect(ELCODE, 'ILEP'))

write.csv(leps, file.path("data", "Inverts", "Contractor_datasets","Lepidoptera",
                          "Lepidoptera_Rank_change_reason.csv"))
#generate odo output
odo <- sdata %>%
  filter(str_detect(ELCODE, 'IIODO'))

write.csv(odo, file.path("data", "Inverts", "Contractor_datasets", "Odonata",
                          "Odonata_rank_change_reason.csv"))

#generate odo output
mol <- sdata %>%
  filter(str_detect(ELCODE, 'IMBIV'))

write.csv(mol, file.path("data", "Inverts", "Contractor_datasets","Molluscs",
                         "Molluscs_rank_change_reason.csv"))
