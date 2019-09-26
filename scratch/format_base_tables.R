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


# Create tables to populate

#ref.0  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/d3651b8c-f560-48f7-a34e-26b0afc77d84/resource/39aa3eb8-da10-49c5-8230-a3b5fd0006a9/download/bcsee_plants_animals.csv")
# or

#hist.data <- file.path(
#  soe_path("Operations ORCS/Data - Working/plants_animals/trends-status-native-species/2019/historical_ranks_for_databc"),
#  "BCSEE_Plants_Animals_final.csv"
#)

hist.data <- file.path(("data"),
  "BCSEE_Plants_Animals_final.csv"
)

ref.0 <- read_csv(hist.data ,
                  col_names = c("Year", "Scientific_name", "foo", "Common_name",
                                "foo1", "foo2", "Element_code", "foo3", "foo4",
                                "Prov_Status", "Prov Status Review Date",
                                "Prov Status Change Date",
                                paste0("foo1", seq_len(48 - 12))))
ref.0 <- ref.0[-1,]

ref <- ref.0 %>%
  select(c("Year", "Scientific_name", "Common_name","Element_code",
           "Prov_Status", "Prov Status Review Date",
           "Prov Status Change Date")) %>%
  mutate(ELCODE = Element_code,
         `Prov Status Review Date` = year(`Prov Status Review Date`),
         `Prov Status Change Date` = year(`Prov Status Change Date`)) %>%
  mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibias",
                                  ifelse(startsWith(ELCODE,"AB"), "Breeding Birds",
                                         ifelse(startsWith(ELCODE,"AF"), "Freshwater Fish",
                                                ifelse(startsWith(ELCODE, "AM"), "Mammals",
                                                       ifelse(startsWith(ELCODE, "AR"), "Reptiles and Turtles",
                                                              ifelse(startsWith(ELCODE, "IIL"), "Lepidoptera",
                                                                     ifelse(startsWith(ELCODE, "IIO"),"Odonata",
                                                                            ifelse(startsWith(ELCODE, "IM"), "Molluscs",
                                                                                   NA))))))))) %>%

  select(-c("Element_code"))

# Run through each group and add pre2004 to historic data sets.

# function to add rank columns per
col.names.fn <- function(x) {
  adjname <- paste0(x,"_Adj_SRank")
  adjcode <- paste0(x,"_Adj_SRank_Code")
  adjcomm <- paste0(x,"_Adj_SRank_Comment")
  new.col.names <- c(adjname, adjcode, adjcomm)
  new.col.names
}

group.oi <- c("Odonata", "Lepidoptera", "Molluscs")

for(i in group.oi) {

  gref <- ref %>%
    filter(Taxonomic_Group ==  i) %>%
    select(ELCODE, Scientific_name, Year, Prov_Status) %>%
    spread(Year, Prov_Status) %>%
    mutate(Scientific_name = tolower(Scientific_name)) %>%
    distinct()

  # read in the per 2004 data
  pre2004 <- read_excel(file.path("data", "Compiled_pre2004",
                                  paste(i ,"_pre2004.xlsx",sep = "")))
  pre2004 <- pre2004 %>%
    mutate(Scientific_name = tolower(Scientific_name))

  data_all <- full_join(gref, pre2004)

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
    select(ELCODE, Scientific_name, everything())

  write.csv(data_all, file.path("data",paste0(i, "_deliverable.csv",sep = "")), row.names = FALSE)

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
  select(Scientific_Name, Common_Name, everything()) %>%
  filter(! BC_LIST == "Exotic") %>%
  filter(str_detect(ELCODE, 'I')) %>%
  filter(!str_detect(ELCODE, 'IMGAS'))

leps <- sdata %>%
  filter(str_detect(ELCODE, 'IMBIV'))

write.csv(leps, file.path("data", "Contractor_datasets", "Lepidoptera",
                          "Lepidoptera_Rank_change_reason.csv"))

odo <- sdata

  gref <- ref %>%
    filter(Taxonomic_Group ==  i) %>%
    select(ELCODE, Scientific_name, Year, Prov_Status) %>%
    spread(Year, Prov_Status) %>%
    mutate(Scientific_name = tolower(Scientific_name)) %>%
    distinct()

