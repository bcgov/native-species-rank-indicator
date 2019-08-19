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

remotes::install_github("bcgov/bcgovr")
remotes::install_github("bcgov/envreportutils")
library(bcgovr)

library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)
library(stringr)
library(purrr)


#excel_file <- file.path(
#  soe_path("Operations ORCS/Data - Working/plants_animals/trends-status-native-species/2019"),
#           "Copy of Rank_Changes_Verts_Leps_Odonates_Molluscs2.xlsx"
#  )

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
      mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibias",
                                      ifelse(startsWith(ELCODE,"AB"), "Breeding Birds",
                                             ifelse(startsWith(ELCODE,"AF"), "Freshwater Fish",
                                                    ifelse(startsWith(ELCODE, "AM"), "Mammals",
                                                           ifelse(startsWith(ELCODE, "AR"), "Reptiles and Turtles",
                                                                  ifelse(startsWith(ELCODE, "IIL"), "Lepidoptera",
                                                                         ifelse(startsWith(ELCODE, "IIO"),"Odonata",
                                                                                ifelse(startsWith(ELCODE, "IM"), "Molluscs",
                                                                                                  NA))))))))) %>%

      select(Taxonomic_Group, Scientific_Name, Common_Name, everything())


# for contracting we want to determine

# 1) number of species per taxanomic_group

data_summary <- sdata %>%
  group_by(Taxonomic_Group) %>%
  summarise(sp.no = length(unique(Scientific_Name)))

#data_summary

no_status <- sdata %>%
  group_by(Taxonomic_Group) %>%
  filter(is.na(rank_review_date)) %>%
  filter(BC_LIST == "No Status") %>%
  summarise(no.status.sp = length(unique(Scientific_Name)))

data_summary = left_join(data_summary, no_status)

# note if there is no date in the rank_change_date then only single assesment
newdata <- sdata %>%
  group_by(Taxonomic_Group) %>%
  filter(is.na(rank_change_date)) %>%
  summarise(sp.single.asses = length(unique(Scientific_Name)))

data_summary = left_join(data_summary, newdata)


## TO DO:

# notes seems like duplicates in the data set (evrything with an ID is duplicated...??)
# remove dumplcates keeping full data

length(sdata$)

tdata <- sdata %>%
  group_by(Taxonomic_Group, Scientific_Name, Common_Name,
           ELCODE, current_SRANK, rank_review_date, rank_change_date) %>%
  filter(is.na(rank_change_date)) %>%


x <-   distinct(sdata,Taxonomic_Group, Scientific_Name, Common_Name,
           ELCODE, current_SRANK, rank_review_date, rank_change_date,
           .keep_all= TRUE)

# 2) How many changed? + time stamps


newdata <- sdata %>%
  group_by(Taxonomic_Group) %>%
  filter(is.na(rank_change_date)) %>%
  summarise(sp.single.asses = length(unique(Scientific_Name)))




# 3) reason for change - when not genuine change

odata <- sdata %>%
  filter(Taxonomic_Group == "Odonata")












