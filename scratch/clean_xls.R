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
library(lubridate)


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


# To estimate contracting details

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

<<<<<<< HEAD
data_summary = left_join(data_summary, no_status)

=======
>>>>>>> ade8be74d1078fa901c3cf0826face7c60387aec
# note if there is no date in the rank_change_date then only single assesment
newdata <- sdata %>%
  group_by(Taxonomic_Group) %>%
  filter(is.na(rank_change_date)) %>%
  summarise(sp.single.asses = length(unique(Scientific_Name)))

data_summary = left_join(data_summary, newdata)




## In detail per taxonomic group

# seems like there is duplicates in the data set (evrything with an ID is duplicated...??)

# this code works for odonates, but needs some tweaks for to run for all tax groups



odata <- sdata %>%
  filter(Taxonomic_Group == "Odonata")

# filter those with only a single review
single.assess <- odata %>%
  filter(is.na(rank_change_date)) %>%
  mutate(year = year(rank_review_date),
         comment = "initial assesment") %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name, current_SRANK,
           year, reason, comment))

# filter and format data with two or more assessments
twice.assess <- odata %>%
  filter(!is.na(rank_change_date)) %>%      # remove sp with only a single review
  distinct(Taxonomic_Group, Scientific_Name, Common_Name,
           ELCODE, current_SRANK, rank_review_date, rank_change_date,
           .keep_all= TRUE)   %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name, current_SRANK,
           rank_review_date,rank_change_date,
           change_entry_date, prev_SRank, new_SRank,
           code, reason))

# convert data with twice assessment and no change
twice.assess.no.change <- twice.assess %>%
  filter(is.na(change_entry_date)) %>%
  mutate(year1 = year(rank_review_date),
         year2 = year(rank_change_date),
         comment = "no change") %>%
  gather("n", "year", 12:13) %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name, current_SRANK,
           year, reason, comment))

# convert data twice assess with change
# extract start ranking ~ unknown date
twice.assess.with.change.start <- twice.assess %>%
  filter(!is.na(change_entry_date)) %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name, prev_SRank)) %>%
  mutate(year = 9999,
         current_SRANK = prev_SRank,
         comment = "unknown start date") %>%
  select(-prev_SRank)

# extract current ranking ~ known date and reason
twice.assess.with.change.end <- twice.assess %>%
  filter(!is.na(change_entry_date)) %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name, change_entry_date,
           current_SRANK, code, reason)) %>%
  mutate(year = year(change_entry_date)) %>%
  select(-change_entry_date)


data_sum <- bind_rows(single.assess,
                      twice.assess.no.change,
                      twice.assess.with.change.start,
                      twice.assess.with.change.end)

data_sum

#write.csv(data_sum, file.path("data", "odo_test.csv"))
write.csv(data_sum, file.path("data", "allgroups_test.csv"))


# 2) How many changed? + time stamp


# 3) reason for change - when not genuine change
cdata <- data_sum %>%
  group_by( Taxonomic_Group, reason, comment) %>%
  summarise(count = n())













