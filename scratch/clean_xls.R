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

      select(Taxonomic_Group, Scientific_Name, Common_Name, everything()) %>%
      filter(! BC_LIST == "Exotic") #%>%

# Calculate number of species per group -------------

# 1) number of native species per taxanomic_group

data_summary <- sdata %>%
  group_by(Taxonomic_Group) %>%
  summarise(sp.no = length(unique(Scientific_Name)))

no_status <- sdata %>%
  group_by(Taxonomic_Group) %>%
  filter(is.na(rank_review_date)) %>%
  filter(BC_LIST == "No Status") %>%
  summarise(no.status.sp = length(unique(Scientific_Name)))

data_summary = left_join(data_summary, no_status)

data_summary


# Reformat data table to match current data input ----------

ssdata <- sdata %>%
  mutate(review_yr = year(rank_review_date),
         change_yr = year(rank_change_date),
         ch_entry_yr = year(change_entry_date)) %>%
  select(Taxonomic_Group, Scientific_Name, Common_Name, ELCODE,
         current_SRANK, BC_LIST, prev_SRank, new_SRank, code,
         reason, comment, review_yr, change_yr, ch_entry_yr)


# test data set with molluscs
ssdata <- ssdata %>%
  filter(Taxonomic_Group == "Molluscs")
# filter(Taxonomic_Group == "Odonata")

# split data by number of rank changes (ie: single or multiple)
ssdata.sum <- ssdata %>%
  group_by(Scientific_Name)%>%
  summarise(nrows = n())

ssdata <- ssdata %>%
  left_join(ssdata.sum)

# filter out species with only one entry line

single <- ssdata %>% filter(nrows == 1)

#single review
t1 <- single %>%
  filter(is.na(change_yr)) %>%
  mutate(GP_comment = "initial review?") %>%
  select(c(Taxonomic_Group, Scientific_Name,
         Common_Name, current_SRANK,
         review_yr, code, reason, comment, GP_comment)) %>%
  rename(year = review_yr,
         srank = current_SRANK)

#write.csv(t2, file.path("data", "test5.csv"), row.names = FALSE)

t2 <- single %>%
  filter(!is.na(change_yr)) %>%
  gather("n", "year", 12:13) %>%
  select(c(Taxonomic_Group, Scientific_Name,
         Common_Name, current_SRANK,
         year, code, reason, comment)) %>%
  distinct() %>%
  rename(srank = current_SRANK) %>%
  mutate(GP_comment = "multiple yr reviews")

t.single <- rbind(t1, t2)

write.csv(t.out, file.path("data", "test4.csv"), row.names = FALSE)

#multiple review
multiple <- ssdata %>% filter(nrows > 1)

write.csv(multiple, file.path("data", "test4.csv"), row.names = FALSE)

sp.list <- as.list(unique(multiple$Scientific_Name))
sp.list[[1]] # convert this to purrr function

m.1 <- multiple %>%
  filter(Scientific_Name == sp.list[[1]]) %>%
  filter(!is.na(prev_SRank)|!is.na(code))

m.1.metadata <- m.1 %>%
  select(Taxonomic_Group, Scientific_Name, Common_Name)

if(length(m.1$Taxonomic_Group) == 1) {
  time1 <- m.1 %>%
    select(Scientific_Name, prev_SRank) %>%
    rename(srank = prev_SRank) %>%
    mutate(year = 9999)

  time2 <- m.1 %>%
    select(Scientific_Name, current_SRANK, code,
         reason, comment, change_yr) %>%
    rename(srank = current_SRANK,
         year = change_yr)

library(gtools)
out <- smartbind(time2, time1)
m.1.out <- left_join(m.1.metadata, out)

if(current)

# if current_SRank == new_SRank


#if(length(m.1$Taxonomic_Group) == 1) {
#  t1 <- m.1 %>%
#    gather("n", "year", 12:13)

  }else {print "fix this still"}




write.csv(m.1, file.path("data", "test6.csv"), row.names = FALSE)




%>%
  mutate(GP_comment = "initial review?") %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name,ELCODE, current_SRANK, BC_LIST,
           review_yr, code, reason, comment, GP_comment)) %>%
  rename(year = review_yr)



write.csv(t1, file.path("data", "test4.csv"), row.names = FALSE)



# filter the duplicates with NA but keep real NA's # not working
#dup <- ssdata %>%
# group_by(Taxonomic_Group, Scientific_Name, prev_SRank) %>%
#  summarise(count = length(prev_SRank))
#write.csv(dup, file.path("data", "testd.csv"))

multi.ssdata <- ssdata %>%
  mutate(yr_change = ifelse(current_SRANK == new_SRank,"YES","NO"))


mdata1 <- multi.ssdata %>%
  filter(yr_change == "YES") %>%
  select(c(Taxonomic_Group, Scientific_Name,
                   Common_Name,ELCODE, current_SRANK, BC_LIST,
                   ch_entry_yr, code, reason, comment))

mdata2 <- multi.ssdata %>%
  filter(yr_change == "NO") %>%
  select(c(Taxonomic_Group, Scientific_Name,
           Common_Name,ELCODE, current_SRANK, BC_LIST,
           ch_entry_yr, code, reason, comment))




# Note: seems like there is duplicates in the data set (evrything with an ID is duplicated...??)




# Attempt 2;


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





