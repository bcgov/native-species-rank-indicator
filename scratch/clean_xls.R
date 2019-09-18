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


# read in historic data set file and format

#ref.0  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/d3651b8c-f560-48f7-a34e-26b0afc77d84/resource/39aa3eb8-da10-49c5-8230-a3b5fd0006a9/download/bcsee_plants_animals.csv")
# or

hist.data <- file.path(
  soe_path("Operations ORCS/Data - Working/plants_animals/trends-status-native-species/2019/historical_ranks_for_databc"),
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
  select(-c("Element_code")) %>%
  filter(!str_detect(ELCODE, 'P')) %>%
  filter(!str_detect(ELCODE, "NB")) %>%
  filter(!str_detect(ELCODE, "NL"))


all.sp.list <- ref %>%
  select(c(Scientific_name, ELCODE)) %>%
  filter(!is.na(ELCODE)) %>%
  distinct()

ref <- ref %>%
  select(-ELCODE) %>%
  left_join( all.sp.list, by = "Scientific_name")


# read in the latest changes document for inverts



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
  filter(! BC_LIST == "Exotic") %>%
  filter(str_detect(ELCODE, 'I')) %>%
  filter(!str_detect(ELCODE, 'IMGAS'))


# Create data summary
data_summary <- sdata %>%
  group_by(Taxonomic_Group) %>%
  summarise(sp.no = length(unique(Scientific_Name)))

data_summary # species = 32 IMBIV (historical) bivalves

# Create a data dictionary for species details
sp.key <- sdata %>%
    select("Taxonomic_Group", "Scientific_Name", "Common_Name",
           "ELCODE", "BC_LIST")

# note this is only the species with a code change (not all species)
elcode.key <- unique(sp.key$ELCODE)

# 3) create a species change yr data set
sp.rank <- sdata %>%
    select("ELCODE", "rank_review_date", "rank_change_date",
            "prev_SRank", "new_SRank" , "code", "reason", "comment") %>%
    mutate(`Prov Status Review Date` = year(rank_review_date),
           `Prov Status Change Date` = year(rank_change_date)) %>%
    select(-c("rank_review_date","rank_change_date"))

sp.rank <- sp.rank[rowSums(is.na(sp.rank[,2:6]))!=5,]





#butterfly from lepidoptieras

# 4) merge the change data to the historic changes

xx <- left_join(sub.species , sp.rank, by = c("ELCODE","Prov Status Review Date",
                                "Prov Status Change Date"))

xx <- xx %>%
  mutate(status = ifelse(is.na(new_SRank), Prov_Status, new_SRank)) %>%
  select(-new_SRank)


# subset to molluscs
xx <- xx %>%
filter(str_detect(ELCODE, 'IMBIV'))

# assess
sp.yr.review <- xx %>%
  group_by(ELCODE) %>%
  summarise(no.record = n())

# set up empty data frame to write into
out <- data.frame(ELCODE = NA, status = NA, year = NA,
                  code = NA, reason = NA, comment = NA)

sp.list <- unique(sp.yr.review$ELCODE) # get list of species

for(i in 1:length(sp.list)) {
  i = 2
  sp.data <- xx[xx$ELCODE == sp.list[i],]
  sp.data <- sp.data %>%
    select(ELCODE,`Prov Status Review Date`,
           `Prov Status Change Date`,`Prov Status`,
           prev_SRank, status, everything())

  sp.out <- sp.data %>%
    gather("foo", "year", 2:3) %>%
    select(-foo) %>%
    distinct()

 # mutate(status = ifelse(is.na(new_SRank),
#           `Prov Status`, new_SRank))

  if(length(unique(sp.out$year == 1))){
    sp.out <- sp.out

  } else {


  }

    sp.out <- sp.out %>%
      distinct() %>%
      select(ELCODE, status, year, code, reason, comment)

  out <- bind_rows(out, sp.out)

}




# get sp with single review
single.yr <- sp.yr.review[sp.yr.review$no.record == 1, 1]
single.yr <- single.yr %>% pull()

single <- xx %>%
  filter(ELCODE %in% single.yr)

single.initial <- single %>%
  filter(code == 8 ) %>%
#  mutate(year = 'Prov Status Review', # fix this..
#         status = `Prov Status`) %>%
  select(ELCODE, status, year,  code, reason, comment)

out <- bind_rows(out, single.initial)

write.csv(single, file.path("data", "testsingle.csv"), row.names = FALSE)


write.csv(xx, file.path("data", "test12.csv"), row.names = FALSE)



# 2004 - 2010 ranks - (ANDY's formatting)
# 2011 - 2018 historic rank changes
# random scan data ? possible

# format to year species.



# minium of three rank changes

# SP , year , rank , reason # comments




# remove moths/ lepidoptera (only butterflies )

# list of the species




# set up empty data frame to write into
out <- data.frame(ELCODE = NA, `Prov Status` = NA, year = NA,
             code = NA, reason = NA, comment = NA)

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

# filter out species with a single rank change ---------
single <- ssdata %>% filter(nrows == 1)

t1 <- single %>%
  filter(is.na(change_yr)) %>%
  mutate(GP_comment = "initial review?") %>%
  select(c(Taxonomic_Group, Scientific_Name,
         Common_Name, current_SRANK,
         review_yr, code, reason, comment, GP_comment)) %>%
  rename(year = review_yr,
         srank = current_SRANK)

#write.csv(t1, file.path("data", "test1.csv"), row.names = FALSE)

t2 <- single %>%
  filter(!is.na(change_yr)) %>%
  gather("n", "year", 12:13) %>%
  select(c(Taxonomic_Group, Scientific_Name,
         Common_Name, current_SRANK,
         year, code, reason, comment)) %>%
  distinct() %>%
  rename(srank = current_SRANK) %>%
  mutate(GP_comment = "multiple yr reviews")

t.single <- bind_rows(t1, t2)
out <- bind_rows(out,t.single)

 # write.csv(t.single, file.path("data", "test4.csv"), row.names = FALSE)

# filter out species with a single rank change ---------
multiple <- ssdata %>% filter(nrows > 1)

#write.csv(multiple, file.path("data", "test2.csv"), row.names = FALSE)

#sp.list <- as.list(unique(multiple$Scientific_Name)) # for future purr function
sp.names <- unique(multiple$Scientific_Name)

#sp.names <- sp.names[1] #test set

# convert this to purrr function

for(i in sp.names){
  print(i)
 # i = sp.names[1]
  m.1 <- multiple %>%
    filter(Scientific_Name == i) %>%
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

    out.time1 <- bind_rows(time2, time1)
    m.1.out <- left_join(m.1.metadata, out.time1)

    out <- bind_rows(out, m.1.out)

  } else {
    print(i)
  }
}


write.csv(out, file.path("data", "test3.csv"), row.names = FALSE)


#if(length(m.1$Taxonomic_Group) == 1) {
#  t1 <- m.1 %>%
#    gather("n", "year", 12:13)

  }else {print "fix this still"}




write.csv(m.1, file.path("data", "test6.csv"), row.names = FALSE)


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





