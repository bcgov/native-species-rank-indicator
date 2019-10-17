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
library(lubridate)
library(readr)


# read in data set already formatted (1992 - 2012)

hist.data  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

hist.data <- hist.data %>%
  mutate(Scientific_Name = tolower(Scientific_Name)) %>%
  select(-Common_Name) %>%
  spread(Year, SRank)

#write.csv(hist.data, file.path("data", "hist.data.csv"), row.names = FALSE)


# read in the latest data catalogue from CDC (2012 - 2018) ------------------------------------------------------------

new.data <- file.path(("data"),
                       "BCSEE_Plants_Animals_final.csv"
)

# or
#hist.data <- file.path(
#  soe_path("Operations ORCS/Data - Working/plants_animals/trends-status-native-species/2019/historical_ranks_for_databc"),
#  "BCSEE_Plants_Animals_final.csv"
#)

new.0 <- read_csv(new.data ,
                  col_names = c("Year", "Scientific_name", "Scientific_Name_old", "Common_name",
                                "foo1", "foo2", "Element_code", "foo3", "foo4",
                                "Prov_Status", "Prov Status Review Date",
                                "Prov Status Change Date",
                                paste0("foo1", seq_len(2)), "BC List",
                                paste0("foo2", seq_len(22)), "Origin",
                                paste0("foo3", seq_len(10))))

new.0 <- new.0[-1,]

new  <- new.0 %>%
  select(c("Year", "Scientific_name", "Scientific_Name_old", "Common_name","Element_code",
           "Prov_Status", "Prov Status Review Date",
           "Prov Status Change Date","BC List","Origin")) %>%
  mutate(ELCODE = Element_code,
         `Prov Status Review Date` = year(`Prov Status Review Date`),
         `Prov Status Change Date` = year(`Prov Status Change Date`)) %>%
  mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibians",
                                  ifelse(startsWith(ELCODE,"AB"), "Breeding Birds",
                                         ifelse(startsWith(ELCODE,"AF"), "Freshwater Fish",
                                                ifelse(startsWith(ELCODE, "AM"), "Mammals",
                                                       ifelse(startsWith(ELCODE, "AR"), "Reptiles and Turtles",
                                                              ifelse(startsWith(ELCODE, "IIL"), "Lepidoptera",
                                                                     ifelse(startsWith(ELCODE, "IIO"),"Odonata",
                                                                            ifelse(startsWith(ELCODE, "IM"), "Molluscs",
                                                                                   NA))))))))) %>%

  select(-c("Element_code")) %>%
  mutate(Scientific_Name = tolower(Scientific_name))

# select groups of interest and reformat

goi <- c("Amphibians", "Breeding Birds", "Freshwater Fish", "Mammals", "Reptiles and Turtles", "NA")

# create a name.change.key
name.change.key <- new %>%
  filter(str_detect(Taxonomic_Group, paste(goi,collapse = "|"))) %>%
  select(Taxonomic_Group, Scientific_Name, Scientific_Name_old, Common_name, ELCODE) %>%
  filter(!is.na(Scientific_Name_old)) %>%
  mutate(Scientific_Name_old = tolower(Scientific_Name_old))%>%
  distinct()

name.change.sp <- c(name.change.key$Scientific_Name_old)

# format new data
new <- new %>%
  filter(str_detect(Taxonomic_Group, paste(goi,collapse = "|"))) %>%
  select(Scientific_Name, Common_name, Year, Prov_Status, ELCODE) %>%
  spread(Year, Prov_Status) %>%
  distinct()

#write.csv(name.change.key,  file.path("data", "name.key.csv"), row.names = FALSE)


# Flag the historic sci names what dont match current name
hist.data <-  hist.data %>%
  mutate(CheckSciame = ifelse(Scientific_Name %in% name.change.sp, 1, 0))


# merge hist and new data sets:
all <- left_join(new, hist.data,  by = "Scientific_Name") %>%
  select(Taxonomic_Group, Scientific_Name, ELCODE, everything()) %>%
  mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibians",
                                ifelse(startsWith(ELCODE,"AB"), "Breeding Birds",
                                       ifelse(startsWith(ELCODE,"AF"), "Freshwater Fish",
                                              ifelse(startsWith(ELCODE, "AM"), "Mammals",
                                                     ifelse(startsWith(ELCODE, "AR"), "Reptiles and Turtles",
                                                            ifelse(startsWith(ELCODE, "IIL"), "Lepidoptera",
                                                                   ifelse(startsWith(ELCODE, "IIO"),"Odonata",
                                                                          ifelse(startsWith(ELCODE, "IM"), "Molluscs",
                                                                                 NA)))))))))



head(all)

#write.csv(all, file.path("data", "test.data1.csv"), row.names = FALSE)

# get last ranked value
all <- all %>%
  group_by(Taxonomic_Group, Scientific_Name, ELCODE) %>%
  mutate(last_rank = ifelse(!is.na(`2018`), `2018`,
                            ifelse(!is.na(`2017`),`2017`,
                                   ifelse(!is.na(`2016`), `2016`,
                                          ifelse(!is.na(`2015`), `2015`,
                                                 ifelse(!is.na(`2014`),`2014`,
                                                        ifelse(!is.na(`2013`), `2013`,
                                                               ifelse(!is.na(`2011`), `2011`, NA)))))))) %>%
  select(c(Taxonomic_Group, Scientific_Name, Common_name, ELCODE, everything()) )


# flag species which 2012 date does not match between data sets.
all  <- all %>%
  mutate(Update_2012_data = ifelse(`2012.y` ==`2012.x`, 0, 1))


write.csv(all, file.path("data", "consolidated_output.csv"), row.names = FALSE)


# Check the reason for date change and compare with consolidated data  ------------------------------------

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
  select(Scientific_Name, Common_Name, everything())  %>%
  mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibians",
                                  ifelse(startsWith(ELCODE,"AB"), "Breeding Birds",
                                         ifelse(startsWith(ELCODE,"AF"), "Freshwater Fish",
                                                ifelse(startsWith(ELCODE, "AM"), "Mammals",
                                                       ifelse(startsWith(ELCODE, "AR"), "Reptiles and Turtles",
                                                              ifelse(startsWith(ELCODE, "IIL"), "Lepidoptera",
                                                                     ifelse(startsWith(ELCODE, "IIO"),"Odonata",
                                                                            ifelse(startsWith(ELCODE, "IM"), "Molluscs",
                                                                                   NA))))))))) %>%
  filter(str_detect(Taxonomic_Group, paste(goi,collapse = "|")))

sdata <- sdata %>%
    select("ELCODE","Scientific_Name","Common_Name","current_SRANK", "rank_review_date", "rank_change_date",
            "prev_SRank", "new_SRank" , "code", "reason", "comment") %>%
    mutate(`Review_Date` = year(rank_review_date),
           `Change_Date` = year(rank_change_date),
           Scientific_name = tolower(Scientific_Name))%>%
    select(-c("rank_review_date","rank_change_date")) %>%
  distinct()

# get list of current ranks
ranks.changes <- sdata %>%
  select(c(ELCODE,Scientific_Name, current_SRANK)) %>%
  distinct() %>%
  mutate(Scientific_Name = tolower(Scientific_Name)) %>%
  filter(!is.na(current_SRANK))

# check the 2018 ranks against the complete data table to highlight possible species to check

sp.checks <- all %>%
  select(ELCODE, Scientific_Name, last_rank) %>%
  distinct() %>%
  left_join(ranks.changes) %>%
  mutate(sp.to.check = ifelse(current_SRANK == last_rank, NA, TRUE)) %>%
  filter(!is.na(sp.to.check))

write.csv(sp.checks, file.path("data", "Species_to_check_manually.csv"), row.names = FALSE)


## Manually verify data

# open the  "consolidated_output.csv"
# check species within the "species to check manually.csv
# check 2012x and 2012y and where different
# check the sci names with no ELCODE.
# remove exotics
# non - breeding (b)
# check column with breeding birds (see large data )

# save as "consolidated_data_verified.csv"






# Format the years of the data for each group

vdata <- read_csv(file.path("data", "consolidated_output_verified.csv"))

vdata <- vdata %>%
  gather("Year", "SRank", 5:24) %>%
  drop_na("SRank")


get.years <- vdata %>%
  group_by(Taxonomic_Group, Year) %>%
  summarise(count = n())


# still need to format years:

# group the years per type
# Amphibian       : 1992, 1998, 2002, 2008, 2012, 2018
# breeding birds  : 1992, 1997, 2001, 2006, 2012,
# Mammals

saveRDS(vdata, file = file.path("data","indata.r"))


