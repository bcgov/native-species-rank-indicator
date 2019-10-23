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

new.1  <- new.0 %>%
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
name.change.key <- new.1 %>%
  filter(str_detect(Taxonomic_Group, paste(goi,collapse = "|"))) %>%
  select(Taxonomic_Group, Scientific_Name, Scientific_Name_old, Common_name, ELCODE) %>%
  filter(!is.na(Scientific_Name_old)) %>%
  mutate(Scientific_Name_old = tolower(Scientific_Name_old))%>%
  distinct()

name.change.sp <- c(name.change.key$Scientific_Name_old)

# format new data
new <- new.1 %>%
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

all <- all %>%
  select(Taxonomic_Group, Scientific_Name, Common_name, ELCODE,"1992","1995", "1997",
          "1998" , "2001","2002", "2003", "2005", "2006" ,"2007" , "2008" , "2010",
         "2011", "2012.x" ,"2012.y", "2013" , "2014", "2015" , "2016", "2017" , "2018",
         "CheckSciame" , "Update_2012_data" )

#
#write.csv(all, file.path("data", "consolidated_output.csv"), row.names = FALSE)


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

#write.csv(sp.checks, file.path("data", "Species_to_check_manually.csv"), row.names = FALSE)


## Manually verify data

# Currently half way through vetting the consolidated_output_edit.csv
# I am using the Species_manually_adjusted.csv to keep track of manual changes
# mostly name changes and duplicates in common name subspecies


# still to do:
# - finish editing the consolidates_output_edit.csv and track changes
# - check 2012x and 2012y and where different
# check the sci names with no ELCODE.
# remove exotics
# non - breeding (b)
# check column with breeding birds (see large data )

# save as "consolidated_data_verified.csv"



# Format the years of the data for each group

vdata <- read_csv(file.path("data", "consolidated_output_edit.csv"))

sp.check <-  vdata %>%
  group_by(ELCODE) %>%
  summarise(count = n()) %>%
  filter(count > 1)

sp.check

# add the bc origin and BC list to the data set

new.1  <- new.0 %>%
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
  mutate(Scientific_Name = tolower(Scientific_name)) %>%
  filter(Year == 2018)

origin <- new.1 %>%
  select(Scientific_name, Origin, `BC List`) %>%
  mutate(Scientific_Name = tolower(Scientific_name),
         `BC List` = tolower(`BC List`)) %>%
  filter(!is.na(Origin)) %>%
  distinct()

# set up years of assessment : still to do :
indata <- left_join(vdata, origin) %>%
  select(-c(CheckSciame, Update_2012_data, Scientific_name))

indata <- indata %>%
  gather("Year", "SRank", 5:25) %>%
  drop_na("SRank")


x <- indata %>%
  group_by(Taxonomic_Group, Year) %>%
  summarise(count = n())


am <- c(1992,1998, 2002, 2010, 2016, 2018)
bb <- c(1992, 1997, 2001, 2006, 2009, 2012, 2015, 2018)
ff <- c(1992, 1998, 2001, 2005, 2010, 2012, 2018, 2019)
ma <- c(1992, 1995, 2001, 2003, 2006, 2007, 2011, 2015, 20017, 2018)
rt <- c(1992, 1998, 2002, 2008, 2012, 2018)



xx <- indata %>%
  mutate(keep = ifelse(Taxonomic_Group == 'Amphibians' & Year %in% am, T,
                       ifelse(Taxonomic_Group == "Breeding Birds" & Year %in% bb, T,
                              ifelse(Taxonomic_Group == "Freshwater Fish"  & Year %in% ff, T,
                                     ifelse(Taxonomic_Group == "Mammals"  & Year %in% ma, T,
                                            ifelse(Taxonomic_Group == "Reptiles and Turtles"  & Year %in% rt, T,F))))))

#length(xx$Taxonomic_Group)
indata <- xx %>%
  filter(keep == TRUE) %>%
  select(-(keep))

length(indata$Taxonomic_Group)


#write.csv(indata, file.path("data", "indata.csv"), row.names = FALSE)

saveRDS(indata, file.path("data","indata.R"))


