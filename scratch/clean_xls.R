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

library(bcgovr)
library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)
library(stringr)
library(lubridate)
library(readr)

source("R/lookup_elcode.R")

# read in data set already formatted (1992 - 2012)

hist.data  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

hist.data <- hist.data %>%
  mutate(Scientific_Name = tolower(Scientific_Name)) %>%
  select(-Common_Name) %>%
  spread(Year, SRank) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x)))

#write.csv(hist.data, file.path("data", "hist.data.csv"), row.names = FALSE)


# read in the latest data catalogue from CDC (2012 - 2018) ------------------------------------------------------------

new.data <- file.path(("data"),
                       "BCSEE_Plants_Animals_final.csv"
)

new.0 <- read_csv(new.data,
                  col_types = cols_only(
                    Year = col_integer(),
                    `Scientific Name` = col_character(),
                    `English Name` = col_character(),
                    `Element Code` = col_character(),
                    `Prov Status` = col_character(),
                    `Prov Status Review Date` = col_character(),
                    `Prov Status Change Date` = col_character(),
                    `Name Category` = col_character(),
                    `BC List` = col_character(),
                    `Origin` = col_character()
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
  mutate(scientific_name = tolower(trimws(scientific_name, "both")),
         prov_status_review_date = year(prov_status_review_date),
         prov_status_change_date = year(prov_status_change_date))


if (!file.exists("data/tax_key_vert_full.csv")) {

  # create a key with all historic ELcode, names, scinema , Taxanomic
  full_key <- new.0 %>%
    select(scientific_name) %>%
    distinct() %>%
    left_join(new.0 %>%
                select(scientific_name, ELCODE, year) %>%
                filter(!is.na(ELCODE)) %>%
                distinct())

  sum(is.na(full_key$ELCODE))

  full_key$ELCODE[is.na(full_key$ELCODE)] <- lookup_elcodes(full_key$scientific_name[is.na(full_key$ELCODE)])
#  x <- full_key
#  full_key <- x

#  sum(is.na(full_key$ELCODE))

  # remove the inverts keep the NA's
  full_key <- full_key %>%
    filter(is.na(ELCODE)|!startsWith(ELCODE, "I"))

  full_key <- full_key %>% mutate(taxonomic_group = case_when(
     startsWith(ELCODE, "AA")  ~ "Amphibians",
     startsWith(ELCODE, "AB")  ~ "Breeding Birds",
     startsWith(ELCODE, "AF")  ~ "Freshwater Fish",
     startsWith(ELCODE, "AM")  ~ "Mammals",
     startsWith(ELCODE, "AR")  ~ "Reptiles and Turtles",
     is.na(ELCODE) ~ "data_check_required",
    TRUE ~ NA_character_)) #%>%
    #filter(!is.na(taxonomic_group))

  key <- full_key %>%
    select(scientific_name, ELCODE, taxonomic_group) %>%
    distinct() %>%
    mutate(scientific_name = tolower(scientific_name))

  # Create a key with just one row for each ELCODE to use for authoritative names
  latest_key <- group_by(full_key, ELCODE, taxonomic_group) %>%
    filter(year == max(year)) %>%
    select(-year)

  write_csv(key, "data/tax_key_vert_full.csv")
  write_csv(latest_key, "data/tax_key_vert_latest_yr.csv")
} else {
  key <- read_csv("data/tax_key_vert_full.csv")
  latest_key <- read_csv("data/tax_key_vert_latest_yr.csv")
}

bc_key <- new.0 %>% select(year, scientific_name, ELCODE, bc_list, origin) %>%
  group_by(scientific_name) %>%
  filter(year == max(year)) %>%
  select(-year)

# create a maximum year reviewed
max_year_reviewed <- new.0 %>%
  group_by(scientific_name, common_name,  ELCODE) %>%
  summarise(latest_review = max(prov_status_review_date))


goi <- c("Amphibians", "Breeding Birds", "Freshwater Fish", "Mammals", "Reptiles and Turtles", "NA", "data_check_required" )

new.1  <- new.0 %>%
  select(year, scientific_name, common_name, ELCODE,
         prov_status, prov_status_review_date,
         prov_status_change_date) %>%
  mutate(scientific_name = tolower(scientific_name),
         taxonomic_group = case_when(
           startsWith(ELCODE, "AA")  ~ "Amphibians",
           startsWith(ELCODE, "AB")  ~ "Breeding Birds",
           startsWith(ELCODE, "AF")  ~ "Freshwater Fish",
           startsWith(ELCODE, "AM")  ~ "Mammals",
           startsWith(ELCODE, "AR")  ~ "Reptiles and Turtles",
           startsWith(ELCODE, "IILEP") ~ "Lepidoptera",
           startsWith(ELCODE, "IIODO") ~ "Odonata",
           startsWith(ELCODE, "IMBIV") ~ "Molluscs",
           is.na(ELCODE) ~ "data_check_required",
           TRUE ~ NA_character_)) %>%
  filter(is.na(ELCODE)|!startsWith(ELCODE, "I")) %>%
  group_by(scientific_name) %>%
  fill(ELCODE, .direction = "up") %>%
  ungroup() %>%
  filter(str_detect(taxonomic_group, paste(goi,collapse = "|"))) %>%
  select(scientific_name, common_name, year, prov_status, ELCODE) %>%
  spread(year, prov_status) %>%
  distinct()


sum(is.na(new.1$ELCODE))

## notes years for old and new datasets
#hist.data <- 1992 - 2008, 2012 (retro - ranked) # data catalogue
#new.data <-  "2004"  - 2018

# merge hist and new data sets:
all <- full_join(new.1, hist.data,  by = "scientific_name") %>%
  mutate(taxonomic_group = case_when(
    startsWith(ELCODE, "AA")  ~ "Amphibians",
    startsWith(ELCODE, "AB")  ~ "Breeding Birds",
    startsWith(ELCODE, "AF")  ~ "Freshwater Fish",
    startsWith(ELCODE, "AM")  ~ "Mammals",
    startsWith(ELCODE, "AR")  ~ "Reptiles and Turtles",
    startsWith(ELCODE, "IILEP") ~ "Lepidoptera",
    startsWith(ELCODE, "IIODO") ~ "Odonata",
    startsWith(ELCODE, "IMBIV") ~ "Molluscs",
    is.na(ELCODE) ~ "data_check_required",
    TRUE ~ NA_character_))


# flag species which 2012 date does not match between data sets.
# with the new

all <- all %>%
  select(taxonomic_group, scientific_name, common_name, ELCODE,
         "1992", "1995","1997", "1998", "2001","2002" , "2003",
         "2004", "2005.y", "2005.x",  "2006.x", "2006.y", "2007.x","2007.y"  ,
         "2008.x", "2008.y",  "2009" , "2010" , "2011" , "2012.x", "2012.y" ,
          "2013" ,"2014" , "2015" , "2016", "2017"  ,"2018")


all  <- all %>%
  mutate(Update_2012_data = ifelse(`2012.y` ==`2012.x`, 0, 1),
         Update_2005_data = ifelse(`2005.y` ==`2005.x`, 0, 1),
         Update_2006_data = ifelse(`2006.y` ==`2006.x`, 0, 1),
         Update_2007_data = ifelse(`2007.y` ==`2007.x`, 0, 1),
         Update_2008_data = ifelse(`2008.y` ==`2008.x`, 0, 1),
         )


unmatched <- all %>%
  filter(all$Update_2012_data == 1 ) #%>%
#  select(scientific_name, common_name, ELCODE,`2012.x`, `2012.y`)

unmatched05 <- all %>%
  filter(all$Update_2005_data == 1 )# %>%
 # select(scientific_name, common_name, ELCODE,`2005.x`, `2005.y`)

unmatched06 <- all %>%
  filter(all$Update_2006_data == 1 ) #%>%
#  select(scientific_name, common_name, ELCODE,`2006.x`, `2006.y`)

unmatched07 <- all %>%
  filter(all$Update_2007_data == 1 ) #%>%
  #select(scientific_name, common_name, ELCODE,`2007.x`, `2007.y`)

unmatched08 <- all %>%
  filter(all$Update_2008_data == 1 ) #%>%
#select(scientific_name, common_name, ELCODE,`2007.x`, `2007.y`)

# join all species which have unmatched datasets

temp <-bind_rows (unmatched, unmatched05, unmatched06, unmatched07,
                  unmatched08 ) %>%
  select(taxonomic_group, scientific_name, common_name, ELCODE,
         "1992", "1995","1997", "1998", "2001","2002" , "2003",
         "2004", "2005.y", "2005.x",  "2006.x", "2006.y", "2007.x","2007.y"  ,
         "2008.x", "2008.y",  "2009" , "2010" , "2011" , "2012.x", "2012.y" ,
         "2013" ,"2014" , "2015" , "2016", "2017"  ,"2018", everything())

write.csv(temp, file.path("data","sp.check.temp.csv"))


#write.csv(all, file.path("data", "consolidated_output.csv"), row.names = FALSE)

# check groups
#new <- all %>%
#  group_by(taxonomic_group, prov_status_review_date) %>%
#  filter(!is.na(rank)) %>%
#  summarise(count = n())


# Manually verify data  ---------------------------------------------------

# Currently manually vetted data is stored here: consolidated_output_edit.csv
## NOTE THIS FILE HAS BEEN MANUALLY EDITED _ PLEASE DONT WRITE OVER
# I used Species_manually_adjusted.csv to keep track of manual changes mades to the Species_to_check_manually.csv
# This was mostly name changes and duplicates in common name subspecies (spacing / upper/lower)

# Leah Ramsey reviewed species with non-matching codes.
# Their review (SOE_2019_review.csv stored in the data folder).
# --------------------------------------------------------------------------


# Read in Leah's revisions and incorporated into the "consolidated_output_edit.csv" above.

toedit <- read_csv(file.path("data", "SOI_2019_review_GP.csv")) %>%
  select(-c(last_rank, current_SRANK, `Proposed ACTION`, `Leah's comments`, `Date change`,
            PREV_SRANK, NEW_RANK, CODE, REASON_DESC, COMMENTS)) %>%
 rename_all(function(x) tolower(gsub("\\s+", "_", x)))


vdata.0 <- file.path("data", "consolidated_output_edit.csv")

vdata.0 <- read_csv(vdata.0,
                  col_types = cols_only(
                    `Taxonomic_Group` = col_character(),
                    `Scientific_Name` = col_character(),
                    `Common_name` = col_character(),
                    `ELCODE` = col_character(),
                    `1992` = col_character(),
                    `1995` = col_character(),
                    `1997` = col_character(),
                    `1998` = col_character(),
                    `2001` = col_character(),
                    `2002` = col_character(),
                    `2003` = col_character(),
                    `2005` = col_character(),
                    `2006` = col_character(),
                    `2007` = col_character(),
                    `2008` = col_character(),
                    `2010` = col_character(),
                    `2011` = col_character(),
                    `2012.x` = col_character(),
                    `2012.y`= col_character(),
                    `2013` = col_character(),
                    `2014` = col_character(),
                    `2015` = col_character(),
                    `2016` = col_character(),
                    `2017` = col_character(),
                    `2018` = col_character()
                  )) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x)))


vdata <- vdata.0 %>%
  filter(!scientific_name %in% unique(toedit$scientific_name)) %>%
  bind_rows(toedit) %>%
  left_join(bc_key) # join the latest BC list / origin information

 # error check
 #sp.check <-  vdata %>%
#   group_by(elcode) %>%
#   summarise(count = n()) %>%
#   filter(count > 1)


# flag species which 2012 date does not match between data sets.
vdata  <- vdata %>%
  mutate(match2012 = ifelse(`2012.y` ==`2012.x`, 0, 1))

#unmatched <- vdata%>%
#  filter(vdata$match2012 == 1 ) %>%
#  select(scientific_name, common_name, ELCODE,`2012.x`, `2012.y`)

##checked individuals with unmatched ranking (need to adjust four species where sub populations are created)
 # "coturnicops noveboracensis" = 2012.x
 # "brachyramphus marmoratus"= 2012.x
 # "empidonax wrightii"= 2012.x
 # "salvelinus malma - southern lineage"= 2012.x ## NA - from 2004 - 2011
 # "salvelinus malma - northern lineage"= 2012.x ## NA - from 2004 - 2011
 # "hybognathus hankinsoni - western arctic group" = 2012.x ## NA - from 2004 - 2011
 # "hybognathus hankinsoni - pacific group" = 2012.x ## NA - from 2004 - 2011
 # "sander vitreus"= 2012.x

vdata <- vdata %>%
  select(-c(`2012.y`, match2012, ELCODE)) %>%   # remove the old 2012 data sets. (as confirmed above)
  mutate(`2012` =`2012.x`) %>%
  select(-`2012.x`) %>%
  select(c(bc_list, origin, everything())) %>%
  gather("year", "rank", 7:27) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(scientific_name) %>%
  arrange( year, .by_group = TRUE) %>%
  #fill(rank, .direction = "down") %>%
  ungroup()

#indata <- vdata %>%
#  drop_na(rank)

#define the years of assesment per group # this needs some reworking

# am <- c(1992,1998, 2002, 2010, 2018)
# bb <- c(1992, 1997, 2001, 2005, 2009, 2015, 2018)
# ff <- c(1992, 1998, 2001, 2005, 2010, 2012, 2018, 2019)
# ma <- c(1992, 1995, 2001, 2003, 2006, 2007, 2011, 2015, 20017, 2018)
# rt <- c(1992, 1998, 2002, 2008, 2012, 2018)
#
# xx <- indata %>%
#   mutate(keep = ifelse(taxonomic_group == 'Amphibians' & year %in% am, T,
#                        ifelse(taxonomic_group == "Breeding Birds" & year %in% bb, T,
#                               ifelse(taxonomic_group == "Freshwater Fish"  & year %in% ff, T,
#                                      ifelse(taxonomic_group == "Mammals"  & year %in% ma, T,
#                                             ifelse(taxonomic_group == "Reptiles and Turtles"  & year %in% rt, T,F))))))

#indata <- xx %>%
#  filter(keep == TRUE) %>%
#  select(-(keep)) %>%
#  select(taxonomic_group, scientific_name,
#         common_name, elcode, bc_list, origin,
#         year, rank)


# remove exotics
indata <- vdata %>%
  filter(!origin %in% c("Exotic","Unknown/Undetermined"))

keep <- c("Blue", "Yellow", "Red", "Extinct")

indata <- indata %>%
  filter(bc_list %in% keep)


# check the number of species by BC List
#sp.catergory <- x %>%
#  group_by(`bc_list`) %>%
#  summarise(count = n())

# check the number of species by BC List
#sp.catergory <- x %>%
#  group_by(taxonomic_group, year) %>%
#  summarise(count = n())


# check the non-matching species : ie where present in early reports and not in laters (ie: subspecies vs species)


unmatched <- function(tgroup, year1, year2) {

#  tgroup = "Mammals"
#  year1 = 2007
#  year2 = 2011

  t1 <- x %>% filter(taxonomic_group == tgroup, year == year1) %>%
    distinct(scientific_name)
  t2 <- x %>% filter(taxonomic_group == tgroup, year == year2) %>%
    distinct(scientific_name)
  out1 <- anti_join(t2, t1)
  out2 <- anti_join(t1, t2)
  out <- bind_rows(out1, out2)
  out
}

mammals <- unmatched("Mammals", year1 = 2007, year2 = 2011 )
reptiles <- unmatched("Reptiles and Turtles", 2008, 2017)
amphibians <- unmatched("Amphibians", 2010, 2018)
ff <- unmatched("Freshwater Fish", 2010, 2018)
bb <- unmatched("Breeding Birds", 2001, 2018)

unmatch_sp <- bind_rows(mammals, reptiles, amphibians, ff, bb)


longsp <- unmatch_sp %>%
    left_join(., vdata.0)

write.csv(longsp, file.path("data", "sp_subsp_check.csv"))




#write.csv(indata, file.path("data", "indata.csv"), row.names = FALSE)

saveRDS(indata, file.path("data","indata.R"))


