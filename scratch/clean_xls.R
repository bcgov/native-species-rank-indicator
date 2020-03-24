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
library(readxl)

source("R/lookup_elcode.R")

# Data set 1: read in data set already formatted (1992 - 2012)

hist.data  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

hist.data <- hist.data %>%
  mutate(Scientific_Name = tolower(Scientific_Name)) %>%
  select(-Common_Name) %>%
  spread(Year, SRank) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x)))

# Data set 2: read in the rank change data sheet

change.data <- file.path("data",
                      "Copy of Rank_Changes_Verts_Leps_Odonates_Molluscs2.csv"
)

cdata.0 <- read_csv(change.data,
                  col_types = cols_only(
                    ELCODE = col_character(),
                    SCIENTIFIC_NAME = col_character(),
                    ENGLISH_NAME = col_character(),
                    CURRENT_SRANK = col_character(),
                    BC_LIST = col_character(),
                    RANK_REVIEW_DATE = col_date(),
                    RANK_CHANGE_DATE = col_date(),
                    CHANGE_ENTRY_DATE = col_date(),
                    PREV_SRANK = col_character(),
                    NEW_RANK = col_character(),
                    CODE= col_number(),
                    REASON_DESC = col_character(),
                    COMMENTS = col_character()
                  )
) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x))) %>%
  filter(!startsWith(elcode, "I")) %>%
  filter(!grepl("^(Search|Sort|Open|Animals)", scientific_name),
         !is.na(scientific_name)) %>%
  filter(!bc_list == "Accidental") %>%
  mutate(scientific_name = tolower(trimws(scientific_name, "both")),
         review_year = year(rank_review_date),
         change_year = year(rank_change_date),
         change_entry_yr = year(change_entry_date),
         taxonomic_group = case_when(
           startsWith(elcode, "AA")  ~ "Amphibians",
           startsWith(elcode, "AB")  ~ "Breeding Birds",
           startsWith(elcode, "AF")  ~ "Freshwater Fish",
           startsWith(elcode, "AM")  ~ "Mammals",
           startsWith(elcode, "AR")  ~ "Reptiles and Turtles",
           TRUE ~ NA_character_))

# keep the full set of change data to use for assessing the multiple review of new data

cdata <- cdata.0 %>%
  filter(change_year > 2012)


# create a list of unique species codes

elcode_list <- as.list(unique(cdata$elcode))

out <- lapply(elcode_list, function(x) {

#  x <- elcode_list[3]

  sp.rows <- cdata %>%
    filter(elcode == x)

  if(any(is.na(sp.rows$change_entry_date))) {

    sp.rows <- sp.rows %>%
      mutate(change_entry_yr = ifelse(
        is.na(change_entry_yr), change_year, change_entry_yr),
        new_rank = ifelse(is.na(new_rank), current_srank, new_rank))
  }

  sp.data <- sp.rows %>%
    select(elcode, scientific_name, current_srank,
           taxonomic_group, current_srank,
           new_rank, change_year,change_entry_yr,
           code, reason_desc, comments) %>%
    filter(change_entry_yr > 2012) %>%
    distinct() %>%
    select(elcode, scientific_name, current_srank, new_rank,
           change_entry_yr, code, comments)

  if(nrow(sp.data)>1){

    sp.data <- sp.data %>%
      group_by(elcode, scientific_name, current_srank,  new_rank,  change_entry_yr) %>%
      filter(!is.na(code)) %>%
      top_n(., 1) %>%
      ungroup()

  } else {

    sp.data

  }

})

out <- do.call("rbind", out) %>%
  select(-current_srank) %>%
  rename(prov_status = new_rank,
         prov_status_review_date = change_entry_yr)



# # Data set 3: latest data catalogue from CDC (2012 - 2018) ------------------------------------------------------------

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
   # xx <- new.0 %>%
    select(scientific_name) %>%
    distinct() %>%
    left_join(new.0 %>%
                select(scientific_name, ELCODE, year) %>%
                filter(!is.na(ELCODE)) %>%
                distinct())

  sum(is.na(full_key$ELCODE))

  full_key$ELCODE[is.na(full_key$ELCODE)] <- lookup_elcodes(full_key$scientific_name[is.na(full_key$ELCODE)])

  sum(is.na(full_key$ELCODE))

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

  write_csv(key, "data/tax_key_vert_full.csv")

} else {

  key <- read_csv("data/tax_key_vert_full.csv") %>%
    rename_all(function(x) tolower(x))

}


# generate the years when reviews occured per species

reviews <- new.0 %>%
  select(year, scientific_name, ELCODE, prov_status,
         prov_status_review_date,
         prov_status_change_date) %>%
  filter(year > 2012) %>%
 # select(- year) %>%
  distinct() %>%
  filter(prov_status_review_date > 2012) %>%
  filter(is.na(ELCODE)|!startsWith(ELCODE, "I")) %>%
  rename_all(function(x) tolower(x)) %>%
  left_join(key)

# separate those with multiple year x ranks or conflicts with year and rank from those
# with single year.

singles <- reviews %>%
  select(-year) %>%
  distinct() %>%
  group_by(scientific_name, prov_status_review_date) %>%
  summarise(count = n()) %>%
  distinct() %>%
  filter(count == 1) %>%
  select(scientific_name)

# these species were being flagged for both single and doubles so split out. In 2013 non-conflict,
# but in 2015 conflict with multiple ranks for same year.

duplicates.sp <- c("geothlypis trichas", "phalacrocorax auritus")

singles.long <- reviews %>%
  select(-year, -prov_status_change_date) %>%
  distinct() %>%
  filter(scientific_name %in% singles$scientific_name)  %>%
  filter(!scientific_name %in% duplicates.sp) %>%
  distinct()

# doubles - add the change dataset and

doubles <- reviews %>%
  select(-year, -prov_status_change_date) %>%
  filter(!scientific_name %in% singles$scientific_name) %>%
  distinct()

double.sp <- as.list(unique(doubles$scientific_name))

# fix difference due to multiple ELCODES per species.

elcode.fix <- lapply(double.sp, function(x) {

  sp.rows <- doubles %>%
    filter(scientific_name == x)

    if(length(unique(sp.rows$elcode))>1){

      sp.rows %>% mutate(comment = "check multiple elcodes per sciname")

    } else {

      sp.rows %>% mutate(comment = "still need to fix these")

    }

})

elcode.fix <- do.call("rbind", elcode.fix) %>%
  filter(comment == "check multiple elcodes per sciname")


# fix difference due to multiple ranks per species per year

double.sp <- doubles %>%
  select(scientific_name) %>%
  filter(!scientific_name %in% elcode.fix$scientific_name)

double.sp <- as.list(unique(double.sp$scientific_name))


rank.fix <- lapply(double.sp, function(x) {

sp.rows <- doubles %>%
    filter(scientific_name == x)

  if(length(unique(sp.rows$prov_status))>1) {

    # get the year where there is multiple ranks
    conflict.yr <- unique(sp.rows$prov_status_review_date)

    # filter the original review data to find matching sp. and conflict year
    # select the rank at the most recent annual year ie: 2018)
    review.dates <- reviews %>%
      filter(scientific_name == x & prov_status_review_date == conflict.yr) %>%
      filter(year == max(year)) %>%
      select(prov_status) %>%
      pull()

    # filter the doubles to match the most current rank & add comment.
    sp.data <- sp.rows %>%
      select(elcode, scientific_name, prov_status, prov_status_review_date, taxonomic_group) %>%
      filter(prov_status == review.dates) %>%
      mutate(comment = "multiple sranks - selected most recent srank")

    sp.data

  } else {

    print (paste0 ("no rank conflict for ", x))
  }

})

rank.fix <- do.call("rbind", rank.fix)

# now assemble all data and spread to wide format

doubles <- bind_rows(elcode.fix, rank.fix, singles.long)

all.data <- bind_rows(doubles, out)

all.data <- all.data %>%
  spread(prov_status_review_date, prov_status)

#length(all.data$taxonomic_group)
#[1] 1035



# merge issue here as dropping species without historic records

all.wide <- left_join(all.data, hist.data, by = c("scientific_name")) %>%
  select(scientific_name, elcode, comment, code, comments,
         "1992", "1995","1997", "1998", "2001","2002" , "2003",
         "2005","2006", "2007", "2008", "2012",
         "2013" ,"2014" , "2015" , "2016", "2017" ,"2018", "2019", everything()) %>%
  mutate(taxonomic_group = case_when(
    startsWith(elcode, "AA")  ~ "Amphibians",
    startsWith(elcode, "AB")  ~ "Breeding Birds",
    startsWith(elcode, "AF")  ~ "Freshwater Fish",
    startsWith(elcode, "AM")  ~ "Mammals",
    startsWith(elcode, "AR")  ~ "Reptiles and Turtles",
    is.na(elcode) ~ "data_check_required",
    TRUE ~ NA_character_)) %>%
    distinct()

length(all.wide$scientific_name)

# add the changes data to

write.csv(all.wide, file.path("data","sp.check.temp.wide.csv"))





pseudacris maculata ~ AAABC05130

#Fill in missing elcode
added_elcode <- tribble(~scientific_name,~ elcode,
                         "pseudacris maculata", "AAABC05130",
                         "accipiter gentilis laingi", "ABNKC12062"
                        aegolius acadicus "ABNSB15020"





### Up To HERE - ignore below here.








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


