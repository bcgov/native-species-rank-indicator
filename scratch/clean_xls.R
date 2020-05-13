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
library(tidyr)
library(envreportutils)
library(stringr)
library(lubridate)
library(readr)
library(readxl)
library(dplyr)
source("R/lookup_elcode.R")

# Data set 1: read in data set already formatted (1992 - 2012)

hist.data  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

hist.data <- hist.data %>%
  mutate(Scientific_Name = tolower(Scientific_Name)) %>%
  select(-Common_Name) %>%
  spread(Year, SRank) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x)))


# fix the fish names :

hist.data <- hist.data %>%
  mutate(scientific_name = ifelse(
    scientific_name == "acipenser transmontanus - columbia river", "acipenser transmontanus pop. 2",
    ifelse(scientific_name == "acipenser transmontanus - kootenay river", "acipenser transmontanus pop. 1",
           ifelse(scientific_name == "acipenser transmontanus - lower fraser river"  ,  "acipenser transmontanus pop. 4",
                  ifelse(scientific_name == "acipenser transmontanus - middle fraser river" , "acipenser transmontanus pop. 6",
                         ifelse(scientific_name == "acipenser transmontanus - nechako river",  "acipenser transmontanus pop. 3",
                                ifelse(scientific_name =="acipenser transmontanus - upper fraser river",  "acipenser transmontanus pop. 5",
                                       ifelse(scientific_name == "lampetra richardsoni - morrison creek non-migratory parasitic form",
                                              "lampetra richardsoni pop. 1",
        ifelse(scientific_name =="lota lota - lower kootenay river population", "lota lota pop. 1",
        ifelse(scientific_name == "thymallus arcticus-nahanni lineage", "thymallus arcticus - nahanni lineage",
        ifelse(scientific_name == "thymallus arcticus-northern beringean lineage", "thymallus arcticus - northern beringean lineage",
                                       scientific_name)))))))))))

# Data set 2: read in the rank change data sheet

change.data <- file.path("data","raw",
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

new.data <- file.path("data","raw",
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

if (!file.exists("data/raw/tax_key_vert_full.csv")) {

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

  write_csv(key, "data/raw/tax_key_vert_full.csv")

} else {

  key <- read_csv("data/raw/tax_key_vert_full.csv") %>%
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


#Fix multiple codes
added_elcode <- tribble(~ scientific_name, ~ elcode,
                          "sula nebouxii", "ABNFB01020",
                          "aphelocoma californica", "ABPAV06070",
                          "eugenes fulgens", "ABNUC37020",
                          "glaucomys sabrinus",	"AMAFB09030",
                          "loxia curvirostra", "ABPBY05050",
                          "oceanodroma leucorhoa", "ABNDC04130",
                          "ochotona princeps", "AMAEA01030",
                          "perognathus parvus",	"AMAFD01100"
                        )


elcode.fix <- elcode.fix %>%
   filter(elcode %in% added_elcode$elcode) %>%
   mutate(comment = "fixed multiple elcode")

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

# add historic data using full join to stop dropping species present in only new or historic
# data sets - code below deals with the duplicate/messiness as a result

all.wide <- full_join(all.data, hist.data) %>%
  select(taxonomic_group, scientific_name, elcode, comment, code, comments,
         "1992", "1995","1997", "1998", "2001","2002" , "2003",
         "2005","2006", "2007", "2008", "2012",
         "2013" ,"2014" , "2015" , "2016", "2017" ,"2018", "2019", everything()) %>%
    distinct()


# Data fixes:

# 1: add elcode to missing elcode

sum(is.na(all.wide$elcode))

missing.elcode.all <- all.wide %>%
  filter(is.na(elcode)) %>%
  select(-(elcode)) %>%
  left_join(key) %>%
  select(scientific_name, elcode)

all.wide  <- all.wide %>%
  left_join( missing.elcode.all, by = "scientific_name") %>%
  mutate(elcode = ifelse(is.na(elcode.x), elcode.y, elcode.x)) %>%
  select(-elcode.x, -elcode.y)


# 2. fix the species with duplicate elcode:

multiple.elcodes <- all.wide %>% group_by(scientific_name) %>%
  summarise(n.elcode = length(unique(elcode))) %>%
  filter(n.elcode > 1) %>%
  pull(scientific_name)


all.wide <- all.wide %>%
  mutate(elcode = ifelse(scientific_name == "larus glaucoides","ABNNM03270",
                         ifelse(scientific_name ==  "martes americana", "AMAJF01040",
                                elcode)))

# note still some missing elcodes (NAs as dont match key)

# 3 : add taxanomic field if missing

all.wide <- all.wide %>%
  mutate(taxonomic_group = case_when(
  startsWith(elcode, "AA")  ~ "Amphibians",
  startsWith(elcode, "AB")  ~ "Breeding Birds",
  startsWith(elcode, "AF")  ~ "Freshwater Fish",
  startsWith(elcode, "AM")  ~ "Mammals",
  startsWith(elcode, "AR")  ~ "Reptiles and Turtles",
  TRUE ~ NA_character_)) %>%
  mutate(comment = ifelse(is.na(elcode),"check elcode manually", comment))


# 4. merge rows with multiple rows per species due to hist and new data inconsistencies
# convert all data to long format then cycle through species

all.long <- all.wide %>%
  gather(.,  "1992", "1995","1997", "1998", "2001","2002" , "2003",
         "2005","2006", "2007", "2008", "2012",
         "2013" ,"2014" , "2015" , "2016", "2017" ,"2018", "2019",
         key = "year", value = "srank")

sp.list <- as.list(unique(all.long$scientific_name))

all.long.temp <- lapply(sp.list, function (x) {
#  x <- sp.list[2]
  sp.temp <- all.long %>%
    filter(scientific_name == x)

  if(length(sp.temp$year) == length(unique(sp.temp$year))) {

    sp.temp %>%
      select(taxonomic_group, scientific_name,  elcode, year, comment, code,
             comments, srank)

  } else {

    sp.temp %>%
      group_by(taxonomic_group, scientific_name, elcode, year) %>%
      summarise(comment = as.character(max(comment, na.rm = TRUE)),
                code = as.character(max(code, na.rm = TRUE)),
                comments = as.character(max(comments, na.rm = TRUE)),
                srank = as.character(max(srank, na.rm = TRUE))) %>%
      mutate(code = as.numeric(code)) %>%
      ungroup()

  }
})

all.long.temp  <- do.call("rbind", all.long.temp)


## step 1: manually check species with no elcode and manually update following guidelines
# from CDC (see below for details)

# 1) catostomus catostomus - chehalis lineage	NA # cant find this species in new data (listed as S1? in hist)

      # LGelling - This must have been split up by lineage historically, we don’t have this in Biotics and I can’t find any paper trail.  I have asked Debbie for details.
      # Debbie W: “I can’t find any record of this record. It’s not in the deleted schema either so if we did track it, it may have been prior to 2003.”

      # Action - remove from analysis


# 2) coregonus sp. 1	NA #(SX) only included in ranking up to 2009 : no elcode in large data table.


      # LGelling : Extinct, but why not in biotics?  I assume a taxonomic change – I’ve asked Debbie.
      #	DW: “It was deleted on January 15, 2010. I don’t have any other information as I have not been able to access my really old e-mail (>5 years old) for some time now.”
      #	http://www.env.gov.bc.ca/wld/documents/ss03fraser1.pdf

      # Action - remove , extinct species ignored and species currently not in biotics with no elcode

sp.to.remove <-  c("catostomus catostomus - chehalis lineage", "coregonus sp. 1")

all.long.temp <- all.long.temp %>%
  filter(!scientific_name %in% sp.to.remove)


# 3) #oncorhynchus mykiss - coastal lineage	NA  # these dont appear in large data sheet
     #oncorhynchus mykiss- interior lineage	NA

    # G Perkins: Should I update all years to SNR ? How many lineages are there? I currently see these:
    # Should the coastal and interior be lumped in with either of these

    # Lea Gelling:  I now remember that I was going to sort all steelhead last year, but
              #it was really confusing, so I post-poned (except pop 47 and 46) .
              # I believe what is happening is that COSEWIC is establishing DUs,
              # pop 46/47 (Chilcotin/Thompson) were evaluated based on emergency
              # listing  (by COSEWIC).  I was able to use the COSEWIC report data
              # to establish the S rank for the two pops.
              #As for the other ones, I believe what I need to do is figure out the
              # same question you asked – are the large lake/interior/coastal elements
              # going to be new DUs, based on COSEWIC DUs?
              # I will ask Greg Wilson about where this is at and get back to you.

    # Lea Gelling : Yes include the lineages (April 23rd 2020)

    # ACTION : Include the lineages:

all.long.temp <- all.long.temp %>%
  mutate(elcode = ifelse(scientific_name == "oncorhynchus mykiss - coastal lineage", "AFCHA0213E",
                         ifelse(scientific_name == "oncorhynchus mykiss- interior lineage", "AFCHA0213D", elcode)))


# 3) spirinchus thaleichthys - pygmy form from pitt and harrison lakes	NA # does not occur in new data (not sp. 1 as only assessed after 2010)

     # L Gelling: NatureServe Explorer says: Nonanadromous populations in Harrison and Pit lakes, British Columbia, have been recognized as an undescribed species (Spirinchus sp 1) by some authors. In Biotics we have Spirinchus sp 1; “Pygmy longfin Smelt”.
      # Action : Update historic data to Spirinchus sp 1; “Pygmy longfin Smelt”.


all.long.temp <- all.long.temp %>%
  mutate(scientific_name = ifelse(scientific_name %in% c("spirinchus thaleichthys - pygmy form from pitt and harrison lakes",
                                          "spirinchus sp. 1"), "spirinchus sp. 1", scientific_name)) %>%
  mutate(elcode = ifelse(scientific_name == "spirinchus sp. 1", "AFCHB03030", elcode))


## 4) Is Thymallus arcticus - Southern Beringean lineage the same as Thymallus arcticus - Northern Beringean lineage ??


     # L Gelling: We added arctic grayling in 2012 – this is from the archived 2012 changes spreadsheet.
                # I can’t figure out when it was in there as northern, but it must be a mistake.

    #Action: update the historic name to Thymallus arcticus - Souuthern Beringean lineage

all.long.temp <- all.long.temp %>%
  mutate(scientific_name = ifelse(scientific_name == "thymallus arcticus - northern beringean lineage",
                                  "thymallus arcticus - southern beringean lineage", scientific_name),
      elcode = ifelse(scientific_name == "thymallus arcticus - southern beringean lineage", "AFCHB03030", elcode))


# check nas
#unique(all.long.temp  %>% filter(is.na(elcode))%>%
#  select(scientific_name))


 ## convert back to wide format
all.wide <- all.long.temp %>%
  spread(year, srank)




        # Step 2:  now check if pre 2012 rank == post 2012 rank

        # flag species which 2012 date does not match between data sets.

        # check which sp have all NAs and the srank at the most recent time reviewed

        rank_check <- lapply(sp.list, function(sps) {

          #sps <- sp.list[10]

          pre_2012_rank <- all.wide %>%
            filter(scientific_name == sps) %>%
            select(`1992`, `1995`,`1997`,`1998`, `2001`,`2002`, `2003`,`2006`,`2007`,`2008`,`2012`) %>%
            gather(key = "year", value = "srank") %>%
            filter(!is.na(srank)) %>%
            filter(year == max(as.numeric(year)))
          # if table is blank then no rank before 2012

          # if there was a rank before 2012 check the post 2012 rank and compare

          if(length(pre_2012_rank$year) > 0) {

            post_2012_rank <- all.wide %>%
              filter(scientific_name == sps) %>%
              select(`2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`) %>%
              gather(key = "year", value = "srank") %>%
              filter(!is.na(srank))
            #  filter(year == min(as.numeric(year)))

            out <- bind_rows(pre_2012_rank, post_2012_rank) %>%
              mutate(srank = sub("B", "", srank)) %>% # remove differences by "Breeding prefix"
              mutate(srank = sub(", SNRN", "", srank)) %>%
              mutate(srank = sub(", SUN", "", srank))

            if(length(unique(out$srank)) > 1) {

             out %>%
                mutate(scientific_name = sps)
            }
          }

        })

        sp.to.check <- do.call("rbind", rank_check)


        # add change data information

        change.code <- cdata.0 %>%
          select(scientific_name, prev_srank, new_rank, code, reason_desc, comments) %>%
          drop_na()

        sp.with.change <- sp.to.check %>%
          left_join(change.code )

            #write.csv(sp.with.change, file.path("data","raw","sp.check.2012.ranks_change.csv"))

        ## remove species with real changes

        sp.ids <- as.list(unique(xx$scientific_name))

        sp.with.real.change <- lapply(sp.ids, function (sp){

        #sp <- sp.ids[[3]]
          code_to_skip  <- xx %>%
            filter(scientific_name == sp ) %>%
            dplyr::select(code) %>%
            distinct() %>%
            pull

          # get species with all change code = 1 or 2
         if(length(code_to_skip) == 1 & code_to_skip %in% c(1,2,3 )){
           sp
         }

        })

        sp.with.real.change <- as.vector(as.character(plyr::compact(sp.with.real.change)))

        # review the species with changes:

        # remove the species who has real change in rank

        sp.with.change = sp.with.change %>%
          filter(scientific_name %in% setdiff(sp.with.change$scientific_name, sp.with.real.change))


        # compare the change species list with the list previously reviewed by Leah
        # cross check the species and for those Leah already provided comment update the all.wide table

        sp.previously.reviewed <- read_csv(file.path ("data", "raw", "SOI_2019_review_GP.csv"))
        names(sp.previously.reviewed) = c("taxonomic_group", "elcode", "scientific_name",
                                          "last_rank", "current_srank", "proposed.action",
                                          "leah.s.comments", "date.change", "prev_srank",
                                          "new_rank" ,  "code", "reason_desc", "comments",
                                          "1992", "1995" , "1997", "1998", "2001",  "2002",
                                          "2003", "2005", "2006",  "2007" ,"2008", "2010",
                                          "2011", "2012", "2012.y", "2013", "x2014",
                                          "2015" ,"2016" ,"2017", "2018", "2019" )

        # cross reference the names in the review list with with previously reviewed species

        prev.reviewed.sp <- unique(sp.previously.reviewed$scientific_name)

        reviewed.sp.to.update.main.table <- sp.with.change %>%
          filter(scientific_name %in% prev.reviewed.sp) %>%
          select(scientific_name) %>%
          distinct() %>%
          pull()

        species.for.Lea <- c("progne subis", "acipenser medirostris", "coregonus nasus",
                             "lampetra ayresii", "rhinichthys osculus")


        reviewed.sp.to.update.main.table <- setdiff(reviewed.sp.to.update.main.table, species.for.Lea)

        # format previously checked table to match wide table
        sp.to.update <- sp.previously.reviewed %>%
          dplyr::filter(scientific_name %in% reviewed.sp.to.update.main.table) %>%
          select(-c(proposed.action, last_rank, current_srank,
                    date.change, prev_srank, new_rank, `2012.y`)) %>%
          rename(coment = reason_desc)

        # filter out species to be updated and add previously reviewed data.
        #all.wide <- all.wide %>%
        #  filter(!scientific_name %in% reviewed.sp.to.update.main.table) %>%
        #  bind_rows(sp.to.update)


        # generate a table for Lea to review
        table.for.lea <-  sp.with.change  %>%
         filter(!scientific_name %in% reviewed.sp.to.update.main.table) %>%
          left_join(all.wide, by = c("scientific_name")) %>%
          dplyr::select(-c(year, srank, comment)) %>%
          distinct()

        write.csv(table.for.lea , file.path("data","raw","table_for_lea.csv"))


        # manually reviewed this table and added a proposed action for Lea to review.
        # will need to read this back in, edit and then update the all.wide table.







# read in table Lea has reviewed and formatted :

updates <- read_excel(file.path("data","raw","2020May8_lea_edit.xlsx"),
                      sheet = "updated_table_to_incorporate") %>%
  select(-c(`...1`, comments.x, comments.y, `propsed action`, `LG agreed`,
            `LG comments`,prev_srank, new_rank, code.x, reason_desc))

update.sp <- unique(updates$scientific_name)


# remove old infomation about species
all.wide.keep <- all.wide %>%
  filter(!scientific_name %in% update.sp)

# replace with manually edited updates
all.wide.final <- bind_rows(all.wide.keep, updates) %>%
  select(-c(comment, code, comments)) %>%
  drop_na(., scientific_name) %>%
  filter(!scientific_name == "myotis keenii")



# Final tidy for bcdata output --------------------------------------------


# comvert to long
all.long <- all.wide.final %>%
  pivot_longer(., cols = -c(taxonomic_group, scientific_name, elcode),
               names_to = "year", values_to = "srank")


# add the provincial rankings and common names

prov_list <- read_csv(file.path("data","raw",
                                "BCSEE_Plants_Animals_final.csv"),
                      col_types = cols_only(
                        Year = col_integer(),
                        `Scientific Name` = col_character(),
                        `English Name` = col_character(),
                        `Element Code` = col_character(),
                        `Name Category` = col_character(),
                        `BC List` = col_character(),
                        `Origin` = col_character()
                      )
) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x))) %>%
  filter(!name_category %in% c("Vascular Plant", "Non-Vascular Plant",
                               "Nonvascular Plant", "Fungus",
                               "International Vegetation Classification")) %>%
  mutate(scientific_name = tolower(scientific_name)) %>%
  group_by(scientific_name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(-c(year, name_category))




# add provincial listing

all.long.prov <- all.long%>%
  left_join(prov_list, by = "scientific_name")

no.list = all.long.prov %>%
  filter(is.na(bc_list)) %>%
  select(scientific_name) %>%
  unique()

#unique(all.long.prov$bc_list)

# add prov list where missing

bc_list_updates <- tribble(
  ~scientific_name, ~ bc_list,
    "couesius plumbeus pop. 2", "Red",
    "melanitta deglandi", "Yellow",
    "oncorhynchus nerka pop. 10", "No Status",
    "oncorhynchus nerka pop. 11", "No Status",
    "oncorhynchus nerka pop. 12", "No Status",
    "oncorhynchus nerka pop. 13", "No Status",
    "oncorhynchus nerka pop. 14", "No Status",
    "oncorhynchus nerka pop. 15", "No Status",
    "oncorhynchus nerka pop. 16", "No Status",
    "oncorhynchus nerka pop. 17", "No Status",
    "oncorhynchus nerka pop. 18", "No Status",
    "oncorhynchus nerka pop. 19", "No Status",
    "oncorhynchus nerka pop. 20", "No Status",
    "oncorhynchus nerka pop. 21", "No Status",
    "oncorhynchus nerka pop. 22", "No Status",
    "oncorhynchus nerka pop. 23", "No Status",
    "oncorhynchus nerka pop. 24", "No Status",
    "oncorhynchus nerka pop. 25", "No Status",
    "oncorhynchus nerka pop. 26", "No Status",
    "oncorhynchus nerka pop. 27", "No Status",
    "oncorhynchus nerka pop. 28", "No Status",
    "oncorhynchus nerka pop. 29", "No Status",
    "oncorhynchus nerka pop. 30", "No Status",
    "oncorhynchus nerka pop. 31", "No Status",
    "oncorhynchus nerka pop. 9", "No Status",
    "prosopium coulterii pop. 1", "Blue",
    "prosopium coulterii pop. 3", "Yellow",
    "sorex obscurus", "Yellow",
    "oncorhynchus mykiss- interior lineage", "No Status",
    "oncorhynchus mykiss - coastal lineage", "No Status"
  )

all.long <- all.long.prov %>%
  left_join(., bc_list_updates, by = "scientific_name") %>%
  mutate(bc_list.x = ifelse(is.na(bc_list.x), bc_list.y, bc_list.x)) %>%
  rename(bc_list = bc_list.x) %>%
  select(-c(bc_list.y, element_code))

## data checks
#no.list = all.long %>%
#  filter(is.na(bc_list)) %>%
#  select(scientific_name) %>%
#  unique()

## check common names
#no.name = all.long %>%
#  filter(is.na(english_name)) %>%
#  select(scientific_name) %>%
#  unique()


all.long <- all.long %>%
  rename(common_name = english_name) %>%
  select(elcode, taxonomic_group, scientific_name, common_name,
         bc_list, origin, year, srank)



write.csv(all.long , file.path("data", "verts_retroranks.csv"), row.names = FALSE)

#saveRDS(all.long, file.path("data","indata.R"))

