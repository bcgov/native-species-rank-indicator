# Copyright 2020 Province of British Columbia
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


# import and consolidate invertebrte data:


library(readxl)
library(dplyr)
library(stringr)

source("R/lookup_elcode.R")


# read in data files
invert.files <- list.files(file.path("data", "Inverts", "Completed"), pattern = "_final.xlsx", full.names = TRUE)

invert.files

idata <- read_xlsx(invert.files[1], na = "NA")

yrs <- c('1995', '1999', '2001', '2004', '2005', '2006', '2007', '2008',
        '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016',
        '2017', '2018')

yrs_adjust <- str_c(yrs, "_Adj_SRank")

ycols <- c(yrs, yrs_adjust)

#unique(idata$`1995`)


idata <- idata %>%
  dplyr::select(ELCODE, scientific_name, common_name, other_scientifi_names,
           taxonomic_group, all_of(ycols))

combo_yrs <- function(df, yr) {

  df <- idata
  yr <- '1995'

  yr_adj = paste0(yr, "_Adj_SRank")


  xx<- df %>%
     mutate(final = ifelse(is.na( !!sym(yr_adj)), !!sym(yr),!!sym(yr_adj))) %>%
    dplyr::select(yr, yr_adj, final)

  xx %>%
    rename(!!sym(yr) == final)


}

x <- idata %>%
 mutate(`1995c` = ifelse(is.na(`1995_Adj_SRank`), `1995`, `1995_Adj_SRank`)) %>%




  dplyr::select(`1995`, `1995_Adj_SRank`, `1995c`)




%>%
  filter(is.na(srank))



  group_by(ElCODE, scientific_name, common_name, other_scientifi_names,
           taxonomic_group, year) %>%
  summarise


ilong <- idata %>%
  gather(., all_of(ycols), key = "year", value = "srank") %>%
  filter(!is.na(srank))

i.long



ilong
idata
change.data <- file.path("data",
                         "Copy of Rank_Changes_Verts_Leps_Odonates_Molluscs2.csv"
)



# Data set 1: read in data set already formatted (1992 - 2012)

hist.data  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

hist.data <- hist.data %>%
  mutate(Scientific_Name = tolower(Scientific_Name)) %>%
  select(-Common_Name) %>%
  spread(Year, SRank) %>%
  rename_all(function(x) tolower(gsub("\\s+", "_", x)))

