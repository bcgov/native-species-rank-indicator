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


# Create tables to populate

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
  mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibias",
                                  ifelse(startsWith(ELCODE,"AB"), "Breeding Birds",
                                         ifelse(startsWith(ELCODE,"AF"), "Freshwater Fish",
                                                ifelse(startsWith(ELCODE, "AM"), "Mammals",
                                                       ifelse(startsWith(ELCODE, "AR"), "Reptiles and Turtles",
                                                              ifelse(startsWith(ELCODE, "IIL"), "Lepidoptera",
                                                                     ifelse(startsWith(ELCODE, "IIO"),"Odonata",
                                                                            ifelse(startsWith(ELCODE, "IM"), "Molluscs",
                                                                                   NA))))))))) %>%

  select(-c("Element_code"))

# make a subset list of sci name and elcode to update missing elcodes
#all.sp.list <- ref %>%
#  select(c(Scientific_name, ELCODE)) %>%
#  distinct()

group.oi <- c("Odonata", "Lepidoptera", "Molluscs")

for(i in group.oi) {

gref <-   ref %>%
  filter(Taxonomic_Group ==  i) %>%
  select(ELCODE, Scientific_name, Year, Prov_Status) %>%
  spread(Year, Prov_Status) %>%
  distinct()

## add the other column names using years as reference.
#years <- unique(gref$Year)
#do.call(paste0, Map(strrep, names(df), df))

write.csv(gref, file.path("data",paste0(i, "_historic.csv",sep = "")), row.names = FALSE)

}

# read in and check

# Odonata
odata<- read_csv(file.path("data", "Odonata_historic.csv", sep = ""))
oyears <- as.list(names(odata)[str_detect(names(odata),"2")])

col.names.fn <- function(x) {
  adjname <- paste0(x,"_Adj_SRank")
  adjcode <- paste0(x,"_Adj_SRank_Code")
  adjcomm <- paste0(x,"_Adj_SRank_Comment")
  new.col.names <- c(adjname, adjcode, adjcomm)
  new.col.names
}

new.cols.to.add <- unlist(map(oyears, col.names.fn))

# add this to the end of data frame and export



