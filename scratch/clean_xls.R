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

library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)
library(stringr)
library(purrr)


#excel_file <- file.path(
#  soe_path("Operations ORCS/Data - Working/plants_animals/trends-status-native-species/2019"),
#           "Rank_Changes_Verts_Leps_Odonates_Molluscs.xlsx"
#  )

# or read Git local version
excel_file <- file.path("data",
                        "Rank_Changes_Verts_Leps_Odonates_Molluscs.xlsx")


sdata <- read_excel(excel_file, sheet = "Query Output",
                    range = "A2:M877",
                    col_types = c("numeric", "text", "numeric",
                                  rep("text", 2), rep("date",3),
                                  rep("text", 2), "numeric",
                                  rep("text", 2)),
                    col_names = c("id", "ELCODE", "est_id",
                                  "Scientific_Name", "Common_Name",
                                  "Rank_Review_Date", "Range_change_Date",
                                  "Change_Entry_Date", "prev_SRank",
                                  "new_SRank", "code", "reason", "comment"))  %>%

      mutate(Taxonomic_Group = ifelse(startsWith(ELCODE,"AA"),"Amphibias",
                                      ifelse(startsWith(ELCODE,"AB"),"Breeding Birds",
                                             ifelse(startsWith(ELCODE,"AF"),"Freshwater Fish",
                                                    ifelse(startsWith(ELCODE,"AM"),"Mammals",
                                                           ifelse(startsWith(ELCODE,"AR"),"Reptiles and Turtles", NA)))))) %>%

      select(Taxonomic_Group, Scientific_Name, Common_Name, everything())


# still need to rename the invers

data_summary <- sdata %>%
  group_by(Taxonomic_Group) %>%
  summarise(count = n())



