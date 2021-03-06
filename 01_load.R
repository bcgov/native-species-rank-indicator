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


library(readr)
library(dplyr)


# read in data set from data catalogue
#ref.0  <- read_csv("https://catalogue.data.gov.bc.ca/dataset/bc-vertebrates-conservation-status-rank-history-1992-2012/resource/842bcf0f-acd2-4587-a6df-843ba33ec271https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")


# or read in a local copy temporarily

verts <- read_csv(file.path("data", "verts_retroranks.csv"))
inverts <- read_csv(file.path("data", "inverts_retroranks.csv"))

indata <- bind_rows(verts, inverts)


# check the number of species by types:
sp.type <- indata %>%
  group_by(origin) %>%
  summarise(count = n())


# check the number of species by BC List
sp.catergory <- indata %>%
  group_by(`bc_list`) %>%
  summarise(count = n())


# check the number of species by BC List
sp.catergory <- indata %>%
  group_by(origin, bc_list) %>%
  summarise(count = n())


# generate unique S ranks table (temporary_table)
u_sranks <- indata %>%
  select(srank) %>%
  filter(!is.na(srank)) %>%
  distinct() %>%
  arrange(srank)


if (!dir.exists("data")) dir.create("data")
saveRDS(indata, file.path("data","indata_load.R"))
