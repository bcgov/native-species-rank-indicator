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


library(dplyr)
library(tidyr)
library(ggplot2)

# or read in a local copy temporarily
indata <- readRDS(file.path("data","indata_load.r"))


# filter species to remove (exotics)

indata <- indata %>%
  filter(!origin %in% c("Exotic", "Unknown/Undetermined")) %>%
  filter(!bc_list == "accidental")


# check the number of species by BC List
#sp.catergory <- indata %>%
#  group_by(origin, bc_list) %>%
#  summarise(count = n())


# get a summary of species per taxanomic group

tax_sum <- indata %>%
  group_by(taxonomic_group)%>%
  summarise(across(scientific_name, n_distinct))


# calculate the time points for each group assessment and make a table

yrs <- indata %>%
  filter(!is.na(srank))

ggplot(yrs, aes(year)) +
  geom_bar() +
  facet_wrap(vars(taxonomic_group), scales = "free")


# generate a table
yrs_tax <- indata %>%
  filter(!is.na(srank)) %>%
  group_by(taxonomic_group, year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  left_join(tax_sum) %>%
  filter(count >= scientific_name/2)  # keep years with more than 50% sp ranked (?)


# AT : this is up for discussion?
# note alternate could be to fill in years and ranks up to the last rank date?


# write R objects
saveRDS(indata, file = "data/indata.rds")
saveRDS(yrs_tax, file = "data/yrs_tax.rds")



