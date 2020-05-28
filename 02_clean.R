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
  filter(!bc_list == "accidental") %>%
  drop_na(srank)


# check the number of species by BC List
#sp.catergory <- indata %>%
#  group_by(origin, bc_list) %>%
#  summarise(count = n())


# get a summary of species per taxanomic group

tax_sum <- indata %>%
  group_by(taxonomic_group)%>%
  summarise(across(scientific_name, n_distinct))



# set dates for invertebrates

#- lepidoptera : 1995, 1999, 2001, 2008, 2013, 2019
#- Odonata : 2001, 2004, 2015, 2019
#- molluscs: 2004, 2010, 2015, 2019

invert_yrs <- tribble(
  ~ taxonomic_group, ~ year,
  "Lepidoptera", 1995,
  "Lepidoptera", 1999,
  "Lepidoptera", 2001,
  "Lepidoptera", 2008,
  "Lepidoptera", 2013,
  "Lepidoptera", 2019,
  "Molluscs", 2004,
  "Molluscs" , 2010,
  "Molluscs" , 2015,
  "Molluscs" , 2019,
  "Odonata" , 2001,
  "Odonata" , 2004,
  "Odonata" , 2015,
  "Odonata" , 2019
)


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
  filter(count >= scientific_name/2) %>% # keep years with more than 50% sp ranked (?)
  select(c(taxonomic_group, year)) %>%
  filter(!taxonomic_group %in% c("Lepidoptera","Molluscs","Odonata" )) %>%
  bind_rows(invert_yrs)


# filter the taxanomic group by years








# check all species are in all years for analysis.



sp.per.yr <- indata %>%
  group_by(taxonomic_group, year) %>%
  summarise(across(scientific_name, n_distinct))


sp.to.keep <- indata

# remove species that are not represented in all years (unless Sx?)




# write R objects
saveRDS(indata, file = "data/indata.rds")
saveRDS(yrs_tax, file = "data/yrs_tax.rds")



