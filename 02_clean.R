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


# calculate the time points for each group assessment and make a table

yr.data <- indata %>%
  drop_na(srank)

ggplot(yr.data, aes(year)) +
  geom_bar() +
  facet_wrap(vars(taxonomic_group), scales = "free")


# set dates for invertebrates

invert_yrs <- tribble(
  ~ taxonomic_group, ~ year,
  "Lepidoptera", 1995,
  "Lepidoptera", 1999,
  "Lepidoptera", 2001,
  "Lepidoptera", 2008,
  "Lepidoptera", 2013,
  "Lepidoptera", 2019,
  "Molluscs" , 2004,
  "Molluscs" , 2010,
  "Molluscs" , 2015,
  "Molluscs" , 2019,
  "Odonata" , 2001,
  "Odonata" , 2004,
  "Odonata" , 2015,
  "Odonata" , 2019
)

# generate a table
yrs_tax <- yr.data %>%
  group_by(taxonomic_group, year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  left_join(tax_sum) %>%
  filter(count >= scientific_name/2) %>% # keep years with more than 50% sp ranked (?)
  select(c(taxonomic_group, year)) %>%
  filter(!taxonomic_group %in% c("Lepidoptera","Molluscs","Odonata" )) %>%
  bind_rows(invert_yrs)



# fill missing ranks then filter by years of assessment

sdata <- indata %>%
  group_by(scientific_name) %>%
  fill(srank, .direction = "down") %>%
  inner_join(yrs_tax) %>%
  drop_na(srank)


# check which species are present in all years

sp.to.keep <- sdata %>%
  group_by(taxonomic_group, scientific_name) %>%
  summarise(count = n()) %>%
  mutate(median = median(count), to.keep = count - median) %>%
  filter(to.keep == 0) %>%
  select(scientific_name) %>%
  pull()

  #length(sdata$elcode)
  #5570


# filter the data

sdata <- sdata %>%
  filter(scientific_name %in% sp.to.keep)

  #length(sdata$elcode)
  # 4912



# write R objects

saveRDS(indata, file = "data/indata.rds")

