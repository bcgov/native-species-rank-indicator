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


# generate plots for taxonomic groups

library(ggplot2)
library(dplyr)

library(envreportutils)


csi.plot <- csi %>%
  group_by(Taxonomic_Group, Year) %>%
  summarize(mean = mean(mean_wt), lci = mean(lci), uci = mean(uci))

csi.plot <- csi.plot %>%
  mutate(Year = as.numeric(Year))



# add plotting parameters

x_scale <- scale_x_continuous(limits = c(1990, max(csi.plot$Year) + 1),
                              breaks = seq(1992, 2018, 4),
                              expand = c(0,0))

normpal <- c("Breeding Birds" = "#e41a1c",
             "Freshwater Fish" = "#377eb8",
             "Mammals" = "#4daf4a",
             "Reptiles & Amphibians" = "#4daf4a")

normpal <- c("#e41a1c", "#377eb8","#4daf4a",  "#4daf4a")


# plot facet plot

ggplot(csi.plot, aes(x = Year, y = mean, group = Taxonomic_Group)) + # same issue here as line below
  facet_wrap(~ Taxonomic_Group) +
  scale_colour_manual(values= normpal) +
  geom_point(aes(y = mean), size = 2) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  theme_soe() +
  x_scale



